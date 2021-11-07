#### Loading libraries and start code ####

library(tidyverse)
library(pzfx)
# library(ggpubr)

## https://yue-jiang.github.io/pzfx/
## https://cran.r-project.org/web/packages/pzfx/vignettes/pzfx.html#:~:text=GraphPad%20Prism%20is%20a%20software,into%20R%20and%20vise%20versa.

## List tables in .pzfx file
pzfx_tables("data/young vs old graphs JTmod.pzfx")

## Read a specific table from a .pzfx file
data1 <- read_pzfx("data/young vs old graphs JTmod.pzfx", "Data 1")

## View dataframe
view(data1)
# str(data1)

## Convert data to "Tall format" using the pivot_longer function
## Ie. list all the observations (values) in one column
## https://datascience.stackexchange.com/questions/66590/how-to-plot-multiple-columns-with-ggplot-in-r
data1.tall <- data1 %>% select(Young, Old, "Old Asplenic") %>%
  pivot_longer(., cols = c(Young,Old,"Old Asplenic"), names_to = "Age", values_to = "HSC") ## Concatenate columns (Young, Old, Old Asplenic) into one column (age)
View (data1.tall)



#### Statistics ####


## Run descriptive statistics (find the mean %HSC for all treatment groups)
data1.tall %>% 
  select(Age, HSC) %>%
  filter(Age == "Young"|
           Age == "Old"|
           Age == "Old Asplenic") %>% 
  na.omit() %>% # Omit values with 'NA'
  group_by(Age) %>% 
  summarise(Average = mean(HSC))

## To perform a unpaired two-sample t-test, create dataframe pairs for comparison
data1.youngvsold <- data1.tall %>% 
  select(Age, HSC) %>%
  filter(Age == "Young"|
           Age == "Old") %>% 
  na.omit()
View(data1.youngvsold)

data1.oldvsoldasplenic <- data1.tall %>% 
  select(Age, HSC) %>%
  filter(Age == "Old"|
           Age == "Old Asplenic") %>% 
  na.omit()
View(data1.oldvsoldasplenic)

## Unpaired two-samples t-test can be used only under certain conditions:
## 1) When the two groups of samples (A and B), being compared, are normally distributed. This can be checked using Shapiro-Wilk test.
## 2) When the variances of the two groups are equal. This can be checked using F-test.
## http://www.sthda.com/english/wiki/unpaired-two-samples-t-test-in-r


## Use Shapiro-Wilk normality test
## Null hypothesis: the data are normally distributed
## Alternative hypothesis: the data are not normally distributed

## Shapiro-Wilk normality test for Young %HSC
with(data1.tall, shapiro.test(HSC[Age == "Young"])) # p = 0.09802
## Shapiro-Wilk normality test for Old %HSC
with(data1.tall, shapiro.test(HSC[Age == "Old"])) # p = 0.6012
## Shapiro-Wilk normality test for Old asplenic %HSC
with(data1.tall, shapiro.test(HSC[Age == "Old Asplenic"])) # p = 0.1929
## From the outputs, the p-values are greater than the significance level 0.05
## Implies that the distribution of the data are not significantly different from the normal distribution.
## In other words, we can assume the normality.


## Use F-test to test for homogeneity in variances
var.test(HSC ~ Age, data = data1.youngvsold) # p-value = 0.8145
var.test(HSC ~ Age, data = data1.oldvsoldasplenic) # p-value = 0.1456
## The p-values of F-test are greater than the significance level 0.05
## Implies that there is no significant difference between the variances of the two sets of data.
## Therefore, we can use the classic t-test witch assume equality of the two variances.


## If the variance of the two groups are equivalent (homoscedasticity; var.equal = TRUE), the student t-test value can be used
## If the variance of the two groups are different (heteroscedasticity; var.equal = FALSE), it’s possible to use the Welch t test, an adaptation of Student t-test


## Use one-tailed test under the following hypothesis:
## H0:mA≤mB, ie. Null hypothesis: HSC(old) is less than or equal to HSC(young[control])
## Ha:mA>mB (greater), ie. Alternative hypothesis: HSC(old) is greater than HSC(young)
t.test(data = data1.youngvsold, HSC ~ Age, var.equal = TRUE, alternative = "greater") ## p-value = 0.005258
## If the observed difference went in the direction predicted by the experimental hypothesis,
## the one-tailed P value is half the two-tailed P value (with most, but not quite all, statistical tests).
t.test(data = data1.oldvsoldasplenic, HSC ~ Age, var.equal = TRUE, alternative = "greater") ## p-value = 0.7297



#### Dot plots with individual points and error bars ####

## Compute summary statistics for the variable "HSC" organized into groups by the variable "Age":
## https://www.datanovia.com/en/lessons/ggplot-error-bars/
## For standard error:
## https://www.r-graph-gallery.com/4-barplot-with-error-bar.html
data1.tallna <- data1.tall %>% na.omit() # Omit values in the dataframe with NA
view(data1.tallna)
data1.tallna.summary <- data1.tallna %>%
  group_by(Age) %>%
  summarise(
    n=n(),
    sd = sd(HSC, na.rm = TRUE), ## Handling missing data: If there are NA’s in the data, you need to pass the flag na.rm=TRUE
    HSC = mean(HSC)
  ) %>%
  mutate( se=sd/sqrt(n))
data1.tallna.summary


## Create dot plot with means + standard errors
## http://www.sthda.com/english/wiki/ggplot2-dot-plot-quick-start-guide-r-software-and-data-visualization
## https://rstudio-pubs-static.s3.amazonaws.com/1406_947a49f2d7914dad8b0fd050a9df9858.html
data1.tallna %>%
  ggplot(aes(Age,HSC, shape=Age)) +
  scale_x_discrete(limits = c("Young", "Old", "Old Asplenic")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 8000)) +
  geom_errorbar( aes(ymin = HSC-se, ymax = HSC+se), 
                 data = data1.tallna.summary, width = 0.5) +
  stat_summary(fun=mean, geom="crossbar",
               size=0.3, color="black") +
  geom_dotplot(binaxis='y', stackdir='center',
               stackratio=1.5, dotsize=0.5) +
  theme(legend.position="none") + 
  theme_bw()+
  labs(x = "",
       y = "MFI",
       title = "Fig. 4.2.3C: Median flourescence intensity of BM CD150^hi LT-HSC")



## Create annotated dataframe/plot
data1.tallna.annotated <- data1.tallna %>%
  ggplot(aes(Age,HSC, shape=Age)) +
  scale_x_discrete(limits = c("Young", "Old", "Old Asplenic")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 8000)) +
  geom_errorbar( aes(ymin = HSC-se, ymax = HSC+se), 
                 data = data1.tallna.summary, width = 0.5) +
  stat_summary(fun=mean, geom="crossbar",
               size=0.3, color="black") +
  geom_dotplot(binaxis='y', stackdir='center',
               stackratio=1.5, dotsize=0.5) +
  theme(legend.position="none") + 
  theme_bw()+
  labs(x = "",
       y = "MFI",
       title = "Fig. 4.2.3C: Median flourescence intensity of BM CD150^hi LT-HSC")


## Add annotations
data1.tallna.annotated +
  annotate("text", x = c(1.1,2.5), y = c(5000,7000), 
           label = c("p=0.005", "p=0.7297") , color="black", 
           size=5 , angle=0, fontface="italic") +
  annotate(geom = "segment", x = c(0.8,2.1), xend = c(1.8,3.1), y = c(4700,6700), yend = c(4700,6700), colour = "#060606") 


