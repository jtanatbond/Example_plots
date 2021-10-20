#### Loading libraries and start code ####

library(tidyverse)
library(pzfx)
# library(ggpubr)

## https://yue-jiang.github.io/pzfx/
## https://cran.r-project.org/web/packages/pzfx/vignettes/pzfx.html#:~:text=GraphPad%20Prism%20is%20a%20software,into%20R%20and%20vise%20versa.

## List tables in .pzfx file
pzfx_tables("/run/media/jon/FILES/Work/Desktop items/Git/jkthesis/young vs old graphs JTmod.pzfx")

## Read a specific table from a .pzfx file
Fig423b <- read_pzfx("/run/media/jon/FILES/Work/Desktop items/Git/jkthesis/young vs old graphs JTmod.pzfx", "Figure 4.2.3B")

## View dataframe
view(Fig423b)
# str(Fig423b)

## Convert data to "Tall format" using the pivot_longer function
## Ie. list all the observations (values) in one column
## https://datascience.stackexchange.com/questions/66590/how-to-plot-multiple-columns-with-ggplot-in-r
Fig423b.tall <- Fig423b %>% select(Young, Old, "Old asplenic") %>%
  pivot_longer(., cols = c(Young,Old,"Old asplenic"), names_to = "Age", values_to = "HSC") ## Concatenate columns (Young, Old, Old Asplenic) into one column (age)
View (tall)

## Create a plot
Fig423b.tall %>%
  ggplot(aes(Age,HSC)) +
  ## Set the order of categories on the x-axis
  ## https://r-graphics.org/recipe-axis-order
  scale_x_discrete(limits = c("Young", "Old", "Old asplenic")) +
  ## Set the y-axis origin to 0 and define range
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  geom_boxplot() +
  geom_point(alpha = 0.7,
             aes(size = 3,
                 colour = "33fff3"))

## Run descriptive statistics (find the mean %HSC for all treatment groups)
Fig423b.tall %>% 
  select(Age, HSC) %>%
  filter(Age == "Young"|
           Age == "Old"|
           Age == "Old asplenic") %>% 
  na.omit() %>% # Omit values with 'NA'
  group_by(Age) %>% 
  summarise(Average = mean(HSC))

## To perform a unpaired two-sample t-test, create dataframe pairs for comparison
Fig423b.youngvsold <- Fig423b.tall %>% 
  select(Age, HSC) %>%
  filter(Age == "Young"|
           Age == "Old") %>% 
  na.omit()
View(Fig423b.youngvsold)

Fig423b.oldvsoldasplenic <- Fig423b.tall %>% 
  select(Age, HSC) %>%
  filter(Age == "Old"|
           Age == "Old asplenic") %>% 
  na.omit()
View(Fig423b.oldvsoldasplenic)

## Unpaired two-samples t-test can be used only under certain conditions:
## 1) When the two groups of samples (A and B), being compared, are normally distributed. This can be checked using Shapiro-Wilk test.
## 2) When the variances of the two groups are equal. This can be checked using F-test.
## http://www.sthda.com/english/wiki/unpaired-two-samples-t-test-in-r


## Use Shapiro-Wilk normality test
## Null hypothesis: the data are normally distributed
## Alternative hypothesis: the data are not normally distributed

## Shapiro-Wilk normality test for Young %HSC
with(Fig423b.tall, shapiro.test(HSC[Age == "Young"])) # p = 0.2301
## Shapiro-Wilk normality test for Old %HSC
with(Fig423b.tall, shapiro.test(HSC[Age == "Old"])) # p = 0.958
## Shapiro-Wilk normality test for Old asplenic %HSC
with(Fig423b.tall, shapiro.test(HSC[Age == "Old asplenic"])) # p = 0.7321
## From the outputs, the p-values are greater than the significance level 0.05
## Implies that the distribution of the data are not significantly different from the normal distribution.
## In other words, we can assume the normality.


## Use F-test to test for homogeneity in variances
var.test(HSC ~ Age, data = Fig423b.youngvsold) # p-value = 0.1663
var.test(HSC ~ Age, data = Fig423b.oldvsoldasplenic) # p-value = 0.3238
## The p-values of F-test are greater than the significance level 0.05
## Implies that there is no significant difference between the variances of the two sets of data.
## Therefore, we can use the classic t-test witch assume equality of the two variances.


## If the variance of the two groups are equivalent (homoscedasticity; var.equal = TRUE), the student t-test value can be used
## If the variance of the two groups are different (heteroscedasticity; var.equal = FALSE), it’s possible to use the Welch t test, an adaptation of Student t-test

## By default, R computes the Welch t-test
## Welch t-test with default arguments
t.test(data = Fig423b.youngvsold, HSC ~ Age) ## p-value = 0.01904, reject null hypothesis (that there is no difference between groups)
t.test(data = Fig423b.oldvsoldasplenic, HSC ~ Age) ## p-value = 0.3136, cannot reject null hypothesis

## Assume homoscedasticity
t.test(data = Fig423b.youngvsold, HSC ~ Age, var.equal = TRUE) ## p-value = 0.009666
t.test(data = Fig423b.oldvsoldasplenic, HSC ~ Age, var.equal = TRUE) ## p-value = 0.2449

## Use one-tailed test under the following hypothesis:
## H0:mA≤mB, ie. Null hypothesis: HSC(old) is less than or equal to HSC(young[control])
## Ha:mA>mB (greater), ie. Alternative hypothesis: HSC(old) is greater than HSC(young)
t.test(data = Fig423b.youngvsold, HSC ~ Age, var.equal = TRUE, alternative = "greater") ## p-value = 0.004833
## If the observed difference went in the direction predicted by the experimental hypothesis,
## the one-tailed P value is half the two-tailed P value (with most, but not quite all, statistical tests).
t.test(data = Fig423b.oldvsoldasplenic, HSC ~ Age, var.equal = TRUE, alternative = "greater") ## p-value = 0.1224



#### Default: Bar charts with individual points and error bars ####

## Compute summary statistics for the variable "HSC" organized into groups by the variable "Age":
## https://www.datanovia.com/en/lessons/ggplot-error-bars/
## For standard error:
## https://www.r-graph-gallery.com/4-barplot-with-error-bar.html
Fig423b.tallna <- Fig423b.tall %>% na.omit() # Omit values in the dataframe with NA
Fig423b.tallna.summary <- Fig423b.tallna %>%
  group_by(Age) %>%
  summarise(
    n=n(),
    sd = sd(HSC, na.rm = TRUE), ## Handling missing data: If there are NA’s in the data, you need to pass the flag na.rm=TRUE
    HSC = mean(HSC)
  ) %>%
  mutate( se=sd/sqrt(n))
Fig423b.tallna.summary




#### Boxplots (working) ####

## Create annotated dataframe/plot
Fig423b.tall.annotated <- Fig423b.tall %>%
  ggplot(aes(Age,HSC)) +
  ## Set the order of categories on the x-axis
  ## https://r-graphics.org/recipe-axis-order
  scale_x_discrete(limits = c("Young", "Old", "Old asplenic")) +
  ## Set the y-axis origin to 0 and define range
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  geom_boxplot() +
  geom_point(alpha = 0.7,
             aes(size = 3,
                 colour = "33fff3"))+
  theme_bw()+
  labs(x = "",
       y = "%CD150^hi LT-HSC",
       title = "Fig. 4.2B: %myeloid-biased BM-HSC")

## View plot for "annotated"
Fig423b.tall %>%
  ggplot(aes(Age,HSC)) +
  ## Set the order of categories on the x-axis
  ## https://r-graphics.org/recipe-axis-order
  scale_x_discrete(limits = c("Young", "Old", "Old asplenic")) +
  ## Set the y-axis origin to 0 and define range
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  geom_boxplot() +
  geom_point(alpha = 0.7,
             aes(size = 3,
                 colour = "33fff3"))+
  theme_bw()+
  labs(x = "",
       y = "%CD150^hi LT-HSC",
       title = "Fig. 4.2B: %myeloid-biased BM-HSC")


## Add annotations
Fig423b.tall.annotated +
  annotate("text", x = c(1.1,2.5), y = c(80,90), 
           label = c("p=0.005", "p=0.1224") , color="orange", 
           size=5 , angle=0, fontface="italic") +
  annotate(geom = "segment", x = c(0.8,2.1), xend = c(1.8,3.1), y = c(77,87), yend = c(77,87), colour = "#060606") +
  guides(color = "none", size = "none") ## Remove legend



