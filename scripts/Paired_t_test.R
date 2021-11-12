library(tidyverse)
library(ggpubr)

# http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/#compare-two-paired-samples
# http://www.sthda.com/english/wiki/paired-samples-t-test-in-r



# Enter data in two numeric vectors
# ++++++++++++++++++++++++++
# %CD150^hi HSC amongst LT-HSC
young <-c(30, 38.9, 31.7, 2.38, 8.11, 33.7, 28.7)
old <-c(50.7, 78.6, 60, 36.9, 24.3, 36.9, 58.2)

# Create a data frame
expt <- data.frame( 
  group = rep(c("young", "old"), each = 7),
  HSC = c(young,  old)
)

print(expt)

# Perform descriptive statistics
group_by(expt, group) %>%
  summarise(
    count = n(),
    mean = mean(HSC, na.rm = TRUE),
    sd = sd(HSC, na.rm = TRUE)
  )

# Compute one-tailed paired students t-test
stats <- t.test(HSC ~ group, data = expt, paired = TRUE, var.equal = TRUE, alternative = "greater")
stats

# Create paired box plot
# Plot %HSC by group (young vs old) and color by group
ggpaired(expt, x = "group", y = "HSC",
         color = "group", line.color = "gray", line.size = 0.4,
         palette = "jco") +
         scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
         theme_bw() +
         theme(legend.position="none") +
         labs(x = "",
               y = "%CD150^hi LT-HSC",
               title = "Paired t-test, one-tailed") +
          annotate("text", x = c(1.5), y = c(89), 
                   label = c("p=0.0009") , color="black", 
                   size=5 , angle=0, fontface="italic") +
          annotate(geom = "segment", x = c(1), xend = c(2), y = c(85), yend = c(85), colour = "#060606") 

