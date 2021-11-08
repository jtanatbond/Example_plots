## Loading libraries and start code ----

library(tidyverse)

## Read .csv file
library(readr)
nhmrc <- read_csv("../data/NHMRC_scoring_history2.csv", 
                  col_types = cols(Score = col_number()))
View(nhmrc)

## Create grouped (dodged) bar graph ----
## We have 2 categorical variables (Year, Criteria) and one numeric variable (Score)
## One categorical variable will be plotted on the x-axis (x=Category)
## The other categorical variable will be displayed using the "fill" argument (fill=Year))

nhmrc  %>% 
  filter(Year %in% c("2020", "2021")) %>%
  filter(Criteria %in% c("RQ", "IC", "S", "C", "OS")) %>%
  ggplot(aes(x=Criteria, y=Score, fill=Year)) +
  scale_x_discrete(limits = c("RQ", "IC", "S", "C", "OS")) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 7)) +
  theme(legend.position="top") +
  annotate(geom = "segment", x = 0.4, xend = 5.5, y = 5.59, yend = 5.59, colour = "#060606", linetype=2) +
  annotate("text", x = c(2.85), y = c(5.9), 
           label = "2021 funding cutoff = 5.56" , color="black", 
           size=5, angle=0) +
  ylab("Score")


nhmrc  %>% 
  filter(Year %in% c("2020", "2021", "(2021_cutoff)")) %>%
  filter(Criteria %in% c("OS")) %>%
  ggplot( aes(x=Criteria, y=Score, fill=Year)) +
  scale_x_discrete(limits = c("OS")) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 7)) +
  theme(legend.position="top") +
  annotate(geom = "segment", x = 0.4, xend = 1.6, y = 5.59, yend = 5.59, colour = "#060606", linetype=2) 
  ylab("Score")

