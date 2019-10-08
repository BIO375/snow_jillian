# NOTES:
#Chapter 13: #20, 25, 26 & Review Problems 2: #'s 16 by Sunday 13th

#Start with this before each problem
rm(list = ls())

getwd()

library("tidyverse")

tidyverse_update()

# LAB 6:
# Chapter 13: Problem #20
# a. List two methods that would be appropriate to test whether there was a difference in mean skin color b/w the two groups
# Answer: You can do a paired t-test or a two-sample t-test
library(readr)
SalmonColor_data <- read_csv("datasets/abd/chapter13/chap13q20SalmonColor.csv")
View(chap13q20SalmonColor)

summ_SalmonColor <- SalmonColor_data %>%
  group_by(species) %>% 
  summarise(mean_skinColor = mean(skinColor),
            sd_skinColor = sd(skinColor),
            n_skinColor = n())

ggplot(SalmonColor_data) +
  geom_histogram(aes(skinColor), binwidth = 0.2)+
  facet_wrap(~species)

ggplot(SalmonColor_data ) +
  geom_boxplot(aes(x = species, y = skinColor))

ggplot(SalmonColor_data)+
  geom_qq(aes(sample = skinColor, color = species))

# Normality??

ratio <-(max(summ_SalmonColor$sd_skinColor))/(min(summ_SalmonColor$sd_skinColor))

# ratio of 4.297, which means variences are not equal (their difference isnt within 3)

# b. Use a transformation

SalmonColor_data<-mutate(SalmonColor_data, log_skinColor = log(skinColor))

summ_SC <- SalmonColor_data %>%
  group_by(species) %>% 
  summarise(mean_log_skinColor = mean(log_skinColor),
            sd_log_skinColor = sd(log_skinColor),
            n_log_skinColor = n())

ggplot(ward) +
  geom_histogram(aes(EGGS), binwidth = 2)+
  facet_wrap(~ZONE)

ggplot(ward) +
  geom_boxplot(aes(x = ZONE, y = EGGS))

ggplot(ward)+
  geom_qq(aes(sample = EGGS, color = ZONE))

ratio <-(max(summ_SC$sd_log_skinColor))/(min(summ_SC$sd_log_skinColor))

# ratio of 2.619 (variences are within 3)

t.test(log_skinColor ~ species, data = SalmonColor_data, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

# t = 12.133, df = 33, p-value: 0.001

# Reject the Null Hypothese. There is a significant skin color difference bettween the Sockeye and Kokanee.




