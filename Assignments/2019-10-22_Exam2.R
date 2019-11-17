#Start with this before each problem:
rm(list = ls())

getwd()

library("tidyverse")

tidyverse_update()

install.packages("DescTools")
library("DescTools")

#Problem 9####

library(readr)
feathers <- read_csv("datasets/exams/feathers.csv")
View(feathers)

untidy_feathers <- mutate(feathers, diff = typical - odd)

ggplot(untidy_feathers) +
  geom_histogram(aes(diff), binwidth = .07)

ggplot(untidy_feathers) +
  geom_boxplot(aes(x = "", y = diff))

ggplot(untidy_feathers)+
  geom_qq(aes(sample = diff))

summ_untidy_feathers <- untidy_feathers %>%
  group_by(diff) %>% 
  summarise(mean_diff = mean(diff),
            sd_diff = sd(diff()),
            n_diff = n())

t.test(untidy_feathers$typical, untidy_feathers$odd, 
       alternative = "less", paired =  TRUE, conf.level = 0.95)

#Problem 10####

library(readr)
baker <- read_csv("datasets/exams/baker.csv")
View(baker)

untidy_baker <- mutate(baker, diff = After - Before)

ggplot(untidy_baker) +
  geom_histogram(aes(diff), binwidth = 2)

ggplot(untidy_baker) +
  geom_boxplot(aes(x = "", y = diff))

ggplot(untidy_baker)+
  geom_qq(aes(sample = diff))

SignTest(untidy_baker$diff, alternative = "less", mu = 0, conf.level = 0.95)

SignTest(untidy_baker$diff, alternative = "greater", mu = 0, conf.level = 0.95)

#Problem 11####

library(readr)
CO2 <- read_csv("datasets/exams/CO2.csv")
View(CO2)

summ_CO2 <- CO2 %>%
  group_by(Treatments) %>% 
  summarise(mean_growthrate = mean(growthrate),
            sd_growthrate = sd(growthrate),
            n_growthrate = n())

ratio <-(max(summ_CO2$sd_growthrate))/(min(summ_CO2$sd_growthrate))

# Ratio is 1.126: variences are equal (within 3)

ggplot(CO2) +
  geom_histogram(aes(growthrate), binwidth = .5)+
  facet_wrap(~Treatments)

ggplot(CO2) +
  geom_boxplot(aes(x = Treatments, y = growthrate))

ggplot(CO2)+
  geom_qq(aes(sample = growthrate, color = Treatments))

t.test(growthrate ~ Treatments, data = CO2, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)


#### CODE RUNS WITHOUT BREAKING 6/6 PTS, GOOD JOB ####