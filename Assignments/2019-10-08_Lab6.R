# NOTES:
#Chapter 13: #20, 25, 26 & Review Problems 2: #'s 16 by Sunday 13th

#Start with this before each problem
rm(list = ls())

getwd()

library("tidyverse")

tidyverse_update()

install.packages("DescTools")
library("DescTools")

# LAB 6:
# Chapter 13: Problem #20
# a. List two methods that would be appropriate to test whether there was a difference in mean skin color b/w the two groups
# Answer: A two-sample t-test. If graphs are not normal or variences are not equal then I would run Mann-Whitney U-test or
# a Welch's t-test

library(readr)
SalmonColor_data <- read_csv("datasets/abd/chapter13/chap13q20SalmonColor.csv")
View(SalmonColor_data)

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

# All graphs data sets are independent. The histogram and box plot look fairly symmetrical
# Histogram and box plot look normal distributed, but there is one outlier on the sockeye boxplot but whiskers look even and
# median is within the confidence diamond. For the Q-Q plot, The both looks linear and normal 
#I would say to my best knowledge that the graphs are Normal and are not violating any assumption

ratio <-(max(summ_SalmonColor$sd_skinColor))/(min(summ_SalmonColor$sd_skinColor))

# ratio of 4.297, which means variences are not equal (their difference isnt within 3)

# b. Use a transformation

SalmonColor_data<-mutate(SalmonColor_data, log_skinColor = log(skinColor))

summ_SC <- SalmonColor_data %>%
  group_by(species) %>% 
  summarise(mean_log_skinColor = mean(log_skinColor),
            sd_log_skinColor = sd(log_skinColor),
            n_log_skinColor = n())

ggplot(SalmonColor_data) +
  geom_histogram(aes(log_skinColor), binwidth = 0.2)+
  facet_wrap(~species)

ggplot(SalmonColor_data ) +
  geom_boxplot(aes(x = species, y = log_skinColor))

ggplot(SalmonColor_data)+
  geom_qq(aes(sample = log_skinColor, color = species))

# All are independent
# Histogram: looks normally distrtibuted.
# Boxplots: Look normal. There are outliers but whiskers look even and median is within the confidence diamond.
# Q-Q plot: datasets are indepedent and both sockeye and kokanee look pretty linear so I would say it looks normal. 
# Overall, I would say to my best knowledge that the the graphs are Normal and are not violating any assumption.

ratio <-(max(summ_SC$sd_log_skinColor))/(min(summ_SC$sd_log_skinColor))

# ratio of 2.619 (variences are within 3)

t.test(log_skinColor ~ species, data = SalmonColor_data, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

# two sample, two sided t-test = 12.133, df = 33, p-value: <0.0001

# Reject the Null Hypothese. There is a significant skin color difference between the Sockeye and Kokanee.

# Chapter 13: Problem #25

# Test whether there is a change in biomass of rainforest area following the clear-cutting 

library(readr)
Clearcuts_data <- read_csv("datasets/abd/chapter13/chap13q25Clearcuts.csv")
View(Clearcuts_data)

ggplot(Clearcuts_data) +
  geom_histogram(aes(biomassChange), binwidth = 1)

ggplot(Clearcuts_data) +
  geom_boxplot(aes(x = "", y = biomassChange))

ggplot(Clearcuts_data)+
  geom_qq(aes(sample = biomassChange))

# Histogram: Has a slight left skew 
# Boxplot: Has a slight left skew, has two outliers and whiskers are uneven
# Q-Q plot: Has a slight curve
# Assumptions of normality have been violated... I would suggest using a sign test.

SignTest(Clearcuts_data$biomassChange, 
         alternative = "two.sided", mu = 0, conf.level = 0.95)

# Sign Test: S = 21, n = 36, and p-value = 0.405

# There is no significant change in biomass of rainforest area following the clear-cutting 

# Chapter 13: Problem #26

# Choose an appropiate method and test wether females preferred one type of male over the other type.

library(readr)
ZebraFinchBeaks_data <- read_csv("datasets/abd/chapter13/chap13q26ZebraFinchBeaks.csv")
View(ZebraFinchBeaks_data)

ggplot(ZebraFinchBeaks_data) +
  geom_histogram(aes(preference), binwidth = 10)

ggplot(ZebraFinchBeaks_data) +
  geom_boxplot(aes(x = "", y = preference))

ggplot(ZebraFinchBeaks_data)+
  geom_qq(aes(sample = preference))

# Histogram: Hard to tell if there is skew... maybe to the right.
# Boxplot: No outliers, but whiskers are uneven.
# Q-Q plot: slightly linear? It has a weird shape to it tho..
# I would say normality has been violated... I would suggest using a sign test.

SignTest(ZebraFinchBeaks_data$preference, 
         alternative = "two.sided", mu = 0, conf.level = 0.95)

# Sigh test: S = 10, n = 10, and p-value = 0.001953

# Females preferred the carotenoid-enhanced males 

# Review Problems 2: #16

library(readr)
FishBoldness_data <- read_csv("datasets/abd/chapter03/chap03q22ZebraFishBoldness.csv")
View(FishBoldness_data)


# a. estimate the magnatitude of the effect of the mutation (the difference b/w the means) on the amount of time
# spent in aggressive activity. Put appropriate bound on your estimate if the effect.

summ_FB <- FishBoldness_data %>%
  group_by(genotype) %>% 
  summarise(mean_secondsAggressiveActivity = mean(secondsAggressiveActivity),
            sd_secondsAggressiveActivity = sd(secondsAggressiveActivity),
            n_secondsAggressiveActivity = n())

diff_mean <- 	142.1 - 74.0

# difference in mean = 68.1

# The effect of the mutation occured on average 68.1 more time in aggressive activity for the spd fish


t.test(secondsAggressiveActivity ~ genotype, data = FishBoldness_data,
       var.equal = TRUE,
       alternative = "two.sided",
       conf.level = 0.95)

# 95% CI is 25.9 < mu1 - mu2 < 110.3 
# The appropriate bound of the effect of mutation is between 25 and 110 second.

# b. What is the weight of evidence that this effect is not zero? Perform the appropiate statistical test of the difference.

ggplot(FishBoldness_data) +
  geom_histogram(aes(secondsAggressiveActivity), binwidth = 25)+
  facet_wrap(~genotype)

ggplot(FishBoldness_data) +
  geom_boxplot(aes(x = genotype, y = secondsAggressiveActivity))

ggplot(FishBoldness_data)+
  geom_qq(aes(sample = secondsAggressiveActivity, color = genotype))

# Histogram: n is less then 15 so not a good show of normality
# Boxplot: Whiskers in both also wild types median is more closely to the 3rd quad.
# Q-Q plot: both are somewhat linear, but it has an interesting S shape to both
# I would say the graphs are not normal.

ratio <-(max(summ_FB$sd_secondsAggressiveActivity))/(min(summ_FB$sd_secondsAggressiveActivity))

# Ratio is 1.39 so variances are equal (within 3)

# Since graphs are not normal but variances are equal... I would run a Mann-Whitney U Test

wilcox.test(secondsAggressiveActivity ~ genotype, data = FishBoldness_data, alternative = "two.sided", conf.level = 0.95)

# Mann-Whitney U Test: W = 90 and p-value = 0.01272

# The effect is not zero

#### 24/24 code runs without breaking ####
