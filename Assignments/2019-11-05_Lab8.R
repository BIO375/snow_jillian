# Clean up the working environment
rm(list = ls())

# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# ggfortify is a package that works with ggplot2 to make nice plots
install.packages("ggfortify")
library("ggfortify")

# multcomp is used for contrasts and multiple comparisons
install.packages("multcomp")
library("multcomp")

# nlme is used for random effects ANOVA
install.packages("nlme")
library("nlme")

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

#### Chapter 15: Problem 22 ####
# Complete parts a, b, c, d

# A) Use ANOVA calculations to estimate the varience within groups for head-width.
# B) Calculate the estimate of the variance among groups.

library(readr)
WSHead_data <- read_csv("datasets/abd/chapter15/chap15q22WalkingStickHeads.csv", col_types = cols(
  specimen = col_factor() ))
View(WSHead_data)

head(WSHead_data)
summary(WSHead_data)

# Boxplot and Histogram:
ggplot(WSHead_data, aes(x = "", y = headwidth))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()

ggplot(WSHead_data) +
  geom_histogram(aes(headwidth), binwidth = .01)

# Model:
model01 <- lm(headwidth~specimen, data = WSHead_data)

#Summary Statistics:
summ_Headwidth <- WSHead_data %>%
  group_by(specimen) %>% 
  summarise(mean_headwidth = mean(headwidth),
            sd_headwidth = sd(headwidth),
            n_headwidth = n())

# Ratio: 
ratio <-(max(summ_Headwidth$sd_headwidth))/(min(summ_Headwidth$sd_headwidth))

#Random effects ANOVA:
model02 <- lme(fixed = headwidth ~ 1,
               random = ~1|specimen, data = WSHead_data)

model02_varcomp <- VarCorr(model02)
model02_varcomp

# varience within groups for head-width is 0.000166.
# variance among groups for head-width is 0.0002459167.

# C) What is the repeatablility of the head-width measurments?

varAmong  <- as.numeric( model02_varcomp[1,1] )

varWithin <- as.numeric( model02_varcomp[2,1] )

repeatability <- varAmong / (varAmong + varWithin)
repeatability

# The repeatability of the head-width measurements is 0.5970058994

# D) Compare your results in part (c) with that for femur length analyze in
# Example 15.6 Which trait has higher repeatability? which trait is more 
# afected by measurement error

# For example 15.6, the repeatability is 0.75, so the repeatability is higher
# in example 15.6. Since Head-width has lower repeatablility than femur length,
# that means head-width will be more affected by measurement error.

#### Chapter 15: Problem 23 ####
# Complete part a 
# A) What do we label this type of comparison? (identify and execute that test!)

# You would label this type of comparison as planned comparison.

library(readr)
LPC_data <- read_csv("datasets/abd/chapter15/chap15q23LodgepolePineCones.csv", col_types = cols(
  habitat = col_factor() ))
View(LPC_data)

head(LPC_data)
summary(LPC_data)

# Boxplot, Q-Q plot and Histogram:
ggplot(LPC_data, aes(x = habitat, y = conemass))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()

ggplot(LPC_data) +
  geom_histogram(aes(conemass), binwidth = .4)

ggplot(LPC_data)+
  geom_qq(aes(sample = conemass, color = habitat))

# Model:
model01 <- lm(conemass~habitat, data = LPC_data)

#Summary Statistics:
summ_LPC <- LPC_data %>%
  group_by(habitat) %>% 
  summarise(mean_conemass = mean(conemass),
            sd_conemass = sd(conemass),
            n_conemass = n())

# Ratio: 
ratio <-(max(summ_LPC$sd_conemass))/(min(summ_LPC$sd_conemass))

#Ratio = 1.33544379823669

#Planned Comparison:
planned <- glht(model01, linfct = 
                  mcp(habitat = c("island.present - island.absent = 0")))
confint(planned)
summary(planned)

# The planned comparison shows that the mean conemass of island with squirel
# had a significantly lower mean conemass compared to the island without
# squirels.
# (Planned comparison: t-value: -8.596, p<0.0001)

#### Chapter 15: Problem 26 ####
# Use the data to perform the correct test.  
# Please show code for all steps in your process.

library(readr)
MFV_data <-read_csv("datasets/abd/chapter15/chap15q26MalariaFungusVenom.csv", col_types = cols(
  treatmentGroup = col_factor() ))
View(MFV_data)

# A) Show the data in a graph. What pattern is suggest?

# Box Plot, Histogram & Q-Q plot:
ggplot(MFV_data, aes(x = treatmentGroup, y = logSporozoiteNumbers))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()

ggplot(MFV_data) +
  geom_histogram(aes(logSporozoiteNumbers), binwidth = 1)+
  facet_wrap(~treatmentGroup)

ggplot(MFV_data)+
  geom_qq(aes(sample = logSporozoiteNumbers, color = treatmentGroup))

# WT: Boxplot - Whiskers are uneven (left whisker is longer than the left), 
# mean and median are not close, but still within quartile box.
#  Histogram - looks slightly left skewed. q-q plot - has a little squiggle 
# shape but somewhat linear
# Scorpine: Boxplot - whiskers look relatively even, has two outliers, and
# Mean and median are not very close but within the quartile box. Histogram -
# Looks relatively normally distributed. Q-q plot - has a curve due the outler
# due to point at 0.0 on the y-axis
# Control: Boxplot - Right whisker looks a little longer than the left, no
# outliers, Mean and median are not super close but within quartile box.
# Histogram - looks normally distributed. Q-q plot - also slight squiggle
# shape, but pretty linear.
# Conclusion: I would conclude that these graphs do not pass assumtions 
# of normality

#Summary Statistics:
summ_MFV <- MFV_data %>%
  group_by(treatmentGroup) %>% 
  summarise(mean_logSporozoiteNumbers = mean(logSporozoiteNumbers),
            sd_logSporozoiteNumbers = sd(logSporozoiteNumbers),
            n_logSporozoiteNumbers = n())

#Ratio:
ratio <-(max(summ_MFV$sd_logSporozoiteNumbers))/(min(summ_MFV$sd_logSporozoiteNumbers))

# Variances are equal/within 3. Ratio: 2.885

# B) Examine the frequency distribution of the data. What approuch would be
# the most appropiate to determine whether these treatments vary in their 
# number of sporozoites? Why?

# Due to the fact that this data (After log) does not meet assumptions of
# normality, but does have equal variance. I would suggest using a Tukey test.

model01 <- lm(logSporozoiteNumbers~treatmentGroup, data = MFV_data)

tukey <- glht(model01, linfct = mcp(treatmentGroup = "Tukey"))
summary(tukey)

# There is a significantly vary b/w the scorpine and the control group.
# (Tukey: t-value: -6.039, p<0.001)
# There was also a significant vary b/w the scorpine and the WT group.
# (Tukey: t-value: -5.346, p<0.001)

#### Chapter 15: Problem 30 ####
# Use the data to perform the correct test.  Please show code for all steps in
# your process.

library(readr)
Crab_data <- read_csv("datasets/abd/chapter15/chap15q30FiddlerCrabFans.csv", col_types = cols(
  crabType = col_factor() )) 

View(Crab_data)

Crab_data <- slice(Crab_data,-85)

# A) Show these data in a graph. What trends are suggested?

# Box Plot, Histogram & Q-Q plot
ggplot(Crab_data, aes(x = crabType, y = bodyTemperature))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()

ggplot(Crab_data) +
  geom_histogram(aes(bodyTemperature), binwidth = .1)+
  facet_wrap(~crabType)

ggplot(Crab_data)+
  geom_qq(aes(sample = bodyTemperature, color = crabType))

# Male minor removed: Boxplot - left whisker is longer than right, Mean and 
# median are close and no outliers. Histogram - looks normally distributed
# (maybe a slight skew)
# Male major removed: Boxplot - right whisker is longer than left, there are
# two outliers, and mean and median are relatively close. Histogram - looks
# normally distibuted.
# Intact male: box plot - left whisker is longer than right whisker, one outlier,
# and mean and median are relatively close. histogram - look normaly distributed
# Female: box plot - Whiskers look pretty even (right might be slightly longer
# than left), has one outlier, and  mean and median are relatively close.
# Histogram - looks normally distributed
# Conclusion: I would conclude that these graphs do pass assumptions of normality

#Summary Statistics:
summ_Crab <- Crab_data %>%
  group_by(crabType) %>% 
  summarise(mean_bodyTemperature = mean(bodyTemperature),
            sd_bodyTemperature = sd(bodyTemperature),
            n_bodyTemperature = n())
#Ratio:
ratio <-(max(summ_Crab$sd_bodyTemperature))/(min(summ_Crab$sd_bodyTemperature))

# Variances are equal/Within 3. Ratio = 1.17884579956101

# Model:
model01 <- lm(bodyTemperature~crabType, data = Crab_data)

# Autoplot:
autoplot(model01)

# B) Use ANOVA to test whether mean rate of heat gain differs among groups.

anova(model01)

# The mean rate of gain significantly differs among groups (Reject Null).
# (Anova: F-value: 20.312, P<0.001)

#### Chapter 15: Problem 31 (same data in both problems) ####
# Use the data to perform the correct test.  Please show code for all steps in
# your process.

# A) The main comparison of interest, which was identified before carring out
# the experiment, was to test for a difference b/w the two male groups "Major
# removed" and "Minor removed." What test method is justified in this case?

# I would do a planned comparison b/w "major removed" and "minor removed"

planned <- glht(model01, linfct = 
                  mcp(crabType = c("major removed - minor removed = 0")))
confint(planned)
summary(planned)


# B) The table at the bottom of the page shows partial results of Tukey-Kramer
# multiple comparisons of means. In what way does this method differ from the
# method identified in part (a)

# The method identified in part (a) was a planned comparison, while a
# Tukey-Kramer is an unplanned comparison. A planned comparison is a 
# comparison planned before the study. As stated in (a),
# They planned to test for a difference b/w the two male groups before the
# experiment was carried out. As for the Tukey-Kramer (unplanned comparison),
# it is a method that is carried out after ANOVA and the results showed no
# diffence among means (rejected the null).

# C) Complete the Table by adding the test conclusion (Table results partial
# results of Tukey-Kramer).

tukey <- glht(model01, linfct = mcp(crabType = "Tukey"))
summary(tukey)

# There was a significant diffence b/w intact males and females
# (Tukey: t-value: -6.077, P<0.001)

# There was a significant difference b/w male major removed and females
# (Tukey: t-value: -7.263, P<0.001)

# There was a significant difference b/w male major removed and females
# (Tukey: t-value: -4.076, P<0.001)

# There was a significant differnce b/w male major removed and male minor
# removed.
# (Tukey: t-value:  3.187, P<0.05)

# There was no significant differnce b/w male minor removed and intact male
# (Tukey: t-value: -1.186, P>0.05)

# There was no significant differnce b/w male major removed and intact male
# (Tukey: t-value:  2.001, P>0.05)

# D) Use symbols to illistrate the results of the Tukey-Kramer test. Decribe
# in words which Population means are grouped together based on statistic 
# significance.

# There was statistical significant diffence b/w the female group and all the
# males groups (intact, major removed and minor removed). There was also 
# a statistical significant difference b/w male major removed and the male 
# minor removed group.




