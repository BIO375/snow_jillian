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
# Example 15.6 Which trait has higher repeatability? which trait is more affected
# By measurement error

# For example 15.6 the repeatability is 0.75, so the repeatability is higher in
# example 15.6. Since Head-width has lower repeatablility than femur length,
# that means head-width will be more affected by measurement error.

#### Chapter 15: Problem 23 ####
# Complete part a 

# A) What do we label this type of comparison? (identify and execute that test)


#### Chapter 15: Problem 26 ####
# Use the data to perform the correct test.  
# Please show code for all steps in your process.

#### Problem 15-30 and/or 15-31 (same data in both problems) ####
# Use the data to perform the correct test.  Please show code for all steps in
# your process.


