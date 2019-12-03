# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Install package ggfortify, *note* only do install.packages ONCE
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

library(readr)
DriverVision <- read_csv("datasets/exams/DriverVision.csv")
View(DriverVision)

model01 <- lm(Distance ~ Age , data = DriverVision)

autoplot(model01, smooth.colour = NA)

### CODE BREAKS BECAUSE LIBRARY BROOM NOT LOADED ####

DriverVision_plus <- augment(model01)
ggplot(data = DriverVision_plus)+
  geom_point(aes(x = Age, y= .resid))

ggplot(data = DriverVision, aes(x = Age, y = Distance)) +
  geom_point() +
  geom_smooth(method = "lm", level=0.95) +
  theme_bw()+
  labs( x = "Age", y = "Distance")

summary(model01)

#### Code breaks once because library "broom" not loaded ####

