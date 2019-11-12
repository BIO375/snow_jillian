#### DO BEFORE EVERY EXAMPLE ####
# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load ggfortify for plotting
library("ggfortify")

# Load broom to convert statistical objects to tidy tibbles and plotly
# for confidence bands
# If you have not installed broom before, you will need to execute
# install.packages("broom")
library("broom")

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

library(readr)
fowler <- read_csv("datasets/demos/fowler.csv")
View(fowler)

ggplot(data = fowler) +
  geom_point(mapping = aes(x = FERTILIZER, y = YIELD ))

ggplot(data = fowler)+
  geom_histogram(aes(YIELD), binwidth = 25)

ggplot(data = fowler)+
  geom_qq(aes(sample = YIELD))

model01 <- lm(YIELD ~ FERTILIZER, data = fowler)

autoplot(model01, smooth.colour = NA)

summary(model01)

# (Linear regression: Intercept = 51.93333 + 0.81139(Fertilzer);df = 1, 8, F=94.04, P<0.0001)
# R = 0.9216




