### Senerio 1 ###

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load ggfortify for plotting
install.packages("ggfortify")
library("ggfortify")

# Load broom to convert statistical objects to tidy tibbles and plotly
# for confidence bands
# If you have not installed broom before, you will need to execute
install.packages("broom")
library("broom")

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

library(readr)
insulation <- read_csv("datasets/final/insulation.csv")
View(insulation)

ggplot(data = insulation) +
  geom_point(mapping = aes(x = leanness, y = heat_loss))

ggplot(data = insulation)+
  geom_histogram(aes(leanness), binwidth = .8)
ggplot(data = insulation)+
  geom_histogram(aes(heat_loss), binwidth = .01)

ggplot(data = insulation)+
  geom_boxplot(aes("",leanness))
ggplot(data = insulation)+
  geom_boxplot(aes("",heat_loss))

ggplot(data = insulation)+
  geom_qq(aes(sample = leanness))
ggplot(data = insulation)+
  geom_qq(aes(sample = heat_loss))

insulationCor <- cor.test(~ leanness + heat_loss, data = insulation,
                     method = "pearson")
insulationCor

r <- insulationCor$estimate
r

### Senerio 2 ###

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
caffeine <- read_csv("datasets/final/caffeine.csv", col_types = cols(
  group = col_factor() ))
View(caffeine)

head(caffeine)
summary(caffeine)

ggplot(caffeine, aes(x = "", y = half_life))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()

ggplot(caffeine) +
  geom_histogram(aes(half_life), binwidth = 1)

model01 <- lm(half_life~group, data = caffeine)

summ_caffeine <- caffeine %>%
  group_by(group) %>% 
  summarise(mean_half_life = mean(half_life),
            sd_half_life = sd(half_life),
            n_half_life = n())

ratio <-(max(summ_caffeine$sd_half_life))/(min(summ_caffeine$sd_half_life))

autoplot(model01)

anova(model01)

planned <- glht(model01, linfct = 
                  mcp(caffeine = c("Male - control = 0",
                                  "norm_prog - control = 0",
                                  "high_prog - control = 0")))
confint(planned)
summary(planned)

### Senerio 3 ###
# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

install.packages("ggmosaic")
library("ggmosaic")

install.packages("epitools")
library("epitools")

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

model02 <-chisq.test(x = davis$observed, p = davis$expected_p)
model02

# CODE BREAKS LINE 108 BECAUSE THERE IS NO GROUP CALLED CONTROL
# CODE BREAKS LIN 132 BECAUSE DID NOT READ IN DAVIS.CSV

### 8/10 PTS ####
