# Do before every question!
#Clean up the working environment
rm(list = ls())
# Verify working directory
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

# Install tidyverse
library("tidyverse")
tidyverse_update()

###Question 1####

library(readr)
Jaffe <- read_csv("datasets/demos/Jaffe.csv")
View(Jaffe)

head(Jaffe)
summary(Jaffe)

#Box plot, Histogram and Q-Q plot of Jaffe Data for HCB.
ggplot(Jaffe, aes(x = Depth, y = HCB))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(Jaffe) +
  geom_histogram(aes(HCB), binwidth = 1)+
  facet_wrap(~Depth)
ggplot(Jaffe)+
  geom_qq(aes(sample = HCB, color = Depth))

# Histogram: Looks normally distributed. Looks like there is not a huge skew.
# Boxplot: Whiskers are not even. Median is not centered for all depths: 
# Surface, Middepth and Bottom. There are also no outliers
# Q-Q plot: Middepth and Bottlom -> not perfectly linear... 
# For, the Suface -> pretty linear
# Graphs look generally normal, but not perfect. 

#Box plot, Histogram and Q-Q plot of Jaffe Data for Aldrin.
ggplot(Jaffe, aes(x = Depth, y = Aldrin))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(Jaffe) +
  geom_histogram(aes(Aldrin), binwidth = 1)+
  facet_wrap(~Depth)
ggplot(Jaffe)+
  geom_qq(aes(sample = Aldrin, color = Depth))
# Histogram: Looks normally distributed. Maybe a little skew.
# Boxplot: Whiskers "somewhat" even (maybe a little uneven for middepth). 
# Median is not centered for all depths: Surface with the worst and 
# Middepth most centered out of the 3 boxs. There is 1 outliers on bottom.
# Q-Q plot: Bottom has some cure. Middepth and Surface are more linear, 
# but are somewhat flat.
# I would say generally more normal then not normal. Maybe Transform to see
# if it will clean up, but I want to look at ratios before anything is done.

model01 <- lm(HCB~Depth, data = Jaffe)

model02 <- lm(Aldrin~Depth, data = Jaffe)

#Summary Statistics of HCB and Aldrin Data
summ_HCB <- Jaffe %>%
  group_by(Depth) %>% 
  summarise(mean_HCB = mean(HCB),
            sd_HCB = sd(HCB),
            n_HCB = n())

summ_Aldrin <- Jaffe %>%
  group_by(Depth) %>% 
  summarise(mean_Aldrin = mean(Aldrin),
            sd_Aldrin = sd(Aldrin),
            n_Aldrin = n())

ratio <-(max(summ_HCB$sd_HCB))/(min(summ_HCB$sd_HCB))

ratio <-(max(summ_Aldrin$sd_Aldrin))/(min(summ_Aldrin$sd_Aldrin))

# Graphs look generally normally distributed. Varience are equal for HCB (1.75),
# but not for Aldrin (3.819) so I would recommend transforming Aldrin data.

#Transformation Log10 of Aldrin Data
Jaffe02<-mutate(Jaffe, Aldrin=log10(Aldrin))

#Summary Statistics after Transformation of Aldrin
summ_1og10 <- Jaffe02 %>%
  group_by(Depth) %>% 
  summarise(mean_Aldrin = mean(Aldrin),
            sd_Aldrin = sd(Aldrin),
            n_Aldrin = n())

model03 <- lm(Aldrin~Depth, data = Jaffe02)

#New boxplots after Transformation of Aldrin
ggplot(Jaffe02, aes(x = Depth, y = Aldrin))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(Jaffe02) +
  geom_histogram(aes(Aldrin), binwidth = .1)+
  facet_wrap(~Depth)
ggplot(Jaffe02)+
  geom_qq(aes(sample = Aldrin, color = Depth))
# Histogram: Looks generally normally distruibuted. Maybe a little skew. 
# Boxplot:
# Q-Q plot: Suface and Middepth are less flat and still somewhat linear.
# Bottom is better but still has a little curve 

#Ratio of Transformation of Aldrin
ratio <-(max(summ_1og10$sd_Aldrin))/(min(summ_1og10$sd_Aldrin))
# Varienced are now equal/Within 3 (ratio = 2.087)

# Mode 1 = HCB, Mode 2 = Aldrin (before transformation) and Mode 3 = 
# log-transformed Aldrin

autoplot(model01)
autoplot(model02)
autoplot(model03)

anova(model01)
anova(model02)
anova(model03)

#Perform a Tukey-Kramer HSD
tukey <- glht(model03, linfct = mcp(depth = "Tukey"))
summary(tukey)
