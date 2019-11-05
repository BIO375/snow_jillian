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
Jaffe <- read_csv("datasets/demos/Jaffe.csv", col_types = cols(
Depth = col_factor() ))
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

# Histogram: Looks fairly normally distributed. It doesn't look like there is a strong 
# skew/hard to tell.
# Boxplot: Whiskers are not even for Mid-depth and Bottom (both left-skewed), 
# while Whiskers for Surface look fairly even.  Medians are not centered, 
# but for bottom and surface means and medians are fairly close. There are also 
# no outliers. Overall, I would say the boxplot looks fairly normal. 
# Q-Q plot: Mid depth, Surface and Bottom look fairly linear.
# Overall: I would say the HCB would meet the assumption of ANOVA.

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

# Histogram: Looks fairly normally distributed.
# Boxplot: Whiskers look "somewhat" even for surface and mid-depth. There is 
# definite right-skewed for bottom because of the outlier. Median is not centered 
# for all depths. Median and means for mid-surface and surface are fairly close
# vs. Bottom is not as close as the other two.  Bottom also has 1 outlier! 
# Due to the skew and outlier for Bottom, I would say the boxplot is not normal.
# Q-Q plot: Mid-depth and Surface look fairly linear, but Bottom looks more 
# exponential rather than linear. Based on bottom, I would say that the Q-Q  plot
# is not normal.
# Overall: Overall: Based on the boxplot and Q-Q plot I would say that Aldrin
# doesn't meet the assumptions of ANOVA.

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
# log10-transformed Aldrin

autoplot(model01)
autoplot(model02)
autoplot(model03)

anova(model01)
anova(model02)
anova(model03)

#Perform a Tukey-Kramer on Log10-Transformed Aldrin Data
Tukey_Mode03 <- glht(model03, linfct = mcp(Depth = "Tukey"))
summary(Tukey_Mode03)

#Perform a Tukey-Kramer on HCB Data
Tukey_Mode01 <- glht(model01, linfct = mcp(Depth = "Tukey"))
summary(Tukey_Mode01)
