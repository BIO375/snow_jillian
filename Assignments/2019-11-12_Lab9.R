#### DO BEFORE EVERY EXAMPLE ####
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
fowler <- read_csv("datasets/demos/fowler.csv")
View(fowler)

ggplot(data = fowler) +
  geom_point(mapping = aes(x = FERTILIZER, y = YIELD ))

ggplot(data = fowler)+
  geom_boxplot(aes("", YIELD))

ggplot(data = fowler)+
  geom_qq(aes(sample = YIELD))

model01 <- lm(YIELD ~ FERTILIZER, data = fowler)

autoplot(model01, smooth.colour = NA)

# the normal Q-Q of the residuals: looks normal!
# Residuals vs. fitted plot: There is no fan shape!

fowler_plus <- augment(model01)
ggplot(data = fowler)+
  geom_point(aes(x = YIELD, y= resid(model01)))

# residual by x plot: Looks good! No fan shape.

summary(model01)

# (Linear regression: Yield = 51.93333 + 0.81139(Fertilzer);df = 1, 8, F=94.04, P<0.0001)
# R = 0.9216

ggplot(data = fowler, aes(x = YIELD, y = FERTILIZER)) +
  geom_point() +
  geom_smooth(method = "lm", level=0.95) +
  theme_bw()+
  labs( x = "Yield", y = "Fertilizer")

# A mjority of the points are within the 95% confidence interval.

#### 10/10 code runs without breaking ####