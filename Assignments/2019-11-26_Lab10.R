#### Lab 10: Chi-squared and friends #### 

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

### Binomial Test ####

# x: the number of observed successes
# n: the number of trials
# p: hypothesized probability of success *Needs to be a number between 0 and 1*
# alternative: the alternative hypothesis (takes "two.sided", "less", or "greater")
# conf.level: confidence level for the CI returned in analysis

# For Females:
model01 <- binom.test(x=41, n=90, p=0.5, alternative = "greater", conf.level = 0.95 )
model01


# For Males:
model02 <- binom.test(x= 49, n=90, p=0.5, alternative = "greater", conf.level = 0.95 )
model02

### Chi-squared goodness of fit ####

# Did by hand on lab packet.

### Contingency Table Analysis ####

tab01 <- matrix(c(17, 49, 30, 41), 2, 2, byrow=TRUE)

dimnames(tab01) <- list("Sex Ratio" = c("Male", "Female"),
                        "Locality" = c("Belgium", "Holland"))

as.matrix(tab01)
model05 <- chisq.test(tab01, correct = FALSE)
model05
