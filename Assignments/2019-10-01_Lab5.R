rm(list = ls())

getwd()

library("tidyverse")

tidyverse_update()

install.packages("DescTools")
library("DescTools")

#Question 1:

library(readr)
Earth_Spin <- read_csv("datasets/demos/Earth_Spin.csv")
View(Earth_Spin)

y<-Earth_Spin$Obliquity

null_mean <- 23.4722

sample_mean <-mean(y)
sample_sd <- sd(y)
sample_n <- as.numeric(length(y))
df <- sample_n -1

t_sample <- (sample_mean - null_mean)/(sample_sd/sqrt(sample_n))

two_tailed <- 2*(1-pt(abs(t_sample), df))

#Question 2:

rm(list = ls())

library("tidyverse")

tidyverse_update()

library(readr)
heartAttack_data <- read_csv("datasets/demos/HeartAttack_short.csv")
View(heartAttack_data)

summ_HeartAttack_short <- heartAttack_data %>%
  group_by(group) %>% 
  summarise(mean_cholest = mean(cholest),
            sd_cholest = sd(cholest),
            n_cholest = n())

ratio <-(max(summ_HeartAttack_short$sd_cholest))/(min(summ_HeartAttack_short$sd_cholest))
