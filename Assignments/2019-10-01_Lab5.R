# Make Sure to do these steps before each question to confirm everything is cleared, updated and all packages are installed.
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

library(readr)
heartAttack_data <- read_csv("datasets/demos/HeartAttack_short.csv", col_names = TRUE,
                             col_types = cols(
                               group = col_character())
)
View(heartAttack_data)

summ_HeartAttack_data <- heartAttack_data %>%
  group_by(group) %>% 
  summarise(mean_cholest = mean(cholest),
            sd_cholest = sd(cholest),
            n_cholest = n())

ggplot(heartAttack_data) +
  geom_histogram(aes(cholest), binwidth = 25)+
  facet_wrap(~group)

ggplot(heartAttack_data) +
  geom_boxplot(aes(x = group, y = cholest), varwidth = TRUE)
  
ggplot(heartAttack_data)+
  geom_qq(aes(sample = cholest, color = group))

ratio <-(max(summ_HeartAttack_short$sd_cholest))/(min(summ_HeartAttack_short$sd_cholest))

t.test(cholest ~ group, data = heartAttack_data, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

#Question 3

library(readr)
furness <- read_csv("datasets/quinn/chpt3/furness.csv")
View(furness)

ggplot(furness) +
  geom_histogram(aes(METRATE), binwidth = 400)+
  facet_wrap(~SEX)

ggplot(furness) +
  geom_boxplot(aes(x = SEX, y = METRATE))

ggplot(furness)+
  geom_qq(aes(sample = METRATE, color = SEX))

wilcox.test(METRATE ~ SEX, data = furness, alternative = "two.sided", conf.level = 0.95)

#Question 4

library(readr)
elgar <- read_csv("datasets/quinn/chpt3/elgar.csv")
View(elgar)

elgar <- mutate(elgar, diff = HORIZDIM - HORIZLIG)

ggplot(elgar) +
  geom_histogram(aes(diff), binwidth = 60)

ggplot(elgar) +
  geom_boxplot(aes(x = "", y = diff))

ggplot(elgar)+
  geom_qq(aes(sample = diff))

t.test(elgar$HORIZDIM, elgar$HORIZLIG, 
       alternative = "two.sided", paired = TRUE, conf.level = 0.95)
