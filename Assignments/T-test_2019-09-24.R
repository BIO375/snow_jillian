Scenerio One:
  
rm(list = ls())

library("tidyverse")

tidyverse_update()

library(readr)
Births <- read_csv("datasets/demos/Births.csv")
View(Births)

summ_Country <- Births %>%
  summarise(mean_Birth_Difference = mean(Birth_Difference),
            median_Birth_Difference = median(Birth_Difference),
            IQR_Birth_Difference = IQR(Birth_Difference),
            sd_Birth_Difference = sd(Birth_Difference),
            var_Birth_Difference = var(Birth_Difference))

View(summ_Country)

ggplot(Births) +
  geom_histogram(aes(Birth_Difference), binwidth = 2)

ggplot(Births)+
  geom_boxplot(aes(x = Country, y = Birth_Difference), notch = TRUE, varwidth = TRUE)

Scenerio 2:
  
rm(list = ls())

library("tidyverse")

tidyverse_update()
  
data01 <- read_csv("datasets/abd/chapter12/chap12e3HornedLizards.csv")
data01 <- data01 %>% slice (-105)

summ_data01 <- data01 %>%
  group_by(Survival) %>% 
  summarise(mean_squamosalHornLength = mean(squamosalHornLength),
            median_squamosalHornLength = median(squamosalHornLength),
            IQR_squamosalHornLength = IQR(squamosalHornLength),
            sd_squamosalHornLength = sd(squamosalHornLength),
            var_squamosalHornLength = var(squamosalHornLength))

ggplot(data01) +
  geom_histogram(aes(squamosalHornLength), binwidth = 2) +
  facet_wrap(~Survival)

ggplot(data01)+
  geom_boxplot(aes(x = Survival, y = squamosalHornLength), notch = TRUE, varwidth = TRUE)

#### 10/10 code runs without breaking ####