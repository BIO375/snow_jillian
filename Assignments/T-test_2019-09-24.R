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
data01 <- %>% slice (-105)

The code u gave above would not slice the 105 N/A observation in the dataset so I put the dataset into excel and took out the n/a myself as a way of working around the problem.  
This is the dataset I made (ignore the X3 column <-doesnt affect anything):
  
library(readr)
data01 <- read_csv("datasets/demos/data01.csv")
View(data01)

summ_data01 <- data01 %>%
  group_by(Survival) %>% 
  summarise(mean_squamosalHornLength = mean(squamosalHornLength),
            median_squamosalHornLength = median(squamosalHornLength),
            sd_squamosalHornLength = sd(squamosalHornLength),
            var_squamosalHornLength = var(squamosalHornLength))

ggplot(data01) +
  geom_histogram(aes(squamosalHornLength), binwidth = 2)

ggplot(data01)+
  geom_boxplot(aes(x = Survival, y = squamosalHornLength), notch = TRUE, varwidth = TRUE)

