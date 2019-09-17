library(readr)
Shrimp <- read_csv("datasets/demos/Shrimp.csv")
View(Shrimp)

summ_Shrimp_Forms <- Shrimp %>%
  group_by(Shrimp_Forms) %>% 
  summarise(mean_Body_Length = mean(Body_Length),
            median_Body_Length = median(Body_Length),
            IQR_Body_Length = IQR(Body_Length),
            sd_Body_Length = sd(Body_Length),
            var_Body_Length = var(Body_Length))

View(summ_Shrimp_Forms)

ggplot(Shrimp)+
  geom_boxplot(aes(x = Shrimp_Forms, y = Body_Length), notch = FALSE, varwidth = TRUE)
