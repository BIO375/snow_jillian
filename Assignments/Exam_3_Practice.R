library(readr)
peake <- read_csv("datasets/demos/peake.csv")
View(peake)

model01 <- lm(SPECIES ~ AREA, data = peake)

autoplot(model01, smooth.colour = NA)

fowler_plus <- augment(model01)
ggplot(data = peake)+
  geom_point(aes(x = SPECIES, y= resid(model01)))

ggplot(data = peake, aes(x = SPECIES, y = AREA)) +
  geom_point() +
  geom_smooth(method = "lm", level=0.95) +
  theme_bw()+
  labs( x = "Species", y = "Area")

peakelog10 <- peake %>%
  mutate(AREA = log(AREA))
summ_ln_skin <- salmon %>%
  group_by(SPECIES) %>%
  summarise(mean_ln_skin = mean(AREA),
            sd_ln_skin = sd(AREA))

model02 <- lm(SPECIES ~ AREA, data = peakelog10)

summary(model02)
