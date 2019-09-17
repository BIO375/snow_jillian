library(readr)
chap13e5SagebrushCrickets <- read_csv("datasets/abd/chapter13/chap13e5SagebrushCrickets.csv")
View(chap13e5SagebrushCrickets)

umm_feedingStatus <- chap13e5SagebrushCrickets %>%
       group_by(feedingStatus) %>% 
       summarise(mean_timeToMating = mean(timeToMating),
                  median_timeToMating = median(timeToMating),
                  IQR_timeToMating = IQR(timeToMating),
                  sd_timeToMating = sd(timeToMating),
                  var_timeToMating = var(timeToMating))
A) and B)
ggplot(chap13e5SagebrushCrickets) +
  geom_histogram(aes(timeToMating), binwidth = 10)+
  facet_wrap(~feedingStatus)

chap13e5SagebrushCrickets<-mutate(chap13e5SagebrushCrickets, log_timeToMating = log(timeToMating))

C) and D)
ggplot(chap13e5SagebrushCrickets) +
  geom_histogram(aes(log_timeToMating), binwidth = .5)+
  facet_wrap(~feedingStatus)
  