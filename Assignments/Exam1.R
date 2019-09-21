# JILLIAN, you need to start by clearing everything out.  I use the function rm, but you can 
# also restart R

# JILLIAN, because you did not load the entire tidyverse, your code below does not work because pipes
# "%>%" and functions like group_by, summarise, ggplot etc. all depend on the tidyverse
# If this were part of the same script as you used for the Artemesia problem, and if that script loaded
# the tidyverse library, then it probably worked for you, but the code breaks if ran all by itself.
# you need a line library(tidyverse)
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
# You have to put a hashtag at the beginning of lines 20 and 27 or else it breaks your code
chap13e5SagebrushCrickets<-mutate(chap13e5SagebrushCrickets, log_timeToMating = log(timeToMating))

C) and D)
ggplot(chap13e5SagebrushCrickets) +
  geom_histogram(aes(log_timeToMating), binwidth = .5)+
  facet_wrap(~feedingStatus)
  