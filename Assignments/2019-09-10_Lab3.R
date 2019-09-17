### Lab 3. Data manipulation and graphing

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

# Read in data file
ward_data<-read_csv("datasets/quinn/chpt3/ward.csv", col_names = TRUE)

# Pasted from Import Dataset Tool
# Note that for us, library(readr) is redundant because we loaded it with
# all the other tidyverse packages earlier
library(readr)
ward <- read_csv("datasets/quinn/chpt3/ward.csv")

# Read in compensation data file
compensation<-read_csv("datasets/demos/compensation.csv")

# names() tells you the names assigned to each column, generally variable
# names
names(ward)

# head() gives you the first six rows of a dataset
head(ward)

# dim() gives you the dimensions of your dataset
dim(ward)

# str() returns the structure of the dataset
str(ward)

# Calculate summary statistics about groups.  I give the general form below
# in comments

# <new_object_name> <- <data> %>%
# group_by(<grouping_variable>) %>%
# summarise(
# mean_resp = mean(<response_variable_name>),
# median_resp = median(<response_variable_name>),
# IQR_resp = IQR(<response_variable_name>),
# sd_resp = sd(<response_variable_name>),
# var_resp = var(<response_variable_name>)
# )

summ_eggs <- ward %>%
group_by(ZONE) %>% 
  summarise(mean_eggs = mean(EGGS),
            median_eggs = median(EGGS),
            IQR_eggs = IQR(EGGS),
            sd_eggs = sd(EGGS),
            var_eggs = var(EGGS))

View(summ_eggs)

# mutate() adds new variables while preserving existing ones.  General form:
# <dataset_name> <- mutate(<dataset_name>, <transform_variable_name> =
# <mathematical_function>(<variable_name>))
ward<-mutate(ward, squareroot_eggs = sqrt(EGGS))

compensation<-mutate(compensation, log(Root))

# R for Data Science, Chapter 3
# https://r4ds.had.co.nz/data-visualisation.html
# Enter your code here
install.packages("tidyverse")
library(tidyverse)
ggplot2::mpg
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = hwy, y = cyl))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = class, y = drv))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

# Compare the histograms and boxplots of EGGS and squareroot_eggs
ggplot(ward) +
  geom_histogram(aes(EGGS), binwidth = 2)+
  facet_wrap(~ZONE)
ggplot(ward) +
  geom_histogram(aes(squareroot_eggs), binwidth = 0.5)+
  facet_wrap(~ZONE)

ggplot(ward)+
  geom_boxplot(aes(x = ZONE, y = EGGS), notch = TRUE, varwidth = TRUE)
ggplot(ward)+
  geom_boxplot(aes(x = ZONE, y = squareroot_eggs), notch = TRUE, varwidth = TRUE)

### Assignment

# Load the sanchez.csv file
# Enter your code here

library(readr)
sanchez <- read_csv("datasets/demos/sanchez.csv")
View(sanchez)

# Calculate summary statistics
# Enter your code here

summ_Bird_Colony <- sanchez %>%
  group_by(Bird_Colony) %>% 
  summarise(mean_Beetle_Density = mean(Beetle_Density),
            median_Beetle_Density = median(Beetle_Density),
            IQR_Beetle_Density = IQR(Beetle_Density),
            sd_Beetle_Density = sd(Beetle_Density),
            var_Beetle_Density = var(Beetle_Density))

View(summ_Bird_Colony)


# Add a new column of log(y+1) transformed beetle densities to the sanchez dataset
# Enter your code here

sanchez<-mutate(sanchez, log_Beetle = log(Beetle_Density+1))


# Generate histograms of beetle density by colony type before and after data 
# transformation
# Enter your code here

ggplot(sanchez) +
 geom_histogram(aes(Beetle_Density), binwidth = 8)+
 facet_wrap(~Bird_Colony)

ggplot(sanchez) +
  geom_histogram(aes(log_Beetle), binwidth = .1)

# Plot boxplots of beetle density by colony type before and after data 
# transformation
# Enter your code here

ggplot(sanchez)+
  geom_boxplot(aes(x = Bird_Colony, y = Beetle_Density), notch = FALSE, varwidth = TRUE)

ggplot(sanchez)+
  geom_boxplot(aes(x = Bird_Colony, y = log_Beetle), notch = FALSE, varwidth = TRUE)
