##################################################################
# The purpose of this R script is to demonstrate a linear regression
# statistical analysis. This was made mostly for my own reference and a way to 
# practice these functions so it is far from perfect or comprehensive,  
# but I will share it for others who may find it helpful.
#
# Script by: Kim Fake
#
#
###################################################################

# Load libraries
library(dplyr) # pretty much always load for filtering and manipulating data
library(lubridate)


#load data
data <- read.csv (
  './Biology 101 Data CLEAN.csv', 
  stringsAsFactors = FALSE, 
  fileEncoding = 'UTF-8-BOM'
)


# In regression, it is often recommended to center and scale continuous variables 
# so that the predictors have a mean of 0 and a standard deviation of 1

data$Classes_Attended <- scale(data$Classes_Attended)
data$Percent_Grade <- scale(data$Percent_Grade)

# as you can see, this data is on 3 biology classes
# we want to use a linear regression to determine if the grades (%) 
# related to the number of classes student attend during the semester

# linear regression model
# examining the relationship between percent grades 
# and the number of classes attended in a semester
model <- lm(
  Percent_Grade ~ Classes_Attended,
  data=data
)
summary(model)

# the coefficient in the model for classes attended is positive and significant
# this indicates that students that attend more classes tend to have higher grades

# However, we hypothesize Age might have an effect 
# on the study habitats and grades of students
# and want to include it in our model to control for
# age differences among students

#first we must calculate the age of students based on their birthdates provided
# change a data type to date
data$B_Day <- as.Date(data$B_Day, 
                      format= "%m/%d/%Y"
)

# create a year of birth column
data <- data %>%
  dplyr::mutate(Year = lubridate::year(B_Day))

# make a new column with age made based on birth year
data <- data %>%
  mutate(Age = 2022-Year)

# we must also scale age
data$Age <- scale(data$Age)

# linear regression model
# examining the relationship between percent grades 
# and the number of classes attended in a semester
# with Age included as a control
model <- lm(
  Percent_Grade ~ Classes_Attended + Age,
  data=data
  )
summary(model)

# the coefficient in the model for classes attended is positive and significant
# this indicates that students that attend more classes tend to have higher grades

# the coefficient in the model for age is not significant
# so student grades did not significantly relate to their age

# the previous model, however does not consider the non-independence of students
# sampled from different biology classes, and as grades may vary between classes due
# to other factors such as instructors, we will class as a random effect in our model
# for this we will need a generalized linear mixed-effects model and so
# we must load a package with a function that allows for random effects in the model

#load package 
library(lme4)

# set class as a factor
data$Class <- as.factor(data$Class)

# Fit a generalized linear mixed-effects models
# with the added random effects
model <- lmer(
  Percent_Grade ~ Classes_Attended + Age + (1|Class),
  data=data
)
summary(model)

# although this package does not return with an indication of significance
# using p-values, we can see if the 95% CI of the coefficient over laps with 
# zero to determine if it is significant
# 0.4171-0.1117 to 0.4171+0.1117 does not overlap with zero
# Therefore there is a significantly positive relationship between
# the number of classes attended and student grade