
library(tidyverse)
library(broom)
library(dplyr)
library(survival)

# Data preparation
setwd ('C:/Users/romai/Documents/DSTI/21-Survival Analysis/UTMB')


data_utmb17 <- read_csv("utmb_2017.csv", col_names = TRUE)

problems(data_utmb17)
colnames(data_utmb17)
str(data_utmb17)

data_utmb17 <- data_utmb17[,-1] #delete 1st column 


#create a column gender & age from category + status (1 = finisher; 0 = DNF / not finisher)
data_utmb17 <-data_utmb17 |>
  mutate(gender = case_when(
    endsWith(category, " H") ~ "Male",
    endsWith(category, " F") ~ "Female"),
    age = substring(data_utmb17$category, first=1, last=2), #create a column age from category
    status = case_when(time != 'NA' ~ 1, TRUE ~ 0),
    .after ="category")


#remove intermediate checkpoints time
data_utmb17<-  data_utmb17[,-11:-34]
colnames(data_utmb17)



#keep useful columns: bib (or ID), gender, age, status, nationality and time
data_utmb17<-  data_utmb17[,-c(2,3,4,8,11)]

View(data_utmb17)

summary(data_utmb17)

class(data_utmb17$time) # to convert in a more accurate format?

#Kaplan-Meier
fit.KM <- survfit(Surv(time, status) ~ 1, data = data_utmb17)
summary(fit.KM)
fit.KM


plot(fit.KM, mark.time = TRUE,
     main = "Kaplan-Meier estimator",
     ylab = "Survival probability",
     xlab = "time (seconds)")
