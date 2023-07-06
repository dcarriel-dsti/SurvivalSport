#time to event = time to finish the race (status = 1)


#import useful libraries
library(tidyverse)
library(broom)
library(dplyr)
library(survival)
library(lubridate)

# Data preparation
setwd ('C:/Users/romai/Documents/DSTI/21-Survival Analysis/UTMB')
data_utmb17 <- read_csv("utmb_2017.csv", col_names = TRUE)

#problems(data_utmb17)
colnames(data_utmb17)
str(data_utmb17)

#delete 1st column (useless)
data_utmb17 <- data_utmb17[,-1]


#create a column gender & age from category + status (1 = finisher; 0 = DNF / not finisher)
data_utmb17 <-data_utmb17 |>
  mutate(gender = case_when(
    endsWith(category, " H") ~ "Male",
    endsWith(category, " F") ~ "Female"),
    age = substring(data_utmb17$category, first=1, last=2), #create a column age from category
    status = case_when(time != 'NA' ~ 1, TRUE ~ 0),
    .after ="category")


#Keep the highest time of the checkpoints and create a new column HighestTime
data_utmb17$highesttime <- apply(data_utmb17[11:35], 1, function(x) max(x, na.rm = TRUE))
data_utmb17<-data_utmb17|>
  mutate(highesttime = replace_na(highesttime, '00:00:00'))

#conversion in time format (expressed in seconds)
data_utmb17$timetoevent<- lubridate::hms(data_utmb17$highesttime)
data_utmb17$timetoevent<- period_to_seconds(data_utmb17$timetoevent)


#remove intermediate checkpoints time
data_utmb17<- data_utmb17[,-11:-34]
colnames(data_utmb17)


#keep useful columns: bib (or ID), gender, age, status, nationality and time
data_utmb17<-  data_utmb17[,-c(2,3,4,8,9,10)]


#Conversion of age category (SE, V1, V2, V3, V4) in age range (in years)
age_range <- tibble('age' =
                      c("V1", "V2","V3", "V4", "SE"),
                    'age_range' = c("40-49", "50-59", "60-69", "over 70", "23-39")
                    )

data_utmb17 <- data_utmb17 |> inner_join(age_range,by = "age")

#Move column "age_range" just after column "age"
data_utmb17 <- data_utmb17  %>% relocate("age_range", .after = "age")
table(data_utmb17$age_range)

view(data_utmb17)

#Kaplan-Meier
fit.KM <- survfit(Surv(timetoevent, status) ~ 1, data = data_utmb17)
summary(fit.KM)
fit.KM

plot(fit.KM, mark.time = TRUE,
     main = "Kaplan-Meier estimator",
     ylab = "Survival probability",
     xlab = "time (seconds)")

# AGE
## Kaplan-Meier
fit.KMage <- survfit(Surv(timetoevent, status) ~ age_range, data = data_utmb17)
fit.KMage
#maybe merge the 2 oldest range together (only 5 with age over 70)

plot(fit.KMage, col = 3:8)
legend("bottomleft", lty = 1, col = 3:8, legend = names(fit.KMage$strata))

##log rank test by age
diff.KMage <- survdiff(Surv(timetoevent, status) ~ age_range, data = data_utmb17)
diff.KMage
## p-value = 2E-10 (<<0.05), we reject H0 => there exists at least a significant difference
## between 2 age range

## semi-parametric Cox regression
cox.age<- coxph(Surv(timetoevent, status) ~ age_range, data = data_utmb17)
summary(cox.age)

#GENDER
## Kaplan-Meier
fit.KMgender <- survfit(Surv(timetoevent, status) ~ gender, data = data_utmb17)
fit.KMgender

plot(fit.KMgender, col = c("#FF3399", "#0066FF"),pch = 19)
legend("bottomleft", lty = 1, col = c("#FF3399", "#0066FF"), cex= 0.75, legend = names(fit.KMgender$strata))

##log rank test by gender
diff.KMgender <- survdiff(Surv(timetoevent, status) ~ gender, data = data_utmb17)
diff.KMgender
##The p-value is large (p=0.1): the difference *is not* statistically significant.

## semi-parametric Cox regression
cox.gender<- coxph(Surv(timetoevent, status) ~ gender, data = data_utmb17)
summary(cox.gender)

#AGE AND GENDER
fit.KMage_gender <- survfit(Surv(timetoevent, status) ~ age_range + strata(gender), data = data_utmb17)
fit.KMage_gender

plot(fit.KMage_gender, col = 1:9)
legend("bottomleft",lty = 1, col = 1:9, legend = names(fit.KMage_gender$strata), cex= 0.6, box.lty=1)

diff.KMage_gender1 <- survdiff(Surv(timetoevent, status) ~ gender + age_range, data = data_utmb17)
diff.KMage_gender1

diff.KMage_gender2 <- survdiff(Surv(timetoevent, status) ~ gender + strata(age_range), data = data_utmb17)
diff.KMage_gender2

## semi-parametric Cox regression
cox.age_gender<- coxph(Surv(timetoevent, status) ~ gender + age_range, data = data_utmb17)
summary(cox.age_gender)

