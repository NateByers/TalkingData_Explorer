library(dplyr)
source("C:/Repositories/TalkingData/makeRegions.R")
app_events <- read.csv("C:/Repositories/TalkingData/data/app_events.csv",
                       stringsAsFactors = FALSE) 
app_labels <- read.csv("C:/Repositories/TalkingData/data/app_labels.csv",
                       stringsAsFactors = FALSE) 
events <- read.csv("C:/Repositories/TalkingData/data/events.csv",
                   stringsAsFactors = FALSE)  %>%
  makeRegions() %>%
  mutate(date = as.Date(timestamp),
         hour = as.numeric(substr(timestamp, 12, 13)),
         morning = hour > 5 & hour < 12,
         midday = hour >= 12 & hour < 18,
         evening = hour >= 18 & hour < 21,
         night = hour >= 21 | hour <= 5) 
events$time <- NA 
for(i in c("morning", "midday", "evening", "night")){
  events[events[, i], "time"] <- i
  events <- events[, names(events)[names(events) != i]]
}
gender_age_test <- read.csv("C:/Repositories/TalkingData/data/gender_age_test.csv",
                            stringsAsFactors = FALSE) 
gender_age_train <- read.csv("C:/Repositories/TalkingData/data/gender_age_train.csv",
                             stringsAsFactors = FALSE) 
label_categories <- read.csv("C:/Repositories/TalkingData/data/label_categories.csv",
                             stringsAsFactors = FALSE) 
sample_submission <- read.csv("C:/Repositories/TalkingData/data/sample_submission.csv",
                              stringsAsFactors = FALSE) 

phone_brand_device_model <- read.csv("C:/Repositories/TalkingData/data/phone_brand_device_model.csv",
                                     encoding = "UTF-8", stringsAsFactors = FALSE) 

save(app_events, app_labels, events, gender_age_train,
     label_categories, phone_brand_device_model,
     file = "data/shiny_data.rda")
