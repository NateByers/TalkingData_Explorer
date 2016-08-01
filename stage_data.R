library(dplyr)
library(data.table)
library(bit64)
source("C:/Repositories/TalkingData/makeRegions.R")
app_events <- fread("C:/Repositories/TalkingData/data/app_events.csv") %>%
  as.data.frame()
app_labels <- fread("C:/Repositories/TalkingData/data/app_labels.csv") %>%
  as.data.frame()
events <- fread("C:/Repositories/TalkingData/data/events.csv") %>%
  as.data.frame() %>%
  makeRegions()
gender_age_test <- fread("C:/Repositories/TalkingData/data/gender_age_test.csv") %>%
  as.data.frame()
gender_age_train <- fread("C:/Repositories/TalkingData/data/gender_age_train.csv") %>%
  as.data.frame()
label_categories <- fread("C:/Repositories/TalkingData/data/label_categories.csv") %>%
  as.data.frame()
sample_submission <- fread("C:/Repositories/TalkingData/data/sample_submission.csv") %>%
  as.data.frame()

phone_brand_device_model <- read.csv("C:/Repositories/TalkingData/data/phone_brand_device_model.csv",
                                     encoding = "UTF-8", stringsAsFactors = FALSE) 

save(app_events, app_labels, events, gender_age_train,
     label_categories, phone_brand_device_model,
     file = "data/shiny_data.rda")
