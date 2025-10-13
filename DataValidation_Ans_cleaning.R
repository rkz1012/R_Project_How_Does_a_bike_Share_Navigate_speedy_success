#Install and Load Packages

install.packages("tidyverse")
install.packages("lubridate")
install.packages("janitor")
install.packages("psych")
library(tidyverse)
library(lubridate)
library(janitor)
library(data.table)
library(readr)
library(psych)
library(ggplot2)

#Import Data
sep_2024 <- read.csv("D:/Data for Case study/Data Monthly/202409-divvy-tripdata.csv")
oct_2024 <- read.csv("D:/Data for Case study/Data Monthly/202410-divvy-tripdata.csv")
nov_2024 <- read.csv("D:/Data for Case study/Data Monthly/202411-divvy-tripdata.csv")
dec_2024 <- read.csv("D:/Data for Case study/Data Monthly/202412-divvy-tripdata.csv")
jan_2025 <- read.csv("D:/Data for Case study/Data Monthly/202501-divvy-tripdata.csv")
feb_2025 <- read.csv("D:/Data for Case study/Data Monthly/202502-divvy-tripdata.csv")
mar_2025 <- read.csv("D:/Data for Case study/Data Monthly/202503-divvy-tripdata.csv")
apr_2025 <- read.csv("D:/Data for Case study/Data Monthly/202504-divvy-tripdata.csv")
may_2025 <- read.csv("D:/Data for Case study/Data Monthly/202505-divvy-tripdata.csv")
jun_2025 <- read.csv("D:/Data for Case study/Data Monthly/202506-divvy-tripdata.csv")
jul_2025 <- read.csv("D:/Data for Case study/Data Monthly/202507-divvy-tripdata.csv")
aug_2025 <- read.csv("D:/Data for Case study/Data Monthly/202508-divvy-tripdata.csv")
sep_2025 <- read.csv("D:/Data for Case study/Data Monthly/202509-divvy-tripdata.csv")

#Data Validation
colnames(sep_2024)
colnames(oct_2024)
colnames(nov_2024)
colnames(dec_2024)
colnames(jan_2025)
colnames(feb_2025)
colnames(mar_2025)
colnames(apr_2025)
colnames(may_2025)
colnames(jun_2025)
colnames(jul_2025)
colnames(aug_2025)
colnames(sep_2025)


# Total number of rows
sum(nrow(sep_2024) + nrow(oct_2024) + nrow(nov_2024)
    + nrow(dec_2024) + nrow(jan_2025) + nrow(feb_2025)
    + nrow(mar_2025) + nrow(apr_2025) + nrow(may_2025)
    + nrow(jun_2025) + nrow(jul_2025) + nrow(aug_2025)+ nrow(sep_2025))

# Merging 12 months data into a single Data Frame
trip_final <- rbind(sep_2024,oct_2024,nov_2024,dec_2024,jan_2025,feb_2025,mar_2025,apr_2025,may_2025,jun_2025,jul_2025,aug_2025,sep_2025)


# Save the combined files
write.csv(trip_final,file = "D:/Data for Case study/Data Monthly/trip_final.csv",row.names = FALSE)

#Count rows with "na" values
colSums(is.na(trip_final))

#Remove missing
clean_trip_final <- trip_final[complete.cases(trip_final), ]

#Remove duplicates
clean_trip_final <- distinct(clean_trip_final)

#Remove data with greater start_at than end_at
clean_trip_final<- clean_trip_final %>%
  filter(started_at < ended_at)

#Remove na
clean_trip_final <- drop_na(clean_trip_final)
clean_trip_final <- remove_empty(clean_trip_final)
clean_trip_final <- remove_missing(clean_trip_final)

#Check Cleaned data
colSums(is.na(clean_trip_final))
View(filter(clean_trip_final, clean_trip_final$started_at > clean_trip_final$ended_at))

#Check Cleaned data
colSums(is.na(clean_trip_final))
View(filter(clean_trip_final, clean_trip_final$started_at > clean_trip_final$ended_at))

#Renaming column for better context
clean_trip_final <- rename(clean_trip_final, costumer_type = member_casual, bike_type = rideable_type)


#Separate date in date, day, month, year for better analysis
clean_trip_final$date <- as.Date(clean_trip_final$started_at)
clean_trip_final$week_day <- format(as.Date(clean_trip_final$date), "%A")
clean_trip_final$month <- format(as.Date(clean_trip_final$date), "%b_%y")
clean_trip_final$year <- format(clean_trip_final$date, "%Y")

#Separate column for time
clean_trip_final$time <- as.POSIXct(clean_trip_final$started_at, format = "%Y-%m-%d %H:%M:%S")
clean_trip_final$time <- format(clean_trip_final$time, format = "%H:%M")

#Add ride length column
clean_trip_final$ride_length <- difftime(clean_trip_final$ended_at, clean_trip_final$started_at, units = "mins")

#Select the data we are going to use
clean_trip_final <- clean_trip_final %>%
  select(bike_type, costumer_type, month, year, time, started_at, week_day, ride_length)

#Remove stolen bikes or accidental start/stop
clean_trip_final <- clean_trip_final[!clean_trip_final$ride_length>1440,]
clean_trip_final <- clean_trip_final[!clean_trip_final$ride_length<5,]

#Save the cleaned data
write.csv(clean_trip_final,file = "clean_trip_final.csv",row.names = FALSE)



view(head(clean_trip_final))