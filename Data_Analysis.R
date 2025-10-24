#Load Packages
library(tidyverse)
library(lubridate)
library(janitor)
library(data.table)
library(readr)
library(psych)
library(ggplot2)

#import the cleaned data
clean_trip_final <- read_csv("D:/Data for Case study/Data Monthly/clean_trip_final.csv")
str(clean_trip_final)
names(clean_trip_final)

# Remove rows that have NA in any of the specified columns
clean_trip_final_cleaned <- clean_trip_final %>%
  drop_na(month)


#order the data
clean_trip_final$month <- ordered(clean_trip_final$month,levels=c("Sep_24","Oct_24","Nov_24","Dec_24", 
                                                                  "Jan_25","Feb_25","Mar_25","Apr_25", 
                                                                  "May_25","Jun_25","Jul_25","Aug_25","Sep_25"))

clean_trip_final$week_day <- ordered(clean_trip_final$week_day, levels = c("Sunday", "Monday", "Tuesday", 
                                                                           "Wednesday", "Thursday", 
                                                                           "Friday", "Saturday"))

#Analysis:- min, max, median, average
summary(clean_trip_final$ride_length)

#Total no. of customers
View(table(clean_trip_final$customer_type))

#Total rides for each customer type in minutes
View(setNames(aggregate(ride_length ~ customer_type, clean_trip_final, sum), c("customer_type", "total_ride_len(mins)")))

#Differences between members and casual riders in terms of length of ride
View(clean_trip_final %>% 
       group_by(customer_type) %>% 
       summarise(min_length_mins = min(ride_length), max_length_min = max(ride_length),
                 median_length_mins = median(ride_length), mean_length_min = mean(ride_length)))

#Average ride_length for users by day_of_week and Number of total rides by day_of_week
View(clean_trip_final %>% 
       group_by(week_day,customer_type) %>% 
       summarise(Avg_length = mean(ride_length),
                 number_of_ride = n()))

#Average ride length comparison by each week day according to each customer type
View(aggregate(clean_trip_final$ride_length ~ clean_trip_final$customer_type + 
                 clean_trip_final$week_day, FUN = mean))

#Average ride length comparison by each month according to each customer type
View(aggregate(clean_trip_final$ride_length ~ clean_trip_final$customer_type + 
                 clean_trip_final$month, FUN = mean))

clean_trip_final_cleaned <- clean_trip_final %>%
  # Remove rows that have NA in any of the specified columns
  drop_na(month)


#Analyze rider length data by customer type and month
View(clean_trip_final %>% 
       group_by(customer_type, month) %>% 
       summarise(nummber_of_ride = n(),
                 average_duration = mean(ride_length),
                 median_duration = median(ride_length),
                 max_duration = max(ride_length),
                 min_duration = min(ride_length)))


#Save the data for data visualization
write.csv(clean_trip_final,file = "clean_trip_final_tableau.csv",row.names = FALSE)

nrow(clean_trip_final)


