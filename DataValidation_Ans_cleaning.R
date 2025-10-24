# =============================================================================
# 1. SETUP: Install and Load Packages (Unchanged)
# =============================================================================
# Note: You can comment out 'install.packages' after the first successful run.
# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("janitor")
# install.packages("psych")
library(tidyverse)
library(lubridate)
library(janitor)
library(data.table)
library(readr)
library(psych)
library(ggplot2)

# =============================================================================
# 2. IMPORT AND MERGE DATA
# =============================================================================
# IMPORTANT: These paths are specific to your machine and will cause an error
# if the files are not found. We assume the files exist for the user.

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

# Merging 13 months of data into a single data frame
trip_final <- rbind(sep_2024, oct_2024, nov_2024, dec_2024, jan_2025, feb_2025,
                    mar_2025, apr_2025, may_2025, jun_2025, jul_2025, aug_2025, sep_2025)

# Free up memory by removing the individual data frames
rm(list=c("sep_2024", "oct_2024", "nov_2024", "dec_2024", "jan_2025", "feb_2025",
          "mar_2025", "apr_2025", "may_2025", "jun_2025", "jul_2025", "aug_2025", "sep_2025"))


# =============================================================================
# 3. INITIAL CLEANING & TYPE CONVERSION (CRITICAL FIXES)
# =============================================================================

# Save the combined files (Optional, useful checkpoint)
write.csv(trip_final, file = "D:/Data for Case study/Data Monthly/trip_final.csv", row.names = FALSE)

# Convert character date/time columns to POSIXct format for accurate comparison
# FIX: This step must happen before difftime and filtering on date/time.
trip_final$started_at <- as.POSIXct(trip_final$started_at, format = "%Y-%m-%d %H:%M:%S")
trip_final$ended_at <- as.POSIXct(trip_final$ended_at, format = "%Y-%m-%d %H:%M:%S")


# Remove rows with NA values (using complete.cases is the most efficient way)
clean_trip_final <- trip_final[complete.cases(trip_final), ]

# Remove duplicates
clean_trip_final <- distinct(clean_trip_final)

# FIX: Now that started_at and ended_at are POSIXct, this comparison is chronological
# Remove data where start time is greater than end time (invalid trips)
clean_trip_final <- clean_trip_final %>%
  filter(started_at < ended_at)

# Remove empty columns/rows using janitor
clean_trip_final <- remove_empty(clean_trip_final, which = c("rows", "cols"))

# Check Cleaned data for NAs again
colSums(is.na(clean_trip_final))


# =============================================================================
# 4. FEATURE ENGINEERING AND FINAL FILTERING
# =============================================================================

# Renaming column for better context
clean_trip_final <- rename(clean_trip_final, costumer_type = member_casual, bike_type = rideable_type)

# Add ride length column (CORRECT: Now uses POSIXct objects)
clean_trip_final$ride_length <- difftime(clean_trip_final$ended_at, clean_trip_final$started_at, units = "mins")

# Separate date/time elements
clean_trip_final <- clean_trip_final %>%
  mutate(
    date = as.Date(started_at),
    week_day = format(date, "%A"),
    month = format(date, "%b_%y"),
    year = format(date, "%Y"),
    time = format(started_at, format = "%H:%M") # Time of day
  )

# Remove stolen bikes or accidental start/stop (outlier removal)
# Retaining only trips between 5 minutes and 1440 minutes (24 hours)
# NOTE: The original code used 5 (min) and 1440 (max), which is appropriate.
clean_trip_final <- clean_trip_final %>%
  filter(ride_length >= 5 & ride_length <= 1440)


# Select the final columns we want to use
clean_trip_final <- clean_trip_final %>%
  select(bike_type, costumer_type, month, year, time, started_at, week_day, ride_length)

# Save the cleaned data
write.csv(clean_trip_final, file = "clean_trip_final.csv", row.names = FALSE)

# Preview the final data
View(head(clean_trip_final))
