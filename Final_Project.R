# Set working directory
setwd("~/Documents/Year 2 Fall 2024/STP 530/Project/stp_530_repo")

# Read data
cab_df <- read.csv("cab_rides.csv")
weather <- read.csv("weather.csv")


# ---------- MERGING THE CAB RIDES AND WEATHER DATA ---------- #



# Load necessary libraries
library(dplyr)
library(lubridate)
library(tidyr)

# Step 1: Convert timestamps to datetime
cab_df$date_time <- as_datetime(cab_df$time_stamp / 1000, origin = "1970-01-01")
weather$date_time <- as_datetime(weather$time_stamp, origin = "1970-01-01")

# Step 2: Create merge_date column with consistent formatting
cab_df <- cab_df %>%
  mutate(
    merge_date = paste0(source, " - ", format(date_time, "%Y-%m-%d"), " - ", sprintf("%02d", hour(date_time)))
  )

weather <- weather %>%
  mutate(
    merge_date = paste0(location, " - ", format(date_time, "%Y-%m-%d"), " - ", sprintf("%02d", hour(date_time)))
  )

# Step 3: Group by 'merge_date' with numeric-only averaging, and replace NA in 'rain' with 0
groupby_value <- weather %>%
  group_by(merge_date) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  mutate(rain = ifelse(is.na(rain), 0, rain))

# Step 4: Merge without dropping NA values and inspect
merged_df <- cab_df %>%
  left_join(groupby_value, by = "merge_date", suffix = c("", "_w"))

# Inspect NA patterns in the merged dataframe
print("Summary of NA values in merged_df after fix:")
print(colSums(is.na(merged_df)))

# Display the first few rows, including rows with NAs
print("Merged dataframe (first 10 rows):")
# print(head(merged_df, 10))
# View(merged_df)




# ---------- DROP NA (missing) values ----------- #


# Number of missing values
print("Total number of missing values:")
sum(is.na(merged_df))

# Columns with missing values and their counts
print("Count of missing values by column:")
missinf_values_df <- data.frame(colSums(is.na(merged_df))) # Keep columns with mssing values in data frame
head(missinf_values_df) # View the first few rows


# Remove all missing values
cleaned_merged_data <- drop_na(merged_df)
head(cleaned_merged_data)

# Confirm all missing values are removed
print("Total number of missing values in cleaned merged data:")
sum(is.na(cleaned_merged_data))


# ----------- SELECT DATA WITH ONLY UBERXL IN IT ---------- #

?subset

# Removing cab type, id, product id, time stamp w

UberXL_data <- subset(cleaned_merged_data, name == "UberXL", select = c(-2, -8, -9, -17))
head(UberXL_data)


# ----------- GROUPING DATE AND TIMES INTO TIME CATEGORIES ---------- #


# Calculate number of days between each timestamps and our reference timestamp (1543104000000)
num_day <- (UberXL_data$time_stamp-1543104000000)/(1000*60*60*24) 

#Calculate day when data was queried (EST)
day <- ceiling(num_day %% 7)
day_word <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")[day]

#Calculate hours since midnight within the day
hours <- 24*(num_day-floor(num_day))
time_category <- c() #vector to store the time categories
v1 <- day_word %in% c("Fri", "Sat", "Sun") & ((hours >= 21 & hours <= 24) |  hours <= 3) 
v2 <- day_word %in% c("Mon", "Tue", "Wed", "Thu", "Fri") & (hours >= 6 & hours <= 9)       
v3 <- day_word %in% c("Mon", "Tue", "Wed", "Thu", "Fri") & (hours >= 16 & hours <= 19)    
v4 <- !v1 & !v2 & !v3      
time_category[v1] <- "Weekend_Nights" 
time_category[v2] <- "Morning_Commute" 
time_category[v3] <- "Evening_Commute" 
time_category[v4] <- "Other_Times"

# Append time category
time_cat_data <- cbind(UberXL_data, time_category)

# Cab prices now has a new column of the time categories
View(time_cat_data)







