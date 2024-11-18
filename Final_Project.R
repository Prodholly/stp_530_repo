# Set working directory
setwd("~/Documents/Year 2 Fall 2024/STP 530/Project/stp_530_repo")

# Read data
cab_df <- read.csv("cab_rides.csv")
weather <- read.csv("weather.csv")


# ---------- MERGING THE CAB RIDES AND WEATHER DATA WITH EXACT TIMESTAMP MATCH ---------- #

# Load necessary libraries
library(dplyr)
library(lubridate)

# Step 1: Convert timestamps to datetime
cab_df$date_time <- as_datetime(cab_df$time_stamp / 1000, origin = "1970-01-01")
weather$date_time <- as_datetime(weather$time_stamp, origin = "1970-01-01")

# Step 2: Create a consistent merge_date column for exact timestamp match
# Both datasets will have merge_date including date, hour, minute, and second
cab_df <- cab_df %>%
  mutate(
    merge_date = paste0(source, " - ", format(date_time, "%Y-%m-%d %H:%M:%S"))
  )

weather <- weather %>%
  mutate(
    merge_date = paste0(location, " - ", format(date_time, "%Y-%m-%d %H:%M:%S"))
  )

# Step 3: Aggregate weather data by exact merge_date (if needed)
# Summarize numeric columns to ensure no duplicates for a single merge_date
weather_grouped <- weather %>%
  group_by(merge_date) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") %>%
  mutate(rain = ifelse(is.na(rain), 0, rain))  # Ensure NA in rain is replaced with 0

# Step 4: Perform an exact join on the merge_date column
final_merged_df <- cab_df %>%
  inner_join(weather_grouped, by = "merge_date", suffix = c("", "_w"))

# Step 5: Inspect the final merged data
print("Summary of NA values in final_merged_df:")
print(colSums(is.na(final_merged_df)))

print("Final merged dataframe (first 10 rows):")
print(head(final_merged_df, 10))
View(final_merged_df)

# This merged data gives exactly data in both cab df and weather have EXACTLY the same hour, minute and seconds
# Hence, our merged data has 3,866 rows


# --------- Drop Missing Values ------------ #

# Number of missing values
print("Total number of missing values:")
sum(is.na(final_merged_df))

# Columns with missing values and their counts
print("Count of missing values by column:")
updated_missing_values_df <- data.frame(colSums(is.na(final_merged_df))) # Keep columns with mssing values in data frame
head(updated_missing_values_df) # View the first few rows

# Count rows with at least one NA
num_rows_with_na <- sum(apply(final_merged_df, 1, function(row) any(is.na(row))))

# Print the result
cat("Number of rows with at least one NA:", num_rows_with_na)

# Here, only the price column has missing values, and there 318 of them
# Price is the response variable, I will not suggest we do any imputation for it? I may consider it though!



# If we decide to remove all missing values
new_cleaned_merged_data <- drop_na(final_merged_df)
head(new_cleaned_merged_data)

# Confirm all missing values are removed
print("Total number of missing values in cleaned merged data:")
sum(is.na(new_cleaned_merged_data))

View(new_cleaned_merged_data)

# This data has no missing values again

# ----------- SELECT DATA WITH ONLY UBERXL IN IT ---------- #

# ?subset

# We need to pick data with UberXL alone

# Removing cab type, id, product id, time stamp w - because we are not interested in these columns as they will not provide much value

updated_UberXL_data <- subset(new_cleaned_merged_data, name == "UberXL", select = c(-2, -8, -9, -17))
head(updated_UberXL_data)



# ----------- GROUPING DATE AND TIMES INTO TIME CATEGORIES ---------- #


# Calculate number of days between each timestamps and our reference timestamp (1543104000000)
num_day <- (updated_UberXL_data$time_stamp-1543104000000)/(1000*60*60*24) 

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
updated_time_cat_data <- cbind(updated_UberXL_data, time_category)

# Cab prices now has a new column of the time categories
View(updated_time_cat_data)


# Export merged_df as a CSV file
write.csv(updated_time_cat_data, "time_cat_data.csv", row.names = FALSE)



# --------- OLD CODES BELOW; DO NOT RUN OR USE !!!!!!!!!!!!!!!! ---------------- #

# --------- OLD CODES BELOW; DO NOT RUN OR USE !!!!!!!!!!!!!!!! ---------------- #


# --------- OLD CODES BELOW; DO NOT RUN OR USE !!!!!!!!!!!!!!!! ---------------- #

# --------- OLD CODES BELOW; DO NOT RUN OR USE !!!!!!!!!!!!!!!! ---------------- #






# ---------- MERGING THE CAB RIDES AND WEATHER DATA ---------- #


# Load necessary libraries
library(dplyr)
library(lubridate)
library(tidyr)

# Step 1: Convert timestamps to datetime
cab_df$date_time <- as_datetime(cab_df$time_stamp / 1000, origin = "1970-01-01")
weather$date_time <- as_datetime(weather$time_stamp, origin = "1970-01-01")

# Step 2: Create merge_date column with consistent formatting (including seconds)
cab_df <- cab_df %>%
  mutate(
    merge_date = paste0(source, " - ", format(date_time, "%Y-%m-%d %H:%M:%S"))
  )

weather <- weather %>%
  mutate(
    merge_date = paste0(location, " - ", format(date_time, "%Y-%m-%d %H:%M:%S"))
  )

# Step 3: Group by 'merge_date' with numeric-only averaging, and replace NA in 'rain' with 0
groupby_value <- weather %>%
  group_by(merge_date) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  mutate(rain = ifelse(is.na(rain), 0, rain))

# Step 4: Merge without dropping NA values and inspect
updated_merged_df <- cab_df %>%
  left_join(groupby_value, by = "merge_date", suffix = c("", "_w"))

# Inspect NA patterns in the merged dataframe
print("Summary of NA values in merged_df after fix:")
print(colSums(is.na(updated_merged_df)))

# Display the first few rows, including rows with NAs
print("Merged dataframe (first 10 rows):")
print(head(updated_merged_df, 10))
View(updated_merged_df)


# --------- Drop Missing Values ------------ #

# Number of missing values
print("Total number of missing values:")
sum(is.na(updated_merged_df))

# Columns with missing values and their counts
print("Count of missing values by column:")
updated_missing_values_df <- data.frame(colSums(is.na(updated_merged_df))) # Keep columns with mssing values in data frame
head(updated_missing_values_df) # View the first few rows

# Count rows with at least one NA
num_rows_with_na <- sum(apply(updated_merged_df, 1, function(row) any(is.na(row))))

# Print the result
cat("Number of rows with at least one NA:", num_rows_with_na)

# So far, we have correct merged data. Recall we started with 693,071 rows in cab_df and 6,276 in weather data
# If we merge these data, we should have data LESS THAN 6276 because not all the data in weather will exactly match cab df, 
# especially since we are merging by hour, minute up to seconds
# So, we have Number of rows with at least one NA: 689523 which are mostly other data in cab df above 6276 



# Remove all missing values
updated_cleaned_merged_data <- drop_na(updated_merged_df)
head(updated_cleaned_merged_data)

# Confirm all missing values are removed
print("Total number of missing values in cleaned merged data:")
sum(is.na(updated_cleaned_merged_data))

View(updated_cleaned_merged_data)


# ----------- SELECT DATA WITH ONLY UBERXL IN IT ---------- #

?subset

# Removing cab type, id, product id, time stamp w and storing for only UberXL

updated_UberXL_data <- subset(updated_cleaned_merged_data, name == "UberXL", select = c(-2, -8, -9, -17))
head(updated_UberXL_data)






# ----------- GROUPING DATE AND TIMES INTO TIME CATEGORIES ---------- #


# Calculate number of days between each timestamps and our reference timestamp (1543104000000)
num_day <- (updated_UberXL_data$time_stamp-1543104000000)/(1000*60*60*24) 

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
updated_time_cat_data <- cbind(updated_UberXL_data, time_category)

# Cab prices now has a new column of the time categories
View(updated_time_cat_data)







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

# Removing cab type, id, product id, time stamp w and storing for only UberXL

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


# Export merged_df as a CSV file
write.csv(time_cat_data, "time_cat_data.csv", row.names = FALSE)




# --- TEST ---- #

# Identifying duplicate rows in time_data
library(dplyr)
duplicates <- time_cat_data[duplicated(time_cat_data), ]

View(duplicates)


# Removing duplicate rows, keeping the first occurrence
time_data_unique <- time_cat_data[!duplicated(time_cat_data), ]
View(time_data_unique)







