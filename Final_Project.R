# Set working directory
setwd("~/Documents/Year 2 Fall 2024/STP 530/Project")
a <- 4
c <- 44

cab_rides <- read.csv("cab_rides.csv")
my_cab_rides <- cab_rides[cab_rides$name == "UberXL", c(-8, -9)]

my_cab_rides

head(my_cab_rides)
row_count <- nrow(my_cab_rides)
print(paste("Number of rows in the data:", row_count))

total_missing <- sum(is.na(my_cab_rides))
print(paste("Total missing values in the dataset:", total_missing))

# Step 3 (optional): Count missing values for each column
missing_by_column <- colSums(is.na(my_cab_rides))
print("Missing values by column:")
print(missing_by_column)