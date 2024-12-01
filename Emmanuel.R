# Set working directory
setwd("~/Documents/Year 2 Fall 2024/STP 530/Project/stp_530_repo")

install.packages("freqparcoord")
library(freqparcoord)
data("prgeng", package = "freqparcoord")
help(prgeng)
#Check the data set
head(prgeng)     
str(prgeng)       
summary(prgeng)
######
help(prgeng, package = "freqparcoord")
data(prgeng)

View(prgeng)
# Examine the histogram of each variable to check the distribution and outliers,

#remove rows with missing values of wageinc, wkswrkd and agenote that in this project we want to include only employed people 
prgeng1 <- prgeng[prgeng$wageinc != 0, ]
prgeng2 <- prgeng1[prgeng1$wkswrkd != 0, ]
prgeng3 <- prgeng2[prgeng1$age != 0, ]
prgeng4 <- prgeng3[prgeng1$powspuma != 0, ]
prgeng5 <- prgeng4[prgeng1$age > 16, ]
prgeng6 <- prgeng4[prgeng1$occ != 0, ]
prgeng5[!complete.cases(prgeng5), ]

sum(is.na(prgeng5$wageinc))

# Remove rows with NA in specific column (e.g., wageinc)
sum(is.na(prgeng5$wageinc)) # Count number of missing values
sum(is.na(prgeng5$wkswrkd))
sum(is.na(prgeng5$age))
sum(is.na(prgeng5$powspuma))
sum(is.na(prgeng5$occ))


formatted_data <- prgeng5[!is.na(prgeng5$wageinc), ]
head(formatted_data)
summary(formatted_data)
View(formatted_data)

# ------ Descriptives
install.packages("psych")


library(psych)
describe(formatted_data)


# --------- AGE --------------------- #

boxplot.stats(formatted_data$age)$out # Print out all the outliers
boxplot(formatted_data$age) # Show boxplot of outliers

# -- log transform

formatted_data$log.Age <- log(formatted_data$age)

par(mfrow=c(1,2)) # Show two tables side by side

hist(formatted_data$age,
     main = "Histogram of Age",
     xlab = "Age",
     col = "red")


hist(formatted_data$log.Age,
     main = " Tranformed Histogram of Age",
     xlab = "Age",
     col = "red")

boxplot(formatted_data$age, main = "Age") # Untransformed
boxplot(formatted_data$log.Age, main = "Tranformed Age") # Transformed

# par(mfrow=c(1,2))
qqnorm(formatted_data$age, main = "Age")
qqline(formatted_data$age, col = 2)

qqnorm(formatted_data$log.Age, main = "Age")
qqline(formatted_data$log.Age, col = 2)

hist(formatted_data$age)
hist(formatted_data$log.Age)

# ----- TAKE NOTE !!!!!
# IF I RUN THE MODEL WITH LOG OF AGE AND WAGE OUTLIERS NOT REMOVED, THE MODEL IS REALLY BAD
# GO TO WAGEINC SECTION AND SEE HOW MUCH REMOVING WAGE OUTLIERS IMPORVED THE MODEL
# THE CODE BELOW SHOWS WHY NOT REMOVING WAGE OUTLIERS MAKES THE MODEL BAD
# SO WE WILL NOT USE THE CODE BELOW !!!!
model_log_Age <- lm(wageinc ~ log.Age + cit + educ + engl + occ + birth + sex + wkswrkd + yrentry + powspuma, data = formatted_data)
plot(fitted(model_log_Age), rstudent(model_log_Age))
abline(h=0, col="red")

qqnorm(residuals(model_log_Age))
qqline(residuals(model_log_Age), col = "red")
plot(fitted(model_log_Age), rstudent(model_log_Age))
abline(h=0, col="red")

# I FEEL OK WITH LOG OF AGE, EVEN THOUGH IT LOOKS NOT NORMAL ON QQ PLOT
# SOMETIMES NORMAL DATA APPEARED AS THOUGH THEY ARE NOT
# THE DEVIATIONS ARE NEGLIGIBLE
# LOG TRANSFORMATION IS BETTER THAN REMOVING OUTLIERS AS THE AGES UP TO 
# 93 IS PLAUSIBLE AND AN INTERESTING AGE TO NOTE
# WHEN I REMOVED HIGH AGES, IT DID NOT IMPROVE IT

# --------- WAGEINC --------------------- #

hist(formatted_data$wageinc)

View(formatted_data)

# Load required packages (if needed)
library(psych) # For the `describe()` function

# Step 1: Print all outliers in wageinc
outliers <- boxplot.stats(formatted_data$wageinc)$out
cat("Outliers in wageinc:", outliers, "\n")

# Step 2: Visualize wageinc data with outliers
boxplot(formatted_data$wageinc, main = "Boxplot of wageinc with Outliers", col = "red")

# Step 3: Describe wageinc data
describe(formatted_data$wageinc)

# Step 4: Identify rows with outliers
outlier_indices <- which(formatted_data$wageinc %in% outliers)

# Print rows with outliers
outlier_data <- formatted_data[outlier_indices, ]
cat("Number of outliers found:", nrow(outlier_data), "\n")
print(outlier_data)

# Step 5: Create a new dataset without outliers
wage_data_no_outliers <- formatted_data[-outlier_indices, ]

# Step 6: Verify that the new dataset does not contain the outliers
boxplot(wage_data_no_outliers$wageinc, main = "Boxplot of wageinc Without Outliers", col = "blue")

# Check if any outliers remain
remaining_outliers <- boxplot.stats(wage_data_no_outliers$wageinc)$out
cat("Remaining outliers:", remaining_outliers, "\n")

# Final print to confirm the number of remaining outliers
if (length(remaining_outliers) == 0) {
  cat("All outliers successfully removed.\n")
} else {
  cat("Outliers still exist in the dataset.\n")
}


# Function to iteratively remove outliers
remove_outliers <- function(data, column) {
  repeat {
    # Identify current outliers
    outliers <- boxplot.stats(data[[column]])$out
    
    # Break the loop if no outliers remain
    if (length(outliers) == 0) break
    
    # Remove rows with current outliers
    outlier_indices <- which(data[[column]] %in% outliers)
    data <- data[-outlier_indices, ]
  }
  return(data)
}

# Apply the function to remove all outliers from wageinc
wage_data_no_outliers <- remove_outliers(formatted_data, "wageinc")

# Verify the final dataset
boxplot(wage_data_no_outliers$wageinc, main = "Boxplot Without Outliers (Final)", col = "green")
remaining_outliers <- boxplot.stats(wage_data_no_outliers$wageinc)$out
cat("Remaining outliers:", remaining_outliers, "\n")

# Final confirmation
if (length(remaining_outliers) == 0) {
  cat("All outliers successfully removed.\n")
} else {
  cat("Outliers still exist in the dataset.\n")
}

boxplot(wage_data_no_outliers$wageinc)


# par(mfrow=c(1,2))
qqnorm(formatted_data$wageinc, main = "Wage")
qqline(formatted_data$wageinc, col = 2)

qqnorm(wage_data_no_outliers$wageinc, main = "No outlier Wage")
qqline(wage_data_no_outliers$wageinc, col = 2)

hist(wage_data_no_outliers$wageinc)

hist(formatted_data$wageinc)



# ---- Build model with outliers removed in wage

model_wage_no_outlier <- lm(wageinc ~ log.Age + cit + educ + engl + occ + birth + sex + wkswrkd + yrentry + powspuma, data = wage_data_no_outliers)
plot(fitted(model_wage_no_outlier), rstudent(model_wage_no_outlier))
abline(h=0, col="red")

summary(model_wage_no_outlier)


#### DIAGNOSTIC ------ #

# 1. Check whether the relationship between wageinc and each predictor is linear
plot(wage_data_no_outliers$wageinc, residuals(model_wage_no_outlier))
plot(wage_data_no_outliers$log.Age, residuals(model_wage_no_outlier))
plot(wage_data_no_outliers$cit, residuals(model_wage_no_outlier))
plot(wage_data_no_outliers$educ, residuals(model_wage_no_outlier))
plot(wage_data_no_outliers$engl, residuals(model_wage_no_outlier))
plot(wage_data_no_outliers$occ, residuals(model_wage_no_outlier))
plot(wage_data_no_outliers$birth, residuals(model_wage_no_outlier))
plot(wage_data_no_outliers$sex, residuals(model_wage_no_outlier))
plot(wage_data_no_outliers$wkswrkd, residuals(model_wage_no_outlier))
plot(wage_data_no_outliers$yrentry, residuals(model_wage_no_outlier))
plot(wage_data_no_outliers$powspuma, residuals(model_wage_no_outlier))

qqnorm(residuals(model_wage_no_outlier))
qqline(residuals(model_wage_no_outlier), col = "red") # NICE !!!!


# 2. Check for outliers

plot(fitted(model_wage_no_outlier), rstudent(model_wage_no_outlier))
abline(h=0, col="red")

# 3. Check heteroskedasticity

plot(predict(model_wage_no_outlier), residuals(model_wage_no_outlier))


# 4. Check whether the residuals are normally distributed

qqnorm(residuals(model_wage_no_outlier))
qqline(residuals(model_wage_no_outlier), col = "red") # NICE !!!!

# 5. Independent errors

# Check problem description and data collection method. How was the data collected?
#


# I WILL GO WITH WAGE DATA WITH NO OUTLIER BUT WILL NOT DO LOG TRANSFORMATION
# BECAUSE IT WORSENS IT
# BUT WE NEED TO REPORT THE SPECIFIC OUTLIERS WE REMOVED AND WHY? THIS IS ACCEPTABLE!
# WE SHOULD CREATE A SEPARATE DOCUMENTATION OF WHY WE REMOVED THE WAGE OUTLIERS



# ------------ WKSWRKD --------------#


boxplot.stats(wage_data_no_outliers$wkswrkd)$out # Print out all the outliers
boxplot(wage_data_no_outliers$wkswrkd) # Show boxplot of outliers

# -- log transform

wage_data_no_outliers$log.wkswrkd <- log(wage_data_no_outliers$wkswrkd)

par(mfrow=c(1,2)) # Show two tables side by side

hist(wage_data_no_outliers$wkswrkd,
     main = "Histogram of Weeks worked",
     xlab = "Weeks worked",
     col = "red")


hist(wage_data_no_outliers$log.wkswrkd,
     main = " Tranformed Histogram of Weeks worked",
     xlab = "Weeks worked",
     col = "red")

boxplot(wage_data_no_outliers$wkswrkd, main = "Weeks worked") # Untransformed
boxplot(wage_data_no_outliers$log.wkswrkd, main = "Tranformed Weeks worked") # Transformed

# par(mfrow=c(1,2))
qqnorm(wage_data_no_outliers$wkswrkd, main = "Weeks worked")
qqline(wage_data_no_outliers$wkswrkd, col = 2)

qqnorm(wage_data_no_outliers$log.wkswrkd, main = "Weeks worked")
qqline(wage_data_no_outliers$log.wkswrkd, col = 2)

hist(wage_data_no_outliers$wkswrkd)
hist(wage_data_no_outliers$log.wkswrkd)

# LOG TRANSFORMATION OF WEEKS WORKED DOES NOT IMPROVE THE DISTRIBUTION
# SO, I WILL JUST LEAVE IT IN THE RAW FORM
# WE ARE OK WITH IT SINCE THE MODEL IN WAGEINC SEEM TO LOOK GOOD


##### ------ CHOOSING MODEL AND PREDICTORS ------------

wage_data_no_outliers$sex.factor <- factor(wage_data_no_outliers$sex, levels = c(1, 2), labels = c("Male", "Female"))
wage_data_no_outliers$engl.factor <- factor(wage_data_no_outliers$engl, levels = c(0,10,11,20,21,30,31,40,41), labels = c("A", "B","C","D","E","F","G","H","I"))

head(wage_data_no_outliers) # View the data to see


# Now, to check for multicollinearity, down below
# Subset the data to exclude factor variables
numeric_data <- wage_data_no_outliers[sapply(wage_data_no_outliers, is.numeric)]

# Calculate the Pearson correlation matrix
cor_matrix <- cor(numeric_data, method = "pearson")

# View the correlation matrix
print(cor_matrix)

# From the correlation matrix, the following correlate:
# cit and (engl and birth, yrentry)
# engl and (cit and birth, yrentry)
# birth and (cit and engl, yrentry)
# yrentry and (cit and engl and birth)

summary(model_wage_no_outlier)

vif(model_wage_no_outlier)

# Multicollinearity only affects the specific predictors that are correlated
# The mulicollinearity observed are non-extreme
# However, since we need to understand the role of each predictor variable
# We need to resolve the multicollinearity
# Check page 22 of LECTURE 6 slide

# So, lets remove yrentry since it appears in all the cor above
# we will also remove birth because it has VERY high corelation with cit
# Moreover, the 2 of them are not statistically significant

removed_multicoll_model <- lm(wageinc ~ log.Age + cit + educ + engl + occ + sex + wkswrkd + powspuma, data = wage_data_no_outliers)

summary(removed_multicoll_model)
vif(removed_multicoll_model)

# This VIF is much better
# We use the model without yrentry and birth





