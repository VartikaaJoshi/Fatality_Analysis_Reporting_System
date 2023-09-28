
# Traffic Crash Analysis in R

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(lmtest)
library(car)

# Read the dataset
data <- read.csv('FARS_crash_level_compile.csv')

# Data Cleaning
data$nm_alcohol_drug_med[data$nm_alcohol_drug_med == 99] <- NA
data$nm_other_impair[data$nm_other_impair == 99] <- NA

# Fill missing values with median for numerical columns and mode for categorical ones
num_cols <- sapply(data, is.numeric)
data[num_cols] <- lapply(data[num_cols], function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))
cat_cols <- sapply(data, is.factor)
data[cat_cols] <- lapply(data[cat_cols], function(x) ifelse(is.na(x), as.numeric(names(sort(table(x), decreasing=TRUE)[1])), x))

# Data Visualization
ggplot(data, aes(x=year)) + geom_bar() + labs(title='Distribution of Crashes by Year')
ggplot(data, aes(x=state)) + geom_bar() + labs(title='Distribution of Crashes by State')
ggplot(data, aes(x=ve_total, y=fatals)) + geom_point(aes(color=fatals)) + labs(title='Relationship between Number of Vehicles Involved and Fatalities')

# Model Development
model <- lm(fatals ~ state + ve_total + ve_forms + county + city + day + month + year + hour + nhs + lgt_cond + weather + drunk_dr, data=data)
summary(model)

# Model Evaluation using Root Mean Squared Error (RMSE)
predictions <- predict(model, newdata=data)
rmse <- sqrt(mean((predictions - data$fatals)^2))
print(paste('Root Mean Squared Error:', rmse))

