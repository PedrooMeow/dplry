# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {

    cat("---
title: \"EDA_LAB3_20MIA1008\"
output: html_document
date: \"2024-01-18\"
---


X <- c(2, 3, 4, 5, 6)
Y <- c(12, 17, 23, 28, 32)

# Calculate the means of X and Y
mean_X <- mean(X)
mean_Y <- mean(Y)

# Calculate the slope (a) and intercept (b)
a <- sum((X - mean_X) * (Y - mean_Y)) / sum((X - mean_X)^2)
b <- mean_Y - a * mean_X

# Print the coefficients
cat(\"The best linear fit is Y =\", a, \"X +\", b)



# Plot the data points
plot(X, Y, main = \"Linear Regression\", xlab = \"X\", ylab = \"Y\")

# Add the best fit line
abline(a = a, b = b, col = \"red\")




# Calculate the predicted values
predicted_Y <- a * X + b

# Calculate the residuals
residuals <- Y - predicted_Y

# Calculate the RSS
RSS <- sum(residuals^2)

# Print the RSS
cat(\"The RSS is\", RSS)




# Calculate the standard errors
se_a <- sqrt(RSS / (length(X) - 2) / sum((X - mean_X)^2))
se_b <- se_a * sqrt(1/length(X) + mean_X^2 / sum((X - mean_X)^2))

# Print the standard errors
cat(\"The standard error for a is\", se_a, \"and for b is\", se_b)





# Calculate the total sum of squares (TSS)
TSS <- sum((Y - mean(Y))^2)

# Calculate the explained sum of squares (ESS)
ESS <- sum((predicted_Y - mean(Y))^2)

# Calculate the R-squared value
RSquared <- ESS / TSS

# Calculate the adjusted R-squared value
n <- length(X)
p <- 2 # Number of predictors (a and b)
Adjusted_RSquared <- 1 - (1 - RSquared) * (n - 1) / (n - p - 1)



# Print the R-squared and adjusted R-squared values
cat(\"The R-squared value is\", RSquared, \"and the adjusted R-squared value is\", Adjusted_RSquared)

# Compute F-Statistic
f_statistic <- (RSquared / (1 - RSquared)) * ((n - 2) / 1)

cat(\" and F-Statistic:\", round(f_statistic, 3), \"\\n\")




# Perform hypothesis test
p_value <- 2 * (1 - pt(abs(a / se_a), df = length(X) - 2))

# Print the p-value
cat(\"The p-value for the hypothesis test is\", p_value, \"\\n\")

# Check significance level (commonly 0.05)
alpha <- 0.05

# Draw inferences
if (p_value < alpha) {
 cat(\"Reject the null hypothesis. There is a significant linear relationship between X and Y.\\n\")
} else {
 cat(\"Fail to reject the null hypothesis. There is no significant linear relationship between X and Y.\\n\")
}





#2. Apply linear regression analysis on any prominent dataset and state the inferences.
# importing dataset with the name HousingData
library(readr)
dataset <- read_csv(\"C:/Users/skp68/OneDrive/Documents/Datasets/HousingData.csv\")
View(dataset)







head(dataset)
summary(dataset)



missing_values <- sum(is.na(dataset))
cat(\"Missing values: \", missing_values)

colSums(is.na(dataset))



for (variable in colnames(dataset))
{
dataset[[variable]][is.na(dataset[[variable]])] <- median(dataset[[variable]],
                                                        na.rm = TRUE)
}

new_missing_values <- sum(is.na(dataset))
cat(\"Missing values after imputation: \", new_missing_values)



options(repos = \"https://cloud.r-project.org/\")



install.packages(\"ggplot2\")
install.packages(\"caret\")
# Plotting Data
library(ggplot2)
# Data splitting into training and test set
library(caret)



ggplot(dataset, aes(x = AGE, y = MEDV)) +
geom_point(color = \"blue\") +
labs(x = \"AGE\", y = \"MEDV (Median Home Value)\") +
ggtitle(\"Scatterplot of AGE vs. MEDV\")




library(ggcorrplot)
#Using ggcorrplot to get correlation between features
ggcorrplot(cor(dataset),hc.order = TRUE, lab = TRUE)




set.seed(123)

train_percentage <- 0.8

trainIndex <- createDataPartition(dataset$MEDV, p = train_percentage)

# Convert trainIndex to a numeric vector
trainIndex <- unlist(trainIndex)

training_data <- dataset[trainIndex, ]
testing_data <- dataset[-trainIndex, ]

\"Training data:\\n\"
head(training_data)
\"Test data:\\n\"
head(testing_data)





lm_model <- lm(MEDV ~ LSTAT + RM, data = training_data)




# Predicting on the test set
predictions <- predict(lm_model, newdata = testing_data)

# getting true value of test data
actual_values <- testing_data$MEDV # Actual values from the test set

# Calculating Mean Absolute Error (MAE)
mae <- mean(abs(predictions - actual_values))
cat(\"Mean Absolute Error (MAE):\", mae, \"\\n\")

# Calculating Mean Squared Error (MSE)
mse <- mean((predictions - actual_values)^2)
cat(\"Mean Squared Error (MSE):\", mse, \"\\n\")

# Calculating Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)
cat(\"Root Mean Squared Error (RMSE):\", rmse, \"\\n\")













summary(lm_model)
")
}

