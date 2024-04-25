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

LAB E1

```{r libraries}
library(ggplot2)
library(Metrics)
library(corrplot)
library(forecast)
library(e1071)
library(rpart)
library(dplyr)
library(ggthemes)
library(reshape2)
library(randomForest)
library(tidyverse)
library(caret)
library(cluster)
```

Lab 1

```{r}
df<-data.frame(X=c(2,3,4,5,6),Y=c(12,17,23,28,32))
df
lr_graph<-ggplot(df,aes(x=X,y=Y))+geom_point()+xlab('X axis')+ylab('Y axis')
lr_graph
lr_graph1=lr_graph+geom_smooth(method='lm',color='red')
lr_graph1
lmm=lm(formula=Y~X,data=df)
lmm
p=summary(lmm)
print(p)

print(p$resid)

R2<-p$r.squared
#Adjusted R squared
n <- nrow(df)
#k <- length(lmm$coefficients) – 1
#adj_R2 <- 1 - ((1 - R2) * (n - 1) / (n - k - 1))
#adj_R2
prediction<-predict(lmm)
prediction
print(mae(df$Y,prediction))
print(mse(df$Y,prediction))
print(rmse(df$Y,prediction))
rss<-sum(resid(lmm)^2)
rss
tss<-rss/(1-R2)
tss
sse <- sum((fitted(lmm) - df$y)^2)
sse
se<-sqrt(diag(vcov(lmm)))
se
#Null and Laternate Hypothesis The null hypothesis for this question is that the coefficient of age is equal tozero, which means there is no statistically significant relationship between age and the height.
#The alternative hypothesis states that the coefficient of the age is not equal to zero, which means there is astatistically significant relationship between the age and height.
#The coefficients of the model indicate that the intercept is 64.9283 and the slope is 0.6350. The interceptrepresents the height when independent variable age is zero. The slope represents the change in height forevery one unit increase in age. Both the variable values are less than 0.05 meaning that both variables arestatistically significant. The F-statistic for the model is 880 with a corresponding p-value of 4.428e-11. The F-statistic tests the null hypothesis that all of the regression coefficients are equal to zero. The multiple R-squaredvalue of the model is 0.9888, which indicates that the model is good fit.
#The residual standard error of the model is 0.256, which represents the average distance that the observedvalues fall from the regression line.
#The linear regression analysis suggests that there is a strong positive relationship between age and height. Asage increases, height also increases.
```
Lab 3
```{r}
df<-data(mtcars)
head(mtcars)
cor(mtcars)
cov(mtcars)
corr_matrix <- cor(mtcars)
#Scatter plot
pairs(mtcars[1:4])
corr_matrix <- cor(mtcars) Numbers of Lynx Trapped")
```
}
