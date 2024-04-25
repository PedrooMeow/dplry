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
corrplot(corr_matrix, method ="color")
#Scatter plot
pairs(mtcars[1:4])
corr_matrix <- cor(mtcars)
heatmap(corr_matrix, xlab = "Features", ylab = "Features", main = "Heatmap of the correlation matrix for mtcars")
#The p value being less than 0.05 makes it asignificant feature. This can be done for other features as well.
t.test(mtcars['mpg'], mu = 0)
```
Lab 4 (Anova and Forecasting)
```{r}
PlantGrowth
str(PlantGrowth)
onewayanova<-aov(weight~group,data=PlantGrowth)
onewayanova
PlantGrowth$Fertilizer<-factor(rep(c('A','B'),each=15))
PlantGrowth
twowayanova<-aov(weight~group+Fertilizer+group:Fertilizer,data=PlantGrowth)
twowayanova
                        
#The results of the one-way ANOVA analysis for the 'group' factor indicate that there is significant variability in the weight of the samples across different groups. The sum of squares for the 'group' factor is 3.76634, suggesting that a considerable portion of the total variation in weight can be attributed to the differences between groups. However, there is still a substantial amount of unexplained variability, as evidenced by the residual sum of squares of 10.49209. The residual standard error, which measures the average deviation of observed values from the fitted values, is relatively low at 0.6233746, indicating a reasonably good fit of the model to the data.

#Moving on to the two-way ANOVA analysis incorporating both the 'group' and 'fertilizer' factors, it becomes evident that both factors and their interaction play a role in explaining the variability in weight. The sum of squares for 'group' remains the same as in the one-way ANOVA analysis, indicating its continued significance. Additionally, the sum of squares for 'fertilizer' is 0.08281, suggesting that this factor also contributes to explaining the variation in weight, albeit to a lesser extent. The interaction between 'group' and 'fertilizer' is not estimable for two of the six effects, possibly due to insufficient data or collinearity issues. Despite the inclusion of the additional factor and interaction, there remains unexplained variability, with a residual sum of squares of 10.40928. However, the residual standard error remains relatively low at 0.6327376, indicating a satisfactory fit of the two-way model to the data. These results collectively provide insights into the factors influencing weight and the overall goodness of fit of the ANOVA models.
set.seed(123)
ts_data<-ts(rnorm(100),start=c(2020,1),frequency=12)
plot(ts_data)
arima_model<-auto.arima(ts_data)
forecast_values<-forecast(arima_model,h=12)
plot(forecast_values)
scores<-data.frame(first_year=c(82,93,71,64,69,53),second_year=c(62,85,94,78,71,66),third_year=c(64,73,87,91,56,78))
str(scores)
onewayanovascores<-aov(c(first_year,second_year,third_year)~rep(c("First","Second","Third")),each=6,data=scores)
summary(onewayanovascores)
```
Lab 5
```{r}
year <- 1:12
sales <- c(5.2, 4.9, 5.5, 4.9, 5.2, 5.7, 5.4, 5.8, 5.9, 6, 5.2, 4.8)
df <- data.frame(year = year, sales = sales)
print(df)
#3 year simple moving average forecast
df$sma_forecast <- c(NA, NA, (df$sales[1] + df$sales[2] + df$sales[3]) / 3)
for (i in 4:nrow(df)) {
df$sma_forecast[i] <- mean(df$sales[(i - 2):i])
}
# Calculate weighted moving average forecast with weights 1, 2, 1
weights <- c(1, 2, 1)
df$wma_forecast <- c(NA, NA, sum(weights * df$sales[1:3]) / sum(weights))
for (i in 4:nrow(df)) {
df$wma_forecast[i] <- sum(weights * df$sales[(i - 2):i]) / sum(weights)
}
#RMSE and MAPE for both forecasts
actual <- df$sales[4:nrow(df)]
rmse_sma <- sqrt(mean((df$sma_forecast[4:nrow(df)] - actual)^2))
mape_sma <- mean(abs((df$sma_forecast[4:nrow(df)] - actual) / actual)) * 100
rmse_wma <- sqrt(mean((df$wma_forecast[4:nrow(df)] - actual)^2))
mape_wma <- mean(abs((df$wma_forecast[4:nrow(df)] - actual) / actual)) * 100
cat("RMSE for 3-year Simple Moving Average Forecast:", rmse_sma, "\n")
cat("MAPE for 3-year Simple Moving Average Forecast:", mape_sma, "%\n\n")
cat("RMSE for Weighted Moving Average Forecast (weights 1, 2, 1):", rmse_wma, "\n")
cat("MAPE for Weighted Moving Average Forecast (weights 1, 2, 1):", mape_wma, "%\n")
#The 3-year Simple Moving Average Forecast gives an RMSE of 0.305707 and a MAPE of 4.550868%. The Weighted Moving
#Average Forecast with weights 1, 2, 1 resulted in an RMSE of 0.3299832 and a MAPE of 5.255309%. Lower RMSE and MAPE values indicate
#better accuracy. Thus, the 3-year Simple Moving Average Forecast gives slightly higher accuracy compared to the Weighted Moving Average
#Forecast in predicting the sales data.
data("lynx")
lynx_ts <- ts(lynx, frequency = 1, start = c(1821))
plot(lynx_ts, main = "Annual Numbers of Lynx Trapped")
fit <- auto.arima(lynx_ts)
forecast_values <- forecast(fit, h = 10)
plot(forecast_values, main = "Forecasted Annual Numbers of Lynx Trapped")
```

Lab 6
```{r}
data(iris)
#Splitting dataset to train and test (80/20) basis
set.seed(123) 
train_index <- sample(1:nrow(iris), 0.8 * nrow(iris)) 
train_data <- iris[train_index, ]
test_data <- iris[-train_index, ] 
model <- naiveBayes(Species ~ ., data = train_data)
model
predictions <- predict(model, newdata = test_data)
predictions
table(predictions, test_data$Species)
accuracy <- sum(diag(table(predictions, test_data$Species))) / nrow(test_data)
accuracy

```

```{r}
data <- data(mtcars)
#Split the data into training and testing sets (80:20 ratio)
set.seed(123)
train_index <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]
#Build the decision tree using Information Gain as the feature selection
 measuretree <- rpart(Personal.Loan ~ ., data = train_data, method = "class", parms = list(split = "information"))
#Plot the decision tree
plot(tree)
text(tree, use.n = TRUE, cex = 0.8)
# Predict the outcome on the testing set
pred <- predict(tree, test_data, type = "class")
# Calculate the accuracy and confusion matrix
accuracy <- mean(pred == test_data$Personal.Loan)
conf_mat <- table(pred, test_data$Personal.Loan)
# Print the results
print(paste("Accuracy:", accuracy))
print(conf_mat)
#USING GINI
tree_gain <- rpart(Personal.Loan ~ ., data = train_data, method = "class", parms = list(split = "gini"))
pred_gain <- predict(tree_gain, test_data, type = "class")
# Calculate the accuracy and confusion matrix
accuracy_gain <- mean(pred_gain == test_data$Personal.Loan)
conf_mat_gain <- table(pred_gain, test_data$Personal.Loan)
# Print the results
print(paste("Accuracy using Gain Ratio:", accuracy_gain))
print(conf_mat_gain)



```

Lab 7
```{r}
#Assume df is the dataset

df$status<-ifelse(df1$status=="Not Placed",1,0)
table(df$status)
set.seed(45)
sample<-sample.split(df$status, SplitRatio = =0.80)
train.lr=subset(df,sample==TRUE)
test.lr=subset(df,sample=FALSE)
prop.table(table(train.lr$status))l
prop.table(table(test.lr$status))

glm_model<-glm(status~degree,family=binomial,data=train.lr)
summary(glm_model)

lr.pred<-predict(glm_model,newdata=test.lr,type="response")
head(lr.pred)
head(test.lr$status)
lr.pred.class<-ifelse(lr.pred>=0.5,1,0)
head(lr.pred.class)
mean((test.lr$status==lr.pred.class)*100)#<- logistic regression accuracy
plot(glm_model)
#RandomForest
#Assume datasett called redwine
summary(redwine)
plot(redwine)
corrplot(cor(redwine))
redwineRD<-randomForest(factor(good.wine)~.-quality,data=redwine,ntree=150)
redwineRD

```

Lab 8 (Linear SVM)
```{r}
#Assume the dataset is df
clean_data<-na.omit(df)
sum(duplicated(clean_data))
clean_data$DEATH_EVENT<-factor(clean_data$DEATH_EVENT)
glimpse(clean_data)
summary(clean_data)
set.seed(123)
train_index<-createDataPartition(clean_data$DEATH_EVENT,p=0.8,list=FALSE)
train_data<-clean_data[train_index,]
test_data<-clean_data[-train_index,]
test_data1<-test_data[1:20, ]
test_data2<-test_data[21:40, ]
test_data3<-test_data[41:59, ]
#RBF kernel
svm_rbf<-svm(DEATH_EVENT~.,data=train_data,kernel="radial")
predictions_rbf<-predict(svm_rbf,newdata=test_data1)
accuracy_rbf<-mean(predictions_rbf==test_data1$DEATH_EVENT)
print(paste("Accuracy of RBF Kernel: ",accuracy_rbf*100))
#similarly do for test data 2 and 3
#Sigmoid kernel
svm_rbf<-svm(DEATH_EVENT~.,data=train_data,kernel="sigmoid")
predictions_rbf<-predict(svm_rbf,newdata=test_data1)
accuracy_rbf<-mean(predictions_rbf==test_data1$DEATH_EVENT)
print(paste("Accuracy of RBF Kernel: ",accuracy_rbf*100))


```

Lab 9
```{r}
#Assume data is df
data.num<-select_if(df,is.numeric)
data.num.cols<-ncol(df.num)
cluster.df<-data.num
x<-scale(cluster.df)
#Elbow point
set.seed(6)
wcss<-vector()
for (i in 1:10) wcss[i]<-sum(kmeans(x,i)$withinss)
plot(1:10,wcss,type="b",main=paste('cluster of countries'),xlab="Number of Clusters",ylab="WCSS")
#Kmeans
set.seed(123)
final<-kmeans(x,centers=3,nstart=25)
print(final)
clusplot(x,final$cluster,lines=0,shade=TRUE,color=TRUE,labels=2,plotchar=FALSE,span=TRUE,
         main=paste('Cluster of countries 3 clusters'),)
#KMedoids
kmedo<-pam(x,k=3,metric="euclidean",stand=FALSE)
cluster_assignments<-kmedo$clustering
medoid_indices<-kmedo$medoids
sw<-silhouette(cluster_assignments,dist(x))
summary(kmedo)
clusplot(x,kmedo$cluster,lines=0,shade=TRUE,color=TRUE,labels=2,plotchar=FALSE,span=TRUE,
         main=paste('Cluster of countries 3 clusters'),)
```

Lab 10
```{r}
 # Define the function
f <- function(x) { 
x1 <- x[1]
 x2 <- x[2]
 return(4 * x1^2 + 3 * x1 * x2 + 2.5 * x2^2 - 5.5 * x1 - 4 * x2)}
# Define the gradient function
grad_f <- function(x) { 
x1 <- x[1] 
x2 <- x[2]
 return(c(8 * x1 + 3 * x2 - 5.5, 3 * x1 + 5 * x2 - 4))}
# Initialize the starting point
x0 <- c(0, 0)
# Perform gradient descent using the optim() function
result <- optim(x0, f, gr = grad_f, method = "BFGS")
# Print the results
cat("Minimum value:", result$value, "\n")
optimized_X1 <- result$par[1]
optimized_X2 <- result$par[2]
# Print the results
cat("Optimized X1:", optimized_X1, "\n")
cat("Optimized X2:", optimized_X2, "\n")

#Now trying Gradient Descent using Negative log likelihood 
#Applying gradient descent approach to minimize the given function using built-in optim() function.
my_function <- function(params) { 
X1 <- params[1] 
X2 <- params[2] 
result <- 4 * X1^2 + 3 * X1 * X2 + 2.5 * X2^2 - 5.5 * X1 - 4 * X2 
return(result)}
# Negative log-likelihood (to minimize)
log_likelihood <- function(params) { 
return(-my_function(params))}
#Setting initial parameter values.
initial_params <- c(0, 0) 
# Using the optim function()
result <- optim(par = initial_params, fn = log_likelihood, method = "BFGS")
# Accessing the optimized parameter values
optimized_X1 <- result$par[1]
optimized_X2 <- result$par[2]
# Print the results
cat("Minimum value:", result$value, "\n")
cat("Optimal solution:", result$par, "\n")
cat("Optimized X1:", optimized_X1, "\n")
cat("Optimized X2:", optimized_X2, "\n")


")
}

