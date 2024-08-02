# Loading mtcars data set from a mtcars.csv file
mtcars <- read.csv(file='mtcars.csv', header=TRUE, sep=",")

# Converting appropriate variables to factors  
mtcars2 <- within(mtcars, {
   vs <- factor(vs)
   am <- factor(am)
   cyl  <- factor(cyl)
   gear <- factor(gear)
   carb <- factor(carb)
})

# Variables and their type
print("Variables")
sapply(mtcars2, class)


# Print the data set
print("dataset")
mtcars2


# Print the first 10 rows
print("head")
head(mtcars2, 10)

# This prints the column names
print("names")
names(mtcars2)

# Another function that prints the column names
print("colnames")
colnames(mtcars2)

# Total number of columns in the data set
print("ncol")
ncol(mtcars2)

# Total number of rows in the data set
print("nrow")
nrow(mtcars2)

hist(mtcars2$mpg, 
     main="Histogram for Fuel Economy", 
     xlab="Miles Per Gallon", 
     border="blue", 
     col="green",
     xlim=c(5,40),
     ylim=c(0,5),
     las=1, 
     breaks=20)

plot(mtcars2$wt, mtcars$mpg, 
     main = "Scatterplot of Fuel Economy against Weight",
     xlab = "Weight", ylab = "Fuel Economy",
     xlim=c(0, 8),
     ylim=c(0, 50),
     col="red", 
     pch = 19, frame = FALSE)

# Selecting mpg, wt, and qsec variables to subset the data
myvars <- c("mpg","wt","qsec")
mtcars_subset <- mtcars2[myvars]

# Print the first 10 rows
print("head")
head(mtcars_subset, 10)

# Print the correlation matrix
print("cor")
corr_matrix <- cor(mtcars_subset, method = "pearson")
round(corr_matrix, 4)

# Create the multiple regression model and print the statistics
model <- lm(mpg ~ wt + qsec, data=mtcars_subset)
summary(model)

fitted_values <- fitted.values(model) 
fitted_values

residuals <- residuals(model)
residuals

plot(fitted_values, residuals, 
     main = "Residuals against Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals",
     col="red", 
     pch = 19)

qqnorm(residuals, pch = 19, col="red", frame = FALSE)
qqline(residuals, col = "blue", lwd = 2)

# confidence intervals for model parameters
print("confint")
conf_90_int <- confint(model, level=0.90) 
round(conf_90_int, 4)

newdata <- data.frame(wt=3.88, qsec=22.74)

print("prediction interval")
prediction_pred_int <- predict(model, newdata, interval="predict", level=0.90) 
round(prediction_pred_int, 4)

print("confidence interval")
prediction_conf_int <- predict(model, newdata, interval="confidence", level=0.90) 
round(prediction_conf_int, 4)

# Load the mtcars data set from a mtcars.csv file
mtcars <- read.csv(file='mtcars.csv', header=TRUE, sep=",")

# Convert the appropriate variables to factors  
mtcars2 <- within(mtcars, {
   vs <- factor(vs)
   am <- factor(am)
   cyl  <- factor(cyl)
   gear <- factor(gear)
   carb <- factor(carb)
})

# Correlation Analysis
# Scatterplot for mpg against rear axle ratio
plot(mtcars2$drat, mtcars$mpg, 
     main = "Scatterplot of Fuel Efficiency in MPG against Rear Axle Ratio",
     xlab = "Rear Axle Ratio", ylab = "Miles Per Gallon",
     col="red", 
     pch = 19, frame = TRUE)

# Scatterplot for mpg against hp
plot(mtcars2$hp, mtcars$mpg, 
     main = "Scatterplot of Fuel Efficiency in MPG against Horsepower",
     xlab = "Horsepower", ylab = "Fuel Efficiency in MPG",
     col="red", 
     pch = 19, frame = TRUE)

# Select mpg, drat, and hp variables to subset the data
myvars <- c("mpg","drat", "hp")
mtcars_subset <- mtcars2[myvars]

# Print the correlation matrix
corr_matrix <- cor(mtcars_subset, method = "pearson")
round(corr_matrix, 9)

# Reporting Results
# Create a multiple regression model for fuel efficiency (mpg) as the response 
# variable and rear axle ratio (drat) and horsepower (hp) as predictors 
# and print statistics
model <- lm(mpg ~ drat + hp, data = mtcars_subset)
summary(model)

# Fitted values
fitted_values <- fitted(model) 
fitted_values

# Residuals
residuals <- residuals(model)
residuals

# Residuals against fitted values
plot(fitted_values, residuals, 
     main = "Residuals against Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals",
     col= "red", 
     pch = 19)

# Normal Q-Q plot
qqnorm(residuals, pch = 19, col= "red")
qqline(residuals, col = "blue", lwd = 2)

# Evaluate Model Significance
# Confidence intervals for model parameters
print("95% confidence interval")
conf_95_int <- confint(model, level = 0.95) 
round(conf_95_int, 4)

# Make predictions using the model
# Prediction for drat = 3.15 and hp = 120
newdata <- data.frame(drat = 3.15, hp = 120)

#print("prediction interval")
print("95% prediction interval")
prediction_pred_int <- predict(model, newdata, interval= "predict", level = 0.95) 
round(prediction_pred_int, 4)

#print("confidence interval")
print("95% confidence interval")
prediction_conf_int <- predict(model, newdata, interval= "confidence", level = 0.95) 
round(prediction_conf_int, 4)
