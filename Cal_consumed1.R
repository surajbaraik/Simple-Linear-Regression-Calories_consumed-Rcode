## Assignment Question 1 on Simple Linear Regression
## calories consumed , weight gained problem

cal.consumed = read.csv(file.choose())
attach(cal.consumed)
View(cal.consumed)

summary(cal.consumed)

plot(Calories.Consumed, Weight.gained..grams.) ### scatter plot
### Calories.consumed is X, and Weight gained is Y

### after visualization of scatter plot, we can say it is positive in direction 
### strength is moderate
### 


cor(Calories.Consumed,Weight.gained..grams.) ### correlation coefficient 
### Calories consumed is X and Weight gained is Y
## cor value = 0.946991, correlatation is good, we can proceed with model building
### proceed with linear model with formula Y= Bo +B1x

model.1 <- lm(Weight.gained..grams.~ Calories.Consumed) ## Linear regression Model
### lm(Output Y ~ Input X)
model.1

summary(model.1)

### pvalue is less than 0.05, good significant value with *** (p-value: 2.856e-07)
### Multiple R-squared:  0.8968,	Adjusted R-squared:  0.8882, both values are close
###  Y' = -625.7524 + 0.4202 x
### for x = 1500, Y' = 4.5476

predict(model.1)
model.1$residuals

confint(model.1,level = 0.95)

####                        2.5 %       97.5 %
### (Intercept)       -845.4266546 -406.0780569 (upper and lower limit)
### Calories.Consumed    0.3305064    0.5098069 (Bo and B1)

predict(model.1, interval = "confidence")
####         fit        lwr        upr
####1     4.482599  -95.02772  103.99291 (first value was 4.5476)

rmse <- sqrt(mean(model.1$residuals^2))
rmse ### rmse [1] 103.3025


########## LOG Model

plot(log(Calories.Consumed),Weight.gained..grams.)### scatter plot
### Calories.consumed is X, and Weight gained is Y

### after visualization of scatter plot, we can say it is positive in direction 
### strength is moderate

cor(log(Calories.Consumed),Weight.gained..grams.)

####  Weight.gained..grams. 0.9466781
####  Calories.Consumed     0.8987253

model.2 <- lm(Weight.gained..grams.~ log(Calories.Consumed))
summary(model.2)

#### pvalue is less than 0.05, good significant value

rmse.2 <- sqrt(mean(model.2$residuals^2))
rmse.2


##### Exponential model

plot(Calories.Consumed, log(Weight.gained..grams.))
cor(Calories.Consumed, log(Weight.gained..grams.))
model.3 = lm(log(Weight.gained..grams.)~ Calories.Consumed)
###### lm(output Y ~ input X)

summary(model.3)

model.3$residuals

log_at <- predict(model.3,interval = "confidence")
log_at
at <- exp(log_at)
at

err<- Weight.gained..grams. - at
err
rmse.3<-sqrt(mean(err^2))
rmse.3

########
######### Polynomial Transformation

model.4 = lm(log(Calories.Consumed) ~ Weight.gained..grams.)
summary(model.4)

confint(model.4, level=0.95)
log_res <- predict(model.4, interval = "confidence")
atpoly <- exp(log_res)
atpoly
err_poly <- Weight.gained..grams. - atpoly
err_poly

rmse.4 <- sqrt(mean(err_poly^2))
rmse.4
