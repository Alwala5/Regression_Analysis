### Importing data
data1=read.csv("C:/Users/A SRINIDHI/Downloads/day.csv")
### install package 'ggplot2' for histogram and box plot
install.packages("ggplot2")
library(ggplot2)

### Generation of Histogram of response variable
qplot(data1$cnt,geom = "histogram",bins=50,main="Histogram for Bike Rentals",xlab="count")

### Generation of Box plot of response variable
ggplot(data1,aes(y=cnt))+geom_boxplot()+ scale_fill_grey() + theme_classic()

### Install 'pastecs' package for descriptive statistics
install.packages("pastecs")
library(pastecs)
stat.desc(data1$cnt)

### Creating training and test data
set.seed(123)
indx=sample(1:nrow(data1),0.9*nrow(data1))
traindata=data1[indx,]
testdata=data1[-indx,]
dim(traindata)
dim(testdata)
head(traindata)

### Fitting Multi Linear Regression model(MLR)
mod1=lm((cnt)~as.factor(season)+as.factor(weekday)+as.factor(workingday)+as.factor(yr)+as.factor(mnth)+as.factor(holiday)+as.factor(weathersit)+temp+atemp+windspeed+hum,data=traindata)
summary(mod1)

### Cross validation of model using out-of-sample data
pred_val=predict(mod1,testdata)

### Finding correlation between two variables using cor(),here between actual values in the test data and pred_val which indicates values predicted by model
cor(testdata$cnt,pred_val)

### calculate root mean squared error of the residuals (RMSE)
sqrt(mean((testdata$cnt-pred_val)^2))

### Plot QQ plot and standardized residual plot for checking normality,homoscedasticity and errors.

### Perform Shapiro test
shapiro.test(mod1$residuals)

### As P<0.05 in Shapiro test so performing Box Cox Transformation
install.packages("MASS")
library(MASS)
bc = boxcox(mod1,lamda=seq(-5,5))
best.lam = bc$x[which(bc$y==max(bc$y))]
best.lam

## Adjust model by taking the response variable to the power of lamda
adjusted_mod1=lm((cnt)^0.79~as.factor(season)+as.factor(weekday)+as.factor(workingday)+as.factor(yr)+as.factor(mnth)+as.factor(holiday)+as.factor(weathersit)+temp+atemp+windspeed+hum,data=data1)

### perform Shapiro test on adjusted model
shapiro.test(adjusted_mod1$residuals)
##Even after performing Box Cox transformation p value is still less than 0.05. Hence continue with original model.

### Perform Durbin Watson test to check independence
install.packages("lmtest")
library(lmtest)
dwtest(mod1) 
## As p>0.05 residuals are not auto correlated i.e.,they are independent

### Checking Multicollinearity
install.packages("car")
library(car)
vif(mod1)
## Dropping mnth as vif value is very high
mod2= lm((cnt)~as.factor(season)+as.factor(weekday)+as.factor(workingday)+as.factor(yr)+as.factor(holiday)+as.factor(weathersit)+temp+atemp+windspeed+hum,data=traindata)
vif(mod2)
## Dropping temp because vif>>10
mod3= lm((cnt)~as.factor(season)+as.factor(weekday)+as.factor(workingday)+as.factor(yr)+as.factor(holiday)+as.factor(weathersit)+atemp+windspeed+hum,data=traindata)
vif(mod3)
## Dropping weekday bcoz vif>10
mod4= lm((cnt)~as.factor(season)+as.factor(workingday)+as.factor(yr)+as.factor(holiday)+as.factor(weathersit)+atemp+windspeed+hum,data=traindata)
vif(mod4)
summary(mod4)

### Variable Selection using  Stepwise Method
library(MASS)
step.model=stepAIC(mod4,direction="both")
summary(step.model)
