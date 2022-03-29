
# Importing data
data1=read.csv("C:/Users/panda/OneDrive/Desktop/DATA science/R SOFTWARE/day.csv")
View(data1)

head(data1)

# Install and activate package 'ggplot2' needed for histogram and box plot

install.packages("ggplot2")
library(ggplot2)

data2=subset(data1,select=-c(casual,registered))

# Histogram of the response variable #

qplot(data2$count, geom="histogram", binwidth=40, main="Histogram for count", xlab="count", fill=I("gray"), col=I("red"))+theme_bw()

# Boxplot of the response variable #

ggplot(data2,main="Boxplot for count", aes(y=count)) + geom_boxplot()+ scale_fill_grey() + theme_classic()

#  Obtaining descriptive statistics #
# Install package 'pastecs' needed for obtaining descriptive stats 
install.packages("pastecs") 
library(pastecs)

#function for displaying the descriptive statistics - mean, median, SD etc.

stat.desc(data2$count)
summary(data2$count)

#Percentile values

quantile(data2$count)

# Creating training(80%) and test set(20%)#

set.seed(123)
indset<- sample(2,nrow(data2),replace=T,prob=c(0.8, 0.2))
traindata<-data2[indset==1,]
testdata<-data2[indset==2,]
head(traindata)

# Fitting the main  MLR model with all relevant predictors #

model1=lm((count)~as.factor(season)+as.factor(yr)+as.factor(mnth)+as.factor(holiday)+as.factor(weekday)+as.factor(workingday)+as.factor(weathersit)+temp+atemp+hum+windspeed,data=traindata)
summary(model1)

# Cross validation of the model using out-of-sample data #

pred_value=predict(model1,testdata)

# cor() is a function that finds correlation between two variables, here between actual values in the testdata and pred_value which indicates values predicted by the model

cor(testdata$count,pred_value)

# calculate root mean squared error of the residuals (RMSE) #

sqrt(mean(testdata$count-pred_value)^2)

# plot the QQ-plot, residual plot and standardized residual plot for our analysis.
plot(model1)

# perfrom shapiro test

shapiro.test(model1$residuals)


# As P < 0.05 in Shapiro Wilks test, therefore performing box cox transformation to find the ideal lambda
# Installing and activating the package 'MASS' #
library(MASS)
bc = boxcox(model1, lambda= seq(-5,5 ))
best.lam = bc$x[which(bc$y==max(bc$y))]
best.lam 

# Adjust model by taking the response variable to the power of lambda
adjusted_mod1=lm((count)^0.75~as.factor(mnth)+as.factor(season)+as.factor(yr)+as.factor(workingday)+as.factor(weathersit)+temp+atemp+windspeed,data=traindata) 
plot(adjusted_mod1)

#perform Shapiro Wilkson test on the adjusted model.
shapiro.test(adjusted_mod1$residuals)

# As p value < 0.05 even after box cox transformation. hence continue with the original model model1.

# Durbin-Watson test #

install.packages("lmtest")
library(lmtest)
dwtest(model1)

#  Multicollinearity check # 

library(car)
vif(model1)

# Fitting the model with the selected variables and evaluate its performance#
model2=lm((count)~as.factor(season)+as.factor(yr)+as.factor(holiday)+as.factor(weekday)+as.factor(workingday)+as.factor(weathersit)+temp+atemp+hum+windspeed,data=traindata)
summary(model2)
vif(model2)

model3=lm((count)~as.factor(season)+as.factor(yr)+as.factor(holiday)+as.factor(weekday)+as.factor(workingday)+as.factor(weathersit)+atemp+hum+windspeed,data=traindata)
summary(model3)
vif(model3)

model4=lm((count)~as.factor(season)+as.factor(yr)+as.factor(holiday)+as.factor(workingday)+as.factor(weathersit)+atemp+hum+windspeed,data=traindata)
vif(model4)

# Variable selection #

# stepAIC() which is used to perform stepwise regression for variable selection #
step.mod=stepAIC(model1, direction = "both", trace = FALSE)
# Returns with summary of the model analysis results #
summary(step.mod)
summary(model4)
