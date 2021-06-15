#Misha Khan
#BANA 288 Predictive Analytics
#Spring 2021

#Homework #2: Modeling with Regression

#FIX: drop wkday5 (NA value, similar to holiday)
################################################################################

#1. Perform all necessary cleaning and transformation of the data to make it
#useful for regression modeling.  Hint:  It might be easier in some steps to
#set up the qualitative variables in the data set as factors rather than to
#create individual indicator variables across the board 
#(use the “as.factor()” function).  This is because there are many levels for 
#some of the qualitative variables in the data set.  

dat <- read.csv("~/Desktop/SPRING 2021/Predictive/Homework 2/hw2_hour.csv")
str(dat)
names(dat)

#Create dat1 - qualitative variables as factors
dat1 <- dat
dat1$mnth <- as.factor(dat1$mnth)
dat1$season <- as.factor(dat1$season)
dat1$hr <- as.factor(dat1$hr)
dat1$wkday <- as.factor(dat1$wkday)
dat1$weathersit <- as.factor(dat1$weathersit)

#Remove NA values
dat1 <- na.omit(dat)
dat1 <- dat1[,c(15,1,3:14)]
str(dat1)

################################################################################

#2. Compute the correlation between all independent variables and the response.  
#List the top 3 (or more) correlated variables.  
#Explain why these make sense or why they are surprising. 

reg.all <- lm(cnt ~., data = dat1)
summary(reg.all)
#Mult R squared 0.389
#Significant (small p): season, yr, hr, holiday,wkday, temp, atemp, hum, windspeed

corr <- data.frame(cor(dat1)[,1])
colnames(corr) <- c("Correlation")
corr
#Top 3 correlated: hr (.3941), temp (.4048), atemp (.4009)

#From observing the dataset before regression, I assumed month, season,
#hour, and temperature would be highly correlated because external factors
#do impact a customer's decision on whether to rent a bike or not.
#I was surprised to see holiday has a negative correlation of -0.03 with count.
#I would assume that if there is a holiday, more people would have more free time
#and perhaps rent a bike but I guess that is not the case.
#Overall, hour, temp, and atemp do make sense for having a high correlation with 
#count of rental bikes because people are more inclined to bike when the temperature
#(or feeling of temperature) is higher or later in the day.

################################################################################

#3. Using the season indicators, the hourly indicators, and all the variables 
#associated with the weather conditions, construct a linear regression model 
#predicting the rental count (cnt) using this set of variables.  

#Create dat2 - with indicator variables for non quantitative variables
#Convert to factor
mnth <- as.factor(dat$mnth) #12 var
season <- as.factor(dat$season) #4 var
hr <- as.factor(dat$hr) #24 var
wkday <- as.factor(dat$wkday) #7 var
weathersit <- as.factor(dat$weathersit) #4 var

#Use model matrix to create the columns and data.frame to make into a data frame 
# -1 because for dummy variable, we need one less var
tmp_mnth <- data.frame(model.matrix(~mnth-1))
tmp_season <- data.frame(model.matrix(~season-1))
tmp_hr <- data.frame(model.matrix(~hr-1))
tmp_wkday <- data.frame(model.matrix(~wkday-1))
tmp_weathersit <- data.frame(model.matrix(~weathersit-1))

#Adding columns to dat2
dat2 <- cbind(dat[,c(15,1,4)], 
              tmp_season[,1:3],               
              tmp_mnth[,1:11], 
              dat[,c(7,9)],               
              tmp_wkday[,1:6], 
              tmp_hr[,1:23],               
              tmp_weathersit[,2:4], 
              dat[,11:14])

#Remove previous variables
rm(mnth, season, hr, wkday, weathersit)
rm(tmp_mnth, tmp_season, tmp_hr, tmp_wkday)
rm(tmp_weathersit)
str(dat2)

#Correlation
corr2 <- data.frame(cor(dat2)[,1])
colnames(corr2) <- c("Correlation")
corr2
#Highly correlated
#hr17 0.3139 ~
#hr18 0.2721 ~ 
#temp 0.4047
#atemp 0.4009
#hum -0.3229

#Construct a linear regression model using dummy variables
#Model with all variables
reg.dum <- lm(cnt ~., data = dat2)
reg.dum
summary (reg.dum) #68.64%

#Model with only season indicators, the hourly indicators, and all the variables 
#associated with the weather conditions
reg.dum2 <- lm(cnt~. -yr-mnth1-mnth2-mnth3-mnth4-mnth5-mnth6-mnth7-mnth8-mnth9-mnth10
               -mnth11-yr-holiday-workday-wkday0-wkday1-wkday2-wkday3-wkday4-wkday5, data = dat2)
summary(reg.dum2) #67.76%

#Model with only significant indicators
reg.dum3 <- lm(cnt~. -yr-mnth1-mnth2-mnth3-mnth4-mnth5-mnth6-mnth7-mnth8-mnth9-mnth10
               -mnth11-yr-holiday-workday-wkday0-wkday1-wkday2-wkday3-wkday4-wkday5-hr6-weathersit4, data = dat2)
summary(reg.dum3) #67.46%

#Interpret the coefficient of determination.  
#Coeff of Det (R^2): Mult - 68.6%, Adj - 68.5%
#68.6% of the data fits the regression model. 
#68.6% of variability in count of rental bikes (y) is explained by the 
#season, hour, weather predictor variables.

#Which variables are significant at the 5% level?
#yr, season1, season2, season3, mnth7, mnth9, holiday, wkday0, wkday1, wkday2,
#hr0 - hr5, hr7 - hr22, weathersit2, weathersit3, temp, atemp, hum, windspeed 

#hr6 and weathersit4 were not significant at alpha = 0.05.
################################################################################

#4.Choose preferred linear regression model predicting the rental count (cnt) 
#using a subset of the variables described in the prior question.  
#Justify the model choice by using the nested F-test to compare the chosen 
#model in this question to the model fit in prior question.  
#Interpret the result of the hypothesis test.   

###USING ANOVA###
#Create a model using the subset from question 3 (without hr6 and weathersit4)
#aka using third model from #3
reg.dum3 <- lm(cnt~ . -yr-mnth1-mnth2-mnth3-mnth4-mnth5-mnth6-mnth7-mnth8-mnth9-mnth10
               -mnth11-yr-holiday-workday-wkday0-wkday1-wkday2-wkday3-wkday4-wkday5-hr6-weathersit4, data = dat2)
summary(reg.dum3) #67.46%

#reg.dum2 = big
#reg.dum3 = small
anova(reg.dum2, reg.dum3)

#  Ho:  Models are same
#  Ha:  Model with more variables is better
# p = 0.5579 > alpha = reject H0, we assume that models are the same
#The small model is equally good at explaining count of rental bikes compared to the bigger model.


###USING BEST SUBSETS###
install.packages("leaps")
library(leaps)
#Run best subset regression (forward)
regfit.full <- regsubsets(cnt ~. -wkday5, data = dat2, nvmax = 10, really.big = T)
summary(regfit.full)
#1 Variable: temp
#2 Variables: temp, hum
#3 Variables: temp, hum, hr17
#4 Variables: temp, hum, hr17, hr8
#5 Variables: temp, hum, hr17, hr8, hr18

summary(regfit.full)$rsq
is.atomic(regfit.full) #FALSE
is.recursive(regfit.full) #TRUE
#As we add more variables, RSQ increases (16% with 1 variable > 68% with all)
#Hence why RSQ is not a good measures because it suggests we should always
#pick more variables.



reg.summary <- summary(regfit.full)
names(reg.summary)
par(mar=c(5.1, 4.1, 4.1, 2.1))
plot(reg.summary$rss , xlab ="Number of Variables", ylab ="RSS ",type ="l")
plot(reg.summary$adjr2 ,xlab ="Number of Variables",ylab ="Adjusted RSq", type ="l")
plot(summary(regfit.full)$rsq)
#Plot stops after ~10 variables

#Stepwise Regression
regfit.fwd <- regsubsets(cnt~. - wkday5,data=dat2, nvmax = 10, method = "forward")
summary(regfit.fwd)
#Comparison Bestsubset vs Stepwise
coef(regfit.fwd, 8)
coef(regfit.full, 8)
#Different answers

#Backward Stepwise Regression
regfit.bwd <- regsubsets(cnt~. - wkday5 ,data=dat2, nvmax = 10, method = "backward")
summary(regfit.bwd)
#1 Variable: temp
#2 Variables: temp, hr17 
#3 Variables: temp, hr17, hr18
#4 Variables: temp, hr17, hr18, hr8
#5 Variables: temp, hr17, hr18, hr8, yr
coef(regfit.bwd, 8)
coef(regfit.full, 8)
#Backward is different from Best Subset and Forward

#Compare models
reg.test <- lm(cnt ~., data = dat2)
summary(reg.all2) #68.6%

#Remove insignificant variables
reg.test2 <- lm(cnt ~ . -mnth1-mnth2-mnth3-mnth4-mnth5-mnth6-mnth8-workday-
              wkday3-wkday4-hr6-weathersit4, data = dat2)
summary(reg.test2) #68.6%

#Remove insignificant variables again
reg.test3 <- lm(cnt ~ . -mnth1-mnth2-mnth3-mnth4-mnth5-mnth6-mnth8-mnth11-workday-
              wkday2-wkday3-wkday4-wkday5-hr6-weathersit4, data = dat2)
summary(reg.test3) #68.6%

#Nested F Test
reg.test2$residuals
RSS.all <- sum((reg.test2$residuals)^2)
RSS.all
RSS.3 <- sum((reg.test3$residuals)^2)
RSS.3
F.calc <- ((RSS.3 - RSS.all)/14) / (RSS.all/(17379 - 54 - 1))
F.calc
F.crit <- qf(1 - 0.05, 14, 17325)
F.crit
#F.calc = 3.119
#F.crit = 1.69
#F.calc > F.crit = reject H0
P.value <- 1 - pf(F.calc, 14, 17325)
P.value

#  Ho:  Models are same
#  Ha:  Model with more variables is better
# p ~ 0 < alpha = reject H0, we assume that models are not the same
################################################################################

#5. Using the entire data set, chose a preferred model for predicting rentals.  
#Justify the model choice both using metrics for measuring model quality and practically. 

#Run all variables on dat2
reg.all <- lm(cnt ~., data = dat2)
summary(reg.all) #68.6%

#Remove insignificant variables
reg.all2 <- lm(cnt~. -obs-season1-season2-season3-mnth7-mnth9-holiday-wkday0
               -wkday1-wkday2-hr0-hr1-hr2-hr3-hr4-hr5-hr7-hr8-hr9-hr10-hr11-hr12-
                 hr13-hr14-hr15-hr16-hr17-hr18-hr19-hr20-hr21-hr22-weathersit2-
                 weathersit3-temp-atemp-windspeed, data = dat2)
summary(reg.all2) 
#After removing insignificant variables from the entire dataset, the model got 
#worse (only 25% fit). Not a good approach.


reg.all3 <- lm(cnt ~. -obs-mnth1-mnth2-mnth3-mnth4-mnth11-workday
               -wkday1-wkday2-wkday3-wkday4-hr0-hr1-hr2-hr3-hr4-hr5-hr6-weathersit4, data=dat2)
summary(reg.all3) #67.82%

#obs is an indicator variable, will not be useful for analysis
#After looking at correlation and using common knowledge, I took out
#the summer months for I would assume more people would rent out bikes
#Also, bike rentals are more likely to be popular over the weekend around afternoon time.

#reg.all: big
#reg.all3: small
anova(reg.all, reg.all3)

#  Ho:  Models are same
#  Ha:  Model with more variables is better
#p ~0 < alpha 0.05
#Because p is smaller than alpha, we reject H0 and assume that the
#models are not the same.

################################################################################

#6.Randomly select approximately half of the observations to create a training 
#data set.  Add the remaining rows to create a test data set.  Using the 
#variables chosen in the model from question 5, compute the RSS, MSE, and 
#RMSE on the training data.  Repeat this on the test data, by using the model 
#fit on the training data to compute predictions on the test set.
#run reg and fit it on training data no longer using dat2

#Split data into 2 subsets
set.seed(100)
train6 <- sample(1:nrow(dat2), nrow(dat2) * 0.5)
dat.train6 <- dat2[train6,]
dat.test6 <- dat2[-train6,]

#Dimension to check split data
dim(dat.train6) #8689 55
dim(dat.test6) #8690 55

#Run previous regression model training data
reg6.train <- lm(cnt ~. -obs-mnth1-mnth2-mnth3-mnth4-mnth11-workday
               -wkday1-wkday2-wkday3-wkday4-hr0-hr1-hr2-hr3-hr4-hr5-hr6-weathersit4, data=dat.train6)
summary(reg6.train)

sum.reg6.train <- summary(reg6.train)
sum.reg6.train$sigma #101.64 (shows variance inflation/ training data RMSE)

#Training data RSS, MSE, and RMSE
yhat6.train <- predict(reg6.train, dat.train6)
yhat6.train
RSS6.train <- sum((dat.train6$cnt - yhat6.train)^2)
RSS6.train #89393010
MSE6.train <- RSS6.train/nrow(dat.train6)
MSE6.train #10288.07
RMSE6.train <- MSE6.train^0.5
RMSE6.train #101.4301(testing data RMSE)

#Testing data RSS, MSE, and RMSE
yhat6.test <- predict(reg6.train, dat.test6)
yhat6.test
RSS6.test <- sum((dat.test6$cnt - yhat6.test)^2)
RSS6.test #95292705
MSE6.test <- RSS6.test/nrow(dat.test6)
MSE6.test #10965.79
RMSE6.test <- MSE6.test^0.5
RMSE6.test #104.7177

table6 <- matrix (c(RSS6.train, MSE6.train, RMSE6.train,
                    RSS6.test, MSE6.test, RMSE6.test), ncol = 2, byrow = FALSE)
options(scipen = 100)
colnames(table6) <- c("Training", "Testing")
rownames(table6) <- c("RSS", "MSE", "RMSE")
table6

#RMSE are close in value, it is a good balance (low bias, low variance)
################################################################################
#7. Repeat question 6 using the same training and test data sets.  
#In this case, fit the model using all variables in the data set.  
#Again, compute the model fit errors on the training set as well as on the 
#test set by using the training model to make the test set predictions.  
#Comment on the performance of the model with respect to the bias-variance 
#tradeoff.  Compare results to question 6.

#Split data into 2 subsets
set.seed(100)
train7 <- sample(1:nrow(dat2), nrow(dat2) * 0.5)
dat.train7 <- dat2[train7,]
dat.test7 <- dat2[-train7,]

dim(dat.train7) #8689 55
dim(dat.test7) #8690 55

#Run regression model on all the variables
reg7.train <- lm(cnt~., data = dat.train)
summary(reg7.train)

sum.reg <- summary(reg7.train)
sum.reg$sigma #100.4 (shows variance inflation/ training data RMSE)

#Training data RSS, MSE, and RMSE
yhat7.train <- predict(reg7.train, dat.train7)
yhat7.train
RSS7.train <- sum((dat.train7$cnt - yhat7.train)^2)
RSS7.train #87080559
MSE7.train <- RSS7.train/nrow(dat.train7)
MSE7.train #10021.93
RMSE7.train <- MSE7.train^0.5
RMSE7.train #100.1096 (testing data RMSE)

#Testing data RSS, MSE, and RMSE
yhat7.test <- predict(reg7.train, dat.test7)
yhat7.test
RSS7.test <- sum((dat.test7$cnt - yhat7.test)^2)
RSS7.test #93177860
MSE7.test <- RSS7.test/nrow(dat.test7)
MSE7.test #10722
RMSE7.test <- MSE7.test^0.5
RMSE7.test #103.5

anova(reg6.train,reg7.train)

#  Ho:  Models are same
#  Ha:  Model with more variables is better
#p ~0 < alpha 0.05
#Because p is smaller than alpha, we reject H0 and assume that the
#models are not the same.

table7 <- matrix (c(RSS7.train, MSE7.train, RMSE7.train,
                    RSS7.test, MSE7.test, RMSE7.test), ncol = 2, byrow = FALSE)
options(scipen = 100)
colnames(table7) <- c("Training", "Testing")
rownames(table7) <- c("RSS", "MSE", "RMSE")
table7

#RMSE are close in value, it is a good balance (low bias, low variance)
################################################################################

#8. Repeat question 7 using the first year’s data as the training data set and 
#the second year’s data as the test set.  How does this result compare to the two earlier questions?
set.seed(100)
dat.train8 <- dat2[dat2$yr ==0,]
dat.test8 <- dat2[dat2$yr == 1,]

reg.8 <- lm(cnt ~., data = dat.train8)
summary(reg.8)

train8_predict <- predict(reg.8, dat.train8)
test8_predict <- predict(reg.8, dat.test8)

RSS8.train <- sum((dat.train8$cnt - train8_predict)^2) 
RSS8.train
MSE8.train <- RSS8.train/nrow(dat.train8)
MSE8.train
RMSE8.train <- MSE8.train^0.5
RMSE8.train

RSS8.test <- sum((dat.test8$cnt - test8_predict)^2)
RSS8.test 
MSE8.test <- RSS8.test/nrow(dat.test8)
MSE8.test 
RMSE8.test <- MSE8.test^0.5
RMSE8.test 
  

table8 <- matrix (c(RSS8.train, MSE8.train, RMSE8.train,
                    RSS8.test, MSE8.test, RMSE8.test), ncol = 2, byrow = FALSE)
options(scipen = 100)
colnames(table8) <- c("Training", "Testing")
rownames(table8) <- c("RSS", "MSE", "RMSE")
table8
#Lower the RMSE = better the fit
#We can see that the Training set has a lower value (75.1) compared to 
#the Testing set (190.8). This may suggest that there is overfitting in the
#training model therefore it is not the best model to use.
  
#When we look at the models in #6 and #7, the RMSE values are very close. This
#means that the model are good because there is little difference in error.

################################################################################

#9. Summarize, briefly, what the managers of the bike rental concern should 
#know about the prediction models found.  How effective are the models?  
#What model should be used to predict in the future (year 3)?  
#How confident are you in future predictions?  
  
#Overall, we can see a pattern in season, hour, month, and weather.
#Colder months or months with less holiday are less likely to have an impact
#on rental bike count. Weekdays and early in the morning also are less likely.
#Warmer weather, summer season, later in the day are more likely to 
#contribute to the rental bike count.

#I would use a model that includes warmer seasons/months, warmer weather,
#and later in the day. The models previously performed roughly have a ~68% fit 
#of the data. Because the model is good but not strong, this suggests that
#further transformation on the predictors might help our model fit.

################################################################################

#10.  Are there any transformations of the variables that can be added to 
#improve the model fit (e.g., squared variables)?  Are there any interaction 
#variables (products of variables) that can be added?  Try at least one 
#variable with higher order terms (e.g., squared,) and at least one interaction 
#model to see if the biased can be reduced in fitting the model on the 
#training set.  Report results. 

reg.10 <- lm(cnt ~ I(temp^2) + I(hum^2) + I(hr17^2) + I(hr18^2), data= dat2)
summary(reg.10)

#I squared the terms that are highly correlated with count. The model is not
#good (36.3% fit)

reg.11 <-lm(cnt ~ season2*temp +hr16*hr17*hr18*hr19 + mnth6*mnth7*mnth8+ temp*hum, dat = dat2)
summary(reg.11) 

#I created interaction terms on variables that I thought would have a close relationship
#but the model is still not that good (43.3%)

reg.12 <- lm(log(cnt)~., dat = dat2)
summary(reg.12)

#I did log transformation of count and the model improved (82.4%). 

################################################################################
################################################################################
################################################################################
#extra
#3. reg.test2 <- lm(cnt~ season + hr + weathersit + temp +atemp + hum +windspeed, data =dat1)
#summary(reg.test2)
#reg.test <- lm(cnt ~ season1+season2+season3+hr0+hr1+hr2+hr3+hr4+hr5+hr6+hr7+hr8
#              +hr9+hr10+hr11+hr12+hr19+hr20+hr21+hr22+weathersit2+weathersit3+
#                weathersit4+temp+atemp+windspeed+hum, data = dat2)
#summary(reg.test)
  
#4.
#Install.packages("leaps")
#install.packages("leaps")
#library(leaps)
#Run best subset regression (forward)
#regfit.full <- regsubsets(cnt ~ ., data = dat2, nvmax = 54)
#summary(regfit.full)
#1 Variable: temp
#2 Variables: temp, hum
#3 Variables: temp, hum, hr17
#4 Variables: temp, hum, hr17, hr8
#5 Variables: temp, hum, hr17, hr8, hr18

#summary(regfit.full)$rsq
#is.atomic(regfit.full) #FALSE
#is.recursive(regfit.full) #TRUE
#As we add more variables, RSQ increases (16% with 1 variable > 68% with all)
#Hence why RSQ is not a good measures because it suggests we should always
#pick more variables.
# ^ Proven by this plot

#reg.summary <- summary(regfit.full)
#names(reg.summary)
#par(mar=c(5.1, 4.1, 4.1, 2.1))
#plot(reg.summary$rss , xlab ="Number of Variables", ylab ="RSS ",type ="l")
#plot(reg.summary$adjr2 ,xlab ="Number of Variables",ylab ="Adjusted RSq", type ="l")
#plot(summary(regfit.full)$rsq)
#Plot stops after ~54 variables

#Stepwise Regression
#regfit.fwd <- regsubsets(cnt~.,data=dat2, nvmax = 54, method = "forward")
#summary(regfit.fwd)
#Comparison Bestsubset vs Stepwise
#coef(regfit.fwd, 8)
#coef(regfit.full, 8)
#Same answer

#Backward Stepwise Regression
#regfit.bwd <- regsubsets(cnt~.,data=dat2, nvmax = 54, method = "backward")
#summary(regfit.bwd)
#1 Variable: temp
#2 Variables: temp, hr17 
#3 Variables: temp, hr17, hr18
#4 Variables: temp, hr17, hr18, hr8
#5 Variables: temp, hr17, hr18, hr8, yr
#coef(regfit.bwd, 8)
#coef(regfit.full, 8)
#Backward is different from Best Subset and Forward

#Compare models
#reg.all2 <- lm(cnt ~., data = dat2)
#summary(reg.all2) #68.6%

#Remove insignificant variables
#reg.2 <- lm(cnt ~ . -mnth1-mnth2-mnth3-mnth4-mnth5-mnth6-mnth8-workday-
#              wkday3-wkday4-hr6-weathersit4, data = dat2)
#summary(reg.2) #68.6%

#Remove insignificant variables again
#reg.3 <- lm(cnt ~ . -mnth1-mnth2-mnth3-mnth4-mnth5-mnth6-mnth8-mnth11-workday-
#              wkday2-wkday3-wkday4-wkday5-hr6-weathersit4, data = dat2)
#summary(reg.3) #68.6%

#Nested F Test
#reg.all2$residuals
#RSS.all <- sum((reg.all2$residuals)^2)
#RSS.all
#RSS.3 <- sum((reg.3$residuals)^2)
#RSS.3
#F.calc <- ((RSS.3 - RSS.all)/14) / (RSS.all/(17379 - 54 - 1))
#F.calc
#F.crit <- qf(1 - 0.05, 14, 17325)
#F.crit
#F.calc = 3.119
#F.crit = 1.69
#F.calc > F.crit = reject H0
#P.value <- 1 - pf(F.calc, 14, 17325)
#P.value

#  Ho:  Models are same
#  Ha:  Model with more variables is better
# p ~ 0 < alpha = reject H0, we assume that models are not the same

#ANOVA
#anova(reg.3, reg.all2)
#p ~ 0 < alpha  = reject H0, we assume that models are not the same
#Model 3 is better at explaining count of rental bikes than model all.

#models r the same ankit and mira
#reg.set <- lm(cnt ~ obs+season1+season2+season3+hr0+hr1+hr2+hr3+hr4+hr5+hr7+hr8
#              +hr9+hr10+hr11+hr12+hr19+hr20+hr21+hr22+weathersit2+weathersit3+
#                temp+atemp+windspeed+hum, data = dat2)
#summary(reg.set)
#anova(reg.dum3, reg.set)
#reg.set <- lm(cnt ~ obs+season1+season2+season3+hr0+hr1+hr2+hr3+hr4+hr5+hr7+hr8
#              +hr9+hr10+hr11+hr12+hr19+hr20+hr21+hr22+weathersit2+weathersit3+
#                temp+atemp+windspeed+hum, data = dat2)
#summary(reg.set) #54.28%

#6.
#reg <- lm(cnt~. - obs - mnth1 - mnth2 - mnth3 - mnth4 - mnth6 - mnth8 - mnth11 -
#            workday - wkday1 - wkday2 - wkday3 - wkday4 - wkday5 - hr6 - 
#           weathersit4 - atemp, data = dat.train)
#summary(reg)

#Predict y-hat
#new.y1hat <- predict(reg21, dat.test)
#new.y2hat <- predict(reg22, dat.test)
#new.y3hat <- predict(reg23, dat.test)

#Compute RSS using predictions and actuals in dat.test (test data)
#RSS21test <- sum((dat.test$Grad.Rate-new.y1hat)^2)
#RSS22test <- sum((dat.test$Grad.Rate-new.y2hat)^2)
#RSS23test <- sum((dat.test$Grad.Rate-new.y3hat)^2)

#Then compute RSS for training residuals 
#RSS21train <- sum(reg21$residuals^2)
#RSS22train <- sum(reg22$residuals^2)
#RSS23train <- sum(reg23$residuals^2)

#So we can compare residuals SSE ofRSE21train <- sqrt(RSS21train/(nrow(dat.train)-1-1))
#RSE22train <- sqrt(RSS22train/(nrow(dat.train)-2-1))
#RSE23train <- sqrt(RSS23train/(nrow(dat.train)-16-1))
#RSE21test <- sqrt(RSS21test/nrow(dat.test))
#RSE22test <- sqrt(RSS22test/nrow(dat.test))
#RSE23test <- sqrt(RSS23test/nrow(dat.test)) #test to train
#Run regressions from #5
#reg21<- lm(cnt ~., data = dat.train)
#summary(reg21) #49.2%
#reg22 <- lm(cnt~. -obs-mnth,data =dat.train)
#summary(reg22) #48.4%
#reg23 <- lm(cnt~. -obs-mnth-workday-weathersit,data =dat.train)
#summary(reg23) #48.1%

#Less rows with the same variables > smaller set, model will fit better because
#we are minimizing sum of squared errors