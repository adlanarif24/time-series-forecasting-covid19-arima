vac <- read.csv("/Users/allandezman/Documents/UNL Courses/Acts 430/Group Project/Total Vaccination.csv", header=TRUE)
# read the total vaccination file 

library(forecast)
library(tseries)
library(ggplot2)
library(MASS)
library(dplyr)
# open all the libraries package required for the time series section

vacUnitedStates <- subset(vac, vac$location == "United States")
# subsetting the United States dataset in vacUnitedStates
vacCanada <- subset(vac, vac$location == "Canada")
# subsetting the Canada dataset in vacCanada


vacCanada <- vacCanada %>% arrange((total_vaccinations)) 
vacUnitedStates <- vacUnitedStates %>% arrange((total_vaccinations)) 
# rearrange both the total vaccination datasets in each subset in ascending order

##########################################################################################
# Canada Dataset #
##########################################################################################

vac.raw1 <- vacCanada[325:520,11]
# input the total vaccination dataset vector as the first data of vaccination starts from the 325th data
vac.ts1 <- ts(vac.raw1)
plot(vac.ts1)
# use the time series function to plot the adjusted graph


acf(vac.ts1)
pacf(vac.ts1)
# conduct the acf & pacf function 
auto.arima(vac.ts1)
# use the auto arima to select the best model according to AIC value

adf.test(vac.ts1)
# testing for stationarity

adf.test(diff(vac.ts1))
# testing for stationarity, differentiate once

adf.test(diff(diff(vac.ts1)))
# testing for stationarity, differentiate twice 

m1 <- arima(vac.ts1,c(5,2,2),include.mean = T)
fitm1 <- fitted(m1)
# fit the model

par(mfrow = c(2,2))
plot(m1$residuals/sd(m1$residuals), main = "Standardized Residuals")
acf(m1$residuals, xlab = "Lag")
pacf(m1$residuals, xlab = "Lag")
qqnorm(m1$residuals/sd(m1$residuals), ylab = "Standardized Residuals")
#residuals and model diagnostics

adf.test(m1$residuals)
#testing for stationarity, for residuals

forem1 <- forecast(m1, 180)
# forecast the data for another 180 days (half a year) 

df.obsNfit1 <- data.frame(time = 1:length(vac.ts1),obs = vac.ts1, fit = fitm1)
df.fore1 <- data.frame(time = (length(vac.ts1)+1):(length(vac.ts1)+180), fore = forem1$mean, upper = forem1$upper[,2], lower = forem1$lower[,2])
# convert into data.frame

Fig1 <- ggplot(data=df.obsNfit1,aes(x=time,y=obs))
Fig1
Fig1 + geom_line(col='black') + geom_smooth(aes(x=time, y=fore, ymax=upper, ymin=lower),
      colour='blue', data=df.fore1, stat='identity') + ggtitle(paste("Total Vaccinations of Canada in 180 days"))
    
# figure 1: Canada plot 

a <- 0
b <- 37742157
#upper bounds and lower bounds
y <- log((vac.raw1-a)/(b-vac.raw1))
fit1 <- ets(y)
fc <- forecast(fit1, h=180)
#transform data
fc$mean <- (b-a)*exp(fc$mean)/(1+exp(fc$mean)) + a
fc$lower <- (b-a)*exp(fc$lower)/(1+exp(fc$lower)) + a
fc$upper <- (b-a)*exp(fc$upper)/(1+exp(fc$upper)) + a
fc$x <- vac.raw1
plot(fc, main = "Total Vaccinations of canada in 180 days")
# Back-transform forecasts

##logit transformation consideration

##########################################################################################
# United States Dataset #
##########################################################################################

vac.raw2 <- vacUnitedStates[350:524,11]
# input the total vaccination dataset vector as the first data of vaccination starts from the 350th data
vac.ts2 <- ts(vac.raw2)
plot(vac.ts2)
# use the time series function to plot the adjusted graph

acf(vac.ts2)
pacf(vac.ts2)
# conduct the acf & pacf function 
auto.arima(vac.ts2)
# use the auto arima to select the best model according to AIC value

adf.test(vac.ts2)
#testing for stationarity

adf.test(diff(vac.ts2))
# testing for stationarity, differentiate once

adf.test(diff(diff(vac.ts2)))
# testing for stationarity, differentiate twice 

m2 <- arima(vac.ts2,c(0,2,2),include.mean = T)
fitm2 <- fitted(m2)
# fit the model

par(mfrow = c(2,2))
plot(m2$residuals/sd(m2$residuals), main = "Standardized Residuals")
acf(m2$residuals, xlab = "Lag")
pacf(m2$residuals, xlab = "Lag")
qqnorm(m2$residuals/sd(m2$residuals), ylab = "Standardized Residuals")
#residuals and model diagnostics

adf.test(m2$residuals)
#testing for residuals

forem2 <- forecast(m2, 180)
# forecast the data for another 365 days (1 year) 

df.obsNfit2 <- data.frame(time = 1:length(vac.ts2),obs = vac.ts2, fit = fitm2)
df.fore2 <- data.frame(time = (length(vac.ts2)+1):(length(vac.ts2)+180), fore = forem2$mean, upper = forem2$upper[,2], lower = forem2$lower[,2])
# convert into data.frame

Fig2 <- ggplot(data=df.obsNfit2,aes(x=time,y=obs))
Fig2
Fig2 + geom_line(col='black') + geom_smooth(aes(x=time, y=fore, ymax=upper, ymin=lower), 
       colour='blue', data=df.fore2, stat='identity') + ggtitle(paste("Total Vaccinations of United States in 180 days"))
# figure 2: United States plot 

a <- 0
b <- 331002647
#upper bounds and lower bounds
y <- log((vac.raw2-a)/(b-vac.raw2))
fit1 <- ets(y)
fc <- forecast(fit1, h=180)
#transform data
fc$mean <- (b-a)*exp(fc$mean)/(1+exp(fc$mean)) + a
fc$lower <- (b-a)*exp(fc$lower)/(1+exp(fc$lower)) + a
fc$upper <- (b-a)*exp(fc$upper)/(1+exp(fc$upper)) + a
fc$x <- vac.raw2
plot(fc, main = "Total Vaccinations of United States in 180 days")
# Back-transform forecasts

##Logit transformation consideration
