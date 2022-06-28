#TSA presentation
library(forecast)
library(tseries)
library(fpp2)
library(readxl)
library(ggplot2)
library(sarima)
library(astsa)
library(lubridate)
library(readxl)
train2 <- read_excel("file name")
head(train2)
store<-train2[c(train2$Store==3&train2$Dept==2),]
store1<-data.frame(store$Date,store$Weekly_Sales)
n=nrow(store1)
#Changing into time series
week<-ts(store1$store.Weekly_Sales,start =decimal_date(ymd('2010-02-05')),frequency=365.25/7)
week_h<-ts(store1$store.Weekly_Sales,frequency=52)

ts.plot(week,main="Time Series Plot of Weekly sales of Store 3, Department 2",xlab="Time",ylab="Weekly Sales")

#ACF anf PACF
plot(acf(week),main="ACF plot of the original series",ylab="ACF values",xlab="Lag")
plot(pacf(week),main="PACF plot of the original series",ylab="PACF values",xlab="Lag")

#Decomposing
d<-decompose(week,'multiplicative')
plot(d)#presence of trend and seasonality

#stationarity check
adf.test(week)
kpss.test(week)

# Smoothing method
fit1<-HoltWinters(week_h,seasonal = 'additive')
fit2<-HoltWinters(week_h,seasonal="multiplicative")
f_1<-forecast:::forecast.HoltWinters(fit2,h=31)
accuracy(f_1)
par(mfrow=c(1,2))
plot(fit1)
plot(fit2)
fit3<-HoltWinters(week,seasonal = 'additive',alpha=0.5,beta = 0.5,gamma = 0.5)
fit4<-HoltWinters(week,seasonal = 'additive',alpha=0.2,beta = 0.2,gamma = 0.2)
fit5<-HoltWinters(week,seasonal = 'additive',alpha=0.8,beta = 0.8,gamma = 0.8)
par(mfrow=c(1,3))
plot(fit3)
plot(fit4)
plot(fit5)
par(mfrow=c(1,1))

#forecast
f_1<-forecast:::forecast.HoltWinters(fit1,h=31)
f_2<-forecast:::forecast.HoltWinters(fit3,h=31)
f_3<-forecast:::forecast.HoltWinters(fit4,h=31)
f_4<-forecast:::forecast.HoltWinters(fit5,h=31)

plot(f_1,main="Forecast of Holt-Winter method",xlab="Time",ylab="Weekly Sales")

#accuracy of each model
round(accuracy(f_1),2)#parameter =1
round(accuracy(f_2),2)#parameter= 0.5
round(accuracy(f_3),2)#parameter= 0.2
round(accuracy(f_4),2)#parameter=0.8


#ARIMA
index=round(0.80*n,0)
train2ing=store1[c(1:index),]
test=store1[c(115:143),]
week1<-ts(train2ing$store.Weekly_Sales,start =decimal_date(ymd('2010-02-05')),frequency=365.25/7)
week2<-ts(test$store.Weekly_Sales,start =decimal_date(ymd('2012-04-13')),frequency=365.25/7)

#No need to do acf and pacf plot as they are same as before
fit_arima_train2<-arima(week1,order=c(1,1,1),seasonal=c(0,1,0))
fit_arima_train2
summary(fit_arima_train2)
plot(week1,main="The fitted values using the ARIMA model",ylab="Weekly Sales") 
lines(fitted(fit_arima_train2),col='red')
f=forecast(fit_arima_train2,h=43)

plot(f)
lines(test,col='red')
fit_arima_test<-Arima(week2,order=c(1,1,1))
plot(forecast(fit_arima_test))
f=forecast(fit_arima_test,h=31)
plot(f)
fit_train2<-auto.arima(week1,trace=TRUE)
mod=auto.arima(week1)


auto.arima(week1)
auto.arima(week2)

fit_arima_test<-arima(week,order=c(1,1,1),seasonal=c(0,1,0))
f=forecast(fit_arima_test,h=31)
plot(f)

round(accuracy(f),2)

fit_arima_test$fitted

forecast(fit_arima_test)

forecast(fit_arima_test[31,])

