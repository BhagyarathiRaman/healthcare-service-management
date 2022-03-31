library(readxl)
library(zoo)
library(forecast)
library(tseries)
surgical <- read_excel("Downloads/surgical.xlsx")
#View(surgical) 
# R is considering the data time series or not
#class(surgical)


# R is showing the data as dataframe. 
#now have to convert the data to time series.


#creating a timeseries
surgicaltime <- ts(surgical$Actual,start=2011-10-10,frequency=52)
                  
#converted the data to ts

class(surgicaltime)

 #check plot surgicaltime and look the pattern. how the data looks like

plot(surgicaltime)

surgicaltime.test<-window(surgicaltime,start=(2012-07-06))
surgicaltime.train<-window(surgicaltime,end=(2021-07-06))

meann<- meanf(surgicaltime.train,h=30)
naivem<-naive(surgicaltime.train,h=30)
driftm<-rwf(surgicaltime.train,h=30,drift=T)
snaivem<-snaive(surgicaltime.train,h=30)

plot(meann,plot.conf=F,main="")
lines(naivem$mean,col=3,lty=1)
lines(driftm$mean,col=2,lty=1)
lines(snaivem$mean,col=6,lty=1)
legend("topleft",lty=1,col=c(1,3,2,6), 
       legend = c("Mean Method","Naive Method","Drift Method", "Seasonal Naive"))

plot(snaivem,plot.conf=F,main="")
lines(surgicaltime.test,col=3,lty=1,lwd=3)

accuracy(meann,surgicaltime.test)
accuracy(naivem,surgicaltime.test)
accuracy(driftm,surgicaltime.test)
accuracy(snaivem,surgicaltime.test)

moving_average3<-forecast(ma(surgicaltime.train,order=3),h=30)


#ARIMA model

# the trend is not upward, its fluctuating. 
#in arima model, the data need to be stationary. 
# lets check the autocorrelation and partial autocorrelation,#lets check the stationary
acf(surgicaltime)
#There are very few spikes crossing blue line. the data is not much correlated. and the data is stationary
pacf(surgicaltime)
#check the augmented dickey fuller test to know the pvalue to be smaller than 0.05, if not do AIC
adf.test(surgicaltime)

#in this case ACF and ADF test shows that the data is stationary. If data is not stationary, auto,arima function takes care of it. 

#data:  surgicaltime
#Dickey-Fuller = -5.1859, Lag order = 6, p-value = 0.01
#alternative hypothesis: stationary

#(not required for this ,data as it is already stationary)
surgicalmodel<-auto.arima(surgicaltime, ic="aic", trace=TRUE)
surgicalmodel

# the best model ARIMA(1,0,0) P,d.q  (p, difference, moving average)

# check whether really stationary are not (not required for this ,data as it is already stationary)
acf(ts(surgicalmodel$residuals))
pacf(ts(surgicalmodel$residuals))

#Forecasting

surgicalforecast<-forecast(surgicalmodel,level=c(95),h=30)
surgicalforecast
plot(surgicalforecast)
#validate the forecast
Box.test(surgicalforecast,lag=1,type="Ljung-Box")

#Box-Ljung test
#checking whether the number increasing.
Box.test(surgicalforecast,lag=2,type="Ljung-Box")
Box.test(surgicalforecast,lag=3,type="Ljung-Box")
Box.test(surgicalforecast,lag=4,type="Ljung-Box")
Box.test(surgicalforecast,lag=5,type="Ljung-Box")
Box.test(surgicalforecast,lag=6,type="Ljung-Box")
Box.test(surgicalforecast,lag=7,type="Ljung-Box")
summary(surgicalforecast)


#SARIMA 
# 1) Plot the series and search for possible outliers. 
# 2) Stabilize the variance by transforming the data (Box-Cox).
# 3) Analyse the stationarity of the transformed series. 
# 4) If the series is not stationary, then we use differencing. 
# 5) Identify the seasonal model by analyzing the seasonal coefficients of the ACF and PACF
# 6) Identify the regular component by exploring the ACF and PACF of the residuals of the seasonal model.
# 7) Check the significance of the coefficients
# 8) Analyze the residuals:
# 9) Compare different models using AIC or SBC (M=p+q+P+Q):
ggtsdisplay(surgicaltime) # Seasonal differencing is mandatory
ggtsdisplay(surgicaltime,lag=200)   # Let's do a zoom

BoxCox.lambda(surgicaltime) # If Close to 1, so do nothing

surgicaltime.sdiff <- diff(surgicaltime, lag = 12, differences = 1)
ggtsdisplay(surgicaltime.sdiff)     # Requires seasonal differencing
# Do both
surgicaltime.rdiff.sdiff <- diff(surgicaltime.rdiff, lag = 12, differences = 1) 
ggtsdisplay(surgicaltime.rdiff.sdiff) # Sweet!

### Part II

# Fitting
arima.fit <- Arima(surgicaltime, order=c(0,1,0),
                   seasonal = list(order=c(0,1,1),period=12),
                   lambda = NULL,
                   include.constant = TRUE)

ggtsdisplay(arima.fit$residuals)
ggtsdisplay(arima.fit$residuals,lag=13)

arima.fit2 <- Arima(surgicaltime, order=c(0,1,1),
                    seasonal = list(order=c(0,1,1),period=12),
                    lambda = NULL, 
                    include.constant = TRUE)
ggtsdisplay(arima.fit2$residuals)
ggtsdisplay(arima.fit2$residuals,lag=80)
autoplot(arima.fit)
autoplot(arima.fit2)
summary(arima.fit)
summary(arima.fit2)

library(lmtest)
coef(arima.fit)
coef(arima.fit2)

autoplot(surgicaltime) +
  autolayer(arima.fit2$fitted,series="Fit")

library(ggplot2)
df <- data.frame(y=surgicaltime,x=arima.fit2$fitted)
ggplot(df,aes(x=x,y=y)) + geom_point() + 
  geom_smooth(method='lm',formula=y~x)

