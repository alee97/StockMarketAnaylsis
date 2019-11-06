library(quantmod);
library(tseries);
library(timeSeries);
library(forecast);
library(xts);


#use quantmod to grab data from google/yahoo finance
getSymbols('TTWO', from='2012-01-01', to='2019-10-25')
#these V and ^ must be the same
class(TTWO)
#get forth column which is the closing price
#the 3rd is the open prices
#the 4th provides the close price 
#2rd prvides the volume
SPY_Close_Prices = TTWO[,4]

#show the un adjusted data
plot(SPY_Close_Prices)
class(SPY_Close_Prices)

#adjust matrix to different dimensions
par(mfrow=c(1,1))
#autocorrect
Acf(SPY_Close_Prices, main='ACF for Differenced Series')
Pacf(SPY_Close_Prices, main='PACF for Differenced Series')



#start "training" the model
print(adf.test(SPY_Close_Prices))
auto.arima(SPY_Close_Prices, seasonal=FALSE)


fitA = auto.arima(SPY_Close_Prices, seasonal = FALSE)
tsdisplay(residuals(fitA),lag.max = 40, main='(0,1,0) Model Residuals')
auto.arima(SPY_Close_Prices, seasonal = FALSE)

fitB = arima(SPY_Close_Prices, order = c(1,2,4))
tsdisplay(residuals(fitB), lag.max = 40, main='(1,2,4) Model Residuals')

fitC = arima(SPY_Close_Prices, order = c(5,1,4))
tsdisplay(residuals(fitC), lag.max = 40, main='(5,1,4) Model Residuals')

fitD = arima(SPY_Close_Prices, order = c(1,1,1))
tsdisplay(residuals(fitD), lag.max = 40, main='(1,1,1) Model Residuals')

fitE= arima(SPY_Close_Prices, order = c(3,1,4))
tsdisplay(residuals(fitE), lag.max = 40, main='(3,1,4) Model Residuals')


par(mfrow = c(2,2))

#Display the models
term<-100
#h frovides the scpe of the end of the graph of howmuch of the orginal/pridiction goes
fcast1<-forecast(fitA, h=term)
plot(fcast1)
plot(fcast1,20)
plot(fcast1,50)
plot(fcast1,100)


fcast2<-forecast(fitB, h=term)
plot(fcast2)

fcast3<-forecast(fitC, h=term)
plot(fcast3)

fcast4<-forecast(fitD, h=term)
plot(fcast4)

plot(forecast(fitE, h=term))


#get accuracy in terms of MAPE
accuracy(fcast1)
accuracy(fcast2)
accuracy(fcast3)
accuracy(fcast4)
accuracy(fcast5)

                  
