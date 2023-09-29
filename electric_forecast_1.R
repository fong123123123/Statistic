#install.packages("forecast")
library(forecast)
library(tseries)
library(lmtest)
library(seasonal)


#Step 0
data <- read.csv("Electric_Production.csv")
data <- ts(data[,2], frequency = 12, start = c(1985,1))


#train test split
train <- head(data, round(length(data) * 0.70))
h <- length(data) - length(train)
test <- tail(data, h)
autoplot(train) + autolayer(test)

#======================================================
#Step 1 (Visualise Time Series)
#plot data 
plot(data, main = "Time Series Plot", xlab = "Time", ylab = "Value") 

#check seasonal
ggseasonplot(data, year.labels=TRUE, year.labels.left=TRUE) 


#Decomposition of Time Series
decomposed_data <- decompose(data)
plot(decomposed_data)
acf(data)
pacf(data)

# Analysis for Residual
#decomposed_data$random <- na.omit(decomposed_data$random)
#summary(decomposed_data$random)
#acf(decomposed_data$random, main = "ACF of Residual Component")
#pacf(decomposed_data$random, main = "PACF of Residual Component")



#======================================================
#Step 2 (Transformation)
#Log transform
#train_log <- log(train)
#train_log <- ts(train_log, frequency = 12, start = c(1985,1))
#plot(train_log, main = "Time Series Plot (Log Transform)", xlab = "Time", ylab = "Value")
#decomposed_data <- decompose(train_log)
#plot(decomposed_data)
#======================================================
#Step 3 (Stationarize the Series)
#seasonal diff
seasonalDiff <- diff(data, lag= 12)
plot(seasonalDiff)
decomposed_data <- decompose(seasonalDiff)
plot(decomposed_data)
acf(seasonalDiff)
pacf(seasonalDiff)

#Check Non-Seasonal Stationarity
adf.test(seasonalDiff)
kpss.test(seasonalDiff)

#train <- seasonalDiff

#======================================================
#Step 4 (ETS, Arima model)
#ETS model
ets_model <- ets(train)
#Arima model
arima_model <- arima(train, order = c(3, 0, 4), seasonal = list(order = c(2,1,1), period = 12))


auto_arima_model <- auto.arima(train)


#Holt-Winters model
hw_model_additive <- HoltWinters(train, seasonal = "additive")

tbats_model <- tbats(train)

#summary 
summary(ets_model)
summary(arima_model)
summary(auto_arima_model)
summary(hw_model_additive)
summary(tbats_model)


#======================================================
#Step 6 (Diagnotic Checking)
checkresiduals(ets_model)
checkresiduals(arima_model)
checkresiduals(auto_arima_model)
checkresiduals(hw_model_additive)
checkresiduals(tbats_model)

#======================================================
#Step 7 (Determine Best Model)
accuracy(ets_model)
accuracy(arima_model)
accuracy(auto_arima_model)
accuracy(tbats_model)

forecast_ets_model <- forecast(ets_model,h = length(test))
forecast_arima_model <-forecast(arima_model,h = length(test))
forecast_auto_arima_model <-forecast(auto_arima_model,h = length(test))
forecast_hw_model_additive <-forecast(hw_model_additive,h = length(test))
forecast_tbats_model <-forecast(tbats_model,h = length(test))

accuracy(forecast_ets_model)
accuracy(forecast_arima_model)
accuracy(forecast_auto_arima_model)
accuracy(forecast_hw_model_additive)
accuracy(forecast_tbats_model)


#Ljung-Box Test 
#**(p value bigger than 0.05 is better, indicating there a white noise in residual)
#**H0: Model does not show lack of fit
#**H1: Model does show a lack of fit
Box.test(resid(ets_model),type="Ljung",lag=12)
Box.test(resid(arima_model),type="Ljung",lag=12)
Box.test(resid(auto_arima_model),type="Ljung",lag=12)
Box.test(resid(hw_model_additive),type="Ljung",lag=12)
Box.test(resid(tbats_model),type="Ljung",lag=12)


# Plot the SARIMA forecasts and actual data
sarima_forecasts <- forecast(arima_model, h = length(test))
plot(sarima_forecasts, main = "SARIMA Forecast vs. Actual")
lines(test, col = "red")
legend("topright", legend = c("Forecast", "Actual"), col = c("blue", "red"),lty = 1)
accuracy(sarima_forecasts,test)
autoarima_forcast <- forecast(auto_arima_model, h = length(test))
accuracy(autoarima_forcast,test)

#======================================================
#Step 8b (Estimate model coefficient)
coef(arima_model)
#======================================================
#Step 8c (Test significance of the coefficient)
#fit <- arima(data, order = c(0,1,1), seasonal = list(order=c(0,1,1), period = 12))
coeftest(arima_model)
#======================================================

#Reference 
#https://www.statisticshowto.com/ljung-box-test/#:~:text=The%20null%20hypothesis%20of%20the,time%20series%20isn't%20autocorrelated.

