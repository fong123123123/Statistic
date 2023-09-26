library(forecast)
library(tseries)
library(forecast)

#Step 0
data <- read.csv("Electric_Production.csv")
data()

#plot data 
data <- ts(data[,2], frequency = 12, start = c(1985,1))
plot(data, main = "Time Series Plot", xlab = "Time", ylab = "Value")

#train test split
train <- head(data, round(length(data) * 0.70))
h <- length(data) - length(train)
test <- tail(data, h)
train
test
autoplot(train) + autolayer(test)

#Step 1 Decomposition
decomposed_data <- decompose(train)
# Plot the decomposed components
plot(decomposed_data)

#Step 2
#transformation
data_transform <- log(data)
data_transform <- ts(data_transform, frequency = 12, start = c(1985,1))
plot(data_transform, main = "Time Series Plot (Log Transform)", xlab = "Time", ylab = "Value")

#Step 3a Stationarize the Series
#Differencing
# First-order differencing
diff_data <- diff(data_transform)
seasonalDiff <- diff(data_transform, lag = 12)

# Plot differenced data
plot(diff_data, main = "Differenced Time Series", xlab = "Time", ylab = "Differences")
plot(seasonalDiff)

#Step 3b Check the stationarity of the series
adf.test(train)

#Step 4 ETS AND ARIMA
arima_model <- arima(train, order = c(2, 0, 1), seasonal = list(order = c(0, 1, 1), period = 12))

#residuals of auto arima model
residuals <- residuals(arima_model)
residuals

#PACF && Acf of the residuals
acf_residuals <-acf(residuals, main="ACF of Residuals")
pacf_residuals <-pacf(residuals, main = "PACF of Residuals")

#PACF && ACF of Original Data
acf <- acf(train, main="Correlogram for the Electric Dataset")
pacf <- pacf(train, main = "Partial AutoCorrelation")

#Step 6 Check residual
#Box-Pierce Q 
Box.test(residuals)
adf.test(residuals)

#Differencing

#Step 7 Compare ETS AND Arima
#Arima model
sarima_model <- arima(train, c(1, 0, 0), seasonal = list(order = c(2, 1, 0), period = 12))
summary(sarima_model)
coef(sarima_model)
Box.test(sarima_model$residual, 12)
acf_ <-acf(sarima_model$residuals, main="ACF of Arima(Manually Guess) Residuals")

forecast_value <- forecast(sarima_model, h=120)
autoplot(forecast_value) + autolayer(fitted(sarima_model), series = "Fitted")

error = test - forecast_value$mean
mse = mean(error*error)
print(mse)

rmse = sqrt(mse)
print(rmse)

#ARIMA using auto.arima
arima_model <- auto.arima(train, ic= "aic", trace = TRUE)
summary(arima_model)
coef(arima_model)
Box.test(arima_model$residuals)
acf_ <-acf(arima_model$residuals, main="ACF of Arima(Auto) Residuals")

forecast_values <- forecast(arima_model, h=120)
autoplot(forecast_values) + autolayer(fitted(sarima_model), series = "Fitted")
error = test - forecast_values$mean
mse = mean(error*error)
print(mse)

rmse = sqrt(mse)
print(rmse)


#Step 8 Form Equation for the Best Model


