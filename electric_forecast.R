library(forecast)
library(tseries)
library(forecast)
library(lmtest)

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
# 1.Arima model
sarima_model <- arima(train, c(1, 0, 0), seasonal = list(order = c(2, 1, 0), period = 12))
summary(sarima_model)


#Step 6: Diagnostic Checking (Randomness)
Box.test(sarima_model$residual, 12)

#Step 8(a): Form Equation for the Best Model
acf_ <-acf(sarima_model$residuals, main="ACF of Arima(Manually Guess) Residuals")

#Step 8b Estimate the model’s coefficients
coef(sarima_model)

#Step 8(c): Test the significance of the coefficients
coeftest(sarima_model)

#Step 9: Forecasting
forecast_value <- forecast(sarima_model, h=120)
autoplot(forecast_value) + autolayer(fitted(sarima_model), series = "Fitted")

error = test - forecast_value$mean
mse = mean(error*error)
print(mse)

rmse = sqrt(mse)
print(rmse)

# 2.ARIMA using auto.arima
arima_model <- auto.arima(train, ic= "aic", trace = TRUE)
summary(arima_model)

#Step 6: Diagnostic Checking (Randomness)
Box.test(arima_model$residuals, lag=12)

#Step 8(a): Form Equation for the Best Model
acf_ <-acf(arima_model$residuals, main="ACF of Arima(Auto) Residuals")

#Step 8b Estimate the model’s coefficients
coef(arima_model)

#Step 8(c): Test the significance of the coefficients
coeftest(arima_model)

#Step 9: Forecasting
forecast_values <- forecast(arima_model, h=120)
autoplot(forecast_values) + autolayer(fitted(sarima_model), series = "Fitted")
error = test - forecast_values$mean
mse = mean(error*error)
print(mse)

rmse = sqrt(mse)
print(rmse)


# 3.ETS
fit <- ets(train)
summary(fit)

#Step 6: Diagnostic Checking (Randomness)
Box.test(fit$residuals, lag=12)

#Step 8(a): Form Equation for the Best Model
acf_ <-acf(fit$residuals, main="ACF of Fit(ETS) Residuals")

#Step 8b Estimate the model’s coefficients
coef(fit)

#Step 9: Forecasting
forecast_values1 <- forecast(fit, h=120)
autoplot(forecast_values1) + autolayer(fitted(fit), series = "Fitted")
error = test - forecast_values1$mean
mse = mean(error*error)
print(mse)

rmse = sqrt(mse)
print(rmse)

# 4. Holt-Winters Model

# Fit an additive Holt-Winters model
holtwinter_add <- HoltWinters(train, seasonal = "additive")
summary(holtwinter_add)

# Extract the residuals
residuals_additive <- resid(holtwinter_add)

# analyze the residuals
coef(holtwinter_add)

print(residuals_additive)

plot(residuals_additive, main = "Residuals of Holt-Winters Model", xlab = "Time", ylab = "Residuals")

acf(residuals_additive, main = "ACF of Residuals of Holt-Winters Model")
pacf(residuals_additive, main = "PACF of Residuals of Holt-Winters Model")

#Step 6: Diagnostic Checking (Randomness)
Box.test(residuals(holtwinter_add), lag=12, type="Ljung-Box")

#Step 9: Forecasting
forecast_values <- forecast(holtwinter_add, h = 120)
autoplot(forecast_values) + autolayer(fitted(holtwinter_add), series = "Fitted")
print(forecast_values)

plot(forecast_values, main = "Holt-Winters Forecast", xlab = "Time", ylab = "Value")
lines(test, col = "red") 

actual_values <- test  # Replace 'test' with your actual test data
forecasted_values <- forecast_values$mean
mae <- mean(abs(actual_values - forecasted_values))
mse <- mean((actual_values - forecasted_values)^2)
rmse <- sqrt(mse)

# Print the accuracy metrics
cat("MAE:", mae, "\n")
cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")
