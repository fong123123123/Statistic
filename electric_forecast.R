library(forecast)
library(tseries)

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
adf.test(diff_data)
adf.test(train)

#Step 4
#residuals of auto arima model
residuals <- residuals(arima_model)
residuals

#PACF && Acf of the residuals
acf_residuals <-acf(residuals, main="ACF of Residuals")
pacf_residuals <-pacf(residuals, main = "PACF of Residuals")

#PACF && ACF of Original Data
acf <- acf(train, main="Correlogram for the Electric Dataset")
pacf <- pacf(train, main = "Autocorrelogram")

#Step 6 Check residual
#Box-Pierce Q 
Box.test(residuals)

#Differencing

#Step 7
#Arima model
#arima_model <- arima(train, c(2,0,1), order = c(0,1,1), 12)
arima_model <- arima(train, order = c(2, 0, 1), seasonal = list(order = c(0, 1, 1), period = 12))
summary(arima_model)

#accuracy(train)

#ARIMA using auto.arima
arima_model <- auto.arima(train, ic= "aic", trace = TRUE)
summary(arima_model)

#Step 8 Form Equation for the Best Model



