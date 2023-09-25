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

#Step 2
#transformation
data_transform <- log(data)
data_transform <- ts(data_transform, frequency = 12, start = c(1985,1))
plot(data_transform, main = "Time Series Plot (Log Transform)", xlab = "Time", ylab = "Value")

#Step 3a Stationarize the Series
#Differencing
# First-order differencing
diff_data <- diff(data_transform)

# Plot differenced data
plot(diff_data, main = "Differenced Time Series", xlab = "Time", ylab = "Differences")

#Step 3b Check the stationarity of the series
adf.test(diff_data)

#Step 4
#auto arima model
arima_model <- auto.arima(train)
summary(arima_model)

#residuals of auto arima model
residuals <- residuals(arima_model)
residuals

#acf of the residuals
acf_residuals <-acf(residuals, main="ACF of Residuals")
ACF$acf_residuals


#ACF
acf <- acf(train, main="Correlogram for the Electric Dataset")
ACF$acf

