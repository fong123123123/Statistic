library(forecast)
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

#log transform
data_transform <- log(data)
data_transform <- ts(data_transform, frequency = 12, start = c(1985,1))
plot(data_transform, main = "Time Series Plot", xlab = "Time", ylab = "Value")

#auto arima model
arima_model <- auto.arima(data)
summary(arima_model)

#residuals of auto arima model
residuals <- residuals(arima_model)
residuals

#acf of the residuals
acf_residuals <-acf(residuals, main="ACF of Residuals")
ACF$acf_residuals


#correlogram ACF
acf <- acf(data, main="Correlogram for the Electric Dataset")
ACF$acf
