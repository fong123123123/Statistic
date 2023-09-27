#install.packages("forecast")
library(forecast)
library(tseries)
library(lmtest)


#Step 0
data <- read.csv("Electric_Production.csv")
data()

#train test split
train <- head(data, round(length(data) * 0.70))
h <- length(data) - length(train)
test <- tail(data, h)
train
test
autoplot(train) + autolayer(test)

#======================================================
#Step 1 (Visualise Time Series)
#plot data 
data <- ts(data[,2], frequency = 12, start = c(1985,1))
plot(data, main = "Time Series Plot", xlab = "Time", ylab = "Value")

#Decomposition of Time Series
decomposed_data <- decompose(data)
plot(decomposed_data)

# Analysis for Residual
test <- decomposed_data$random
summary(decomposed_data$random)
acf(test, main = "ACF of Residual Component")
pacf(test, main = "PACF of Residual Component")

#======================================================
#Step 2 (Transformation)
#Log transform
train <- log(train)
train <- ts(train, frequency = 12, start = c(1985,1))
plot(train, main = "Time Series Plot (Log Transform)", xlab = "Time", ylab = "Value")

#======================================================
#Step 3 (Stationarize the Series)
#Check Stationarity
adf.test(train)


#======================================================
#Step 4 (ETS, Arima model)
#ETS model
ets_model_addictive <- ets(train, model = "AAA")
ets_model_multiplicative <- ets(train, model = "MMM")
ets_model_mixed <- ets(train, model = "ZZZ")
#Arima model
arima_model <- arima(train, order = c(2, 0, 1), seasonal = list(order = c(0, 1, 1), period = 12))
auto_arima_model <- auto.arima(train)
#Holt-Winters model
hw_model_additive <- HoltWinters(train, seasonal = "additive")



#======================================================
#Step 6 (Diagnotic Checking)
checkresiduals(ets_model_addictive)
checkresiduals(ets_model_multiplicative)
checkresiduals(ets_model_mixed)
checkresiduals(arima_model)
checkresiduals(auto_arima_model)

#======================================================
#Step 7 (Determine Best Model)
accuracy(arima_model)
accuracy(auto_arima_model)
accuracy(ets_model_addictive)
accuracy(ets_model_multiplicative)
accuracy(ets_model_mixed)

#======================================================
#Step 8a (Form Equation for Best Model)

#======================================================
#Step 8b (Estimate model coefficient)
arima(data, order = c(0,1,1), seasonal = list(order=c(0,1,1), period = 12))

#======================================================
#Step 8c (Test significance of the coefficient)
fit <- arima(data, order = c(0,1,1), seasonal = list(order=c(0,1,1), period = 12))
coeftest(fit)

#======================================================