#install.packages("forecast")
library(forecast)
library(tseries)
library(lmtest)


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

#Decomposition of Training Time Series
decomposed_data <- decompose(train)
plot(decomposed_data)
acf(train)
pacf(train)


# Analysis for Residual
#decomposed_data$random <- na.omit(decomposed_data$random)
#summary(decomposed_data$random)
#acf(decomposed_data$random, main = "ACF of Residual Component")
#pacf(decomposed_data$random, main = "PACF of Residual Component")



#======================================================
#Step 2 (Transformation)
#Log transform
train <- log(train)
train <- ts(train, frequency = 12, start = c(1985,1))
plot(train, main = "Time Series Plot (Log Transform)", xlab = "Time", ylab = "Value")
#======================================================
#Step 3 (Stationarize the Series)
#seasonal diff
seasonalDiff <- diff(train, lag= 12)
plot(seasonalDiff)
acf(seasonalDiff)
pacf(seasonalDiff)

#Check Non-Seasonal Stationarity
adf.test(seasonalDiff)

train <- seasonalDiff

#======================================================
#Step 4 (ETS, Arima model)
#ETS model
ets_model <- ets(train)
#Arima model
arima_model <- arima(train, order = c(2, 0, 1), seasonal = list(order = c(0, 1, 1), period = 12))
auto_arima_model <- auto.arima(train)

#Holt-Winters model
hw_model_additive <- HoltWinters(train, seasonal = "additive")

#summary 
summary(ets_model)
summary(arima_model)
summary(auto_arima_model)



#======================================================
#Step 6 (Diagnotic Checking)
checkresiduals(ets_model)
checkresiduals(arima_model)
checkresiduals(auto_arima_model)
checkresiduals(hw_model_additive)

#======================================================
#Step 7 (Determine Best Model)
accuracy(ets_model)
accuracy(arima_model)
accuracy(auto_arima_model)
accuracy(hw_model_additive)

#Ljung-Box Test 
#**(p value bigger than 0.05 is better, indicating there a white noise in residual)
#**H0: Model does not show lack of fit
#**H1: Model does show a lack of fit
Box.test(resid(ets_model),type="Ljung",lag=12)
Box.test(resid(arima_model),type="Ljung",lag=12)
Box.test(resid(auto_arima_model),type="Ljung",lag=12)

autoplot(forecast(arima_model))



#======================================================
#Step 8a (Form Equation for Best Model)


#======================================================
#Step 8b (Estimate model coefficient)
arima(data, order = c(0,1,1), seasonal = list(order=c(0,1,1), period = 12))

#======================================================
#Step 8c (Test significance of the coefficient)
#fit <- arima(data, order = c(0,1,1), seasonal = list(order=c(0,1,1), period = 12))
coeftest(auto_arima_model)

#======================================================

#Reference 
#https://www.statisticshowto.com/ljung-box-test/#:~:text=The%20null%20hypothesis%20of%20the,time%20series%20isn't%20autocorrelated.