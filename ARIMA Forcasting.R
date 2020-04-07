library("forecast")
library("tseries") 
library("MASS")

# For Facebook
fb_data <- read.csv("FB.csv")
head(fb_data)
tail(fb_data)
# Pulling Closing Price of FB stock price
fb_price <- fb_data$Close

# Plotting the closing price over the last year
yy <-  ts(fb_price, frequency = 251, start = c(2019,42), end = c(2020,38))
plot(yy)			

# Checking if stationary 
adf.test(yy)

# Series is not stationary hence we differciate
yd <-  diff(yy,differences = 1)			
plot.ts(yd)	
# Checking if stationary 
adf.test(yd)
# Small pvalue (< 0.10) so yd is stationar and d = 1

# Determining AR(q)
Pacf(yd, lag.max = 70)	
# p = 4

#Determining MA(p)
Acf(yd, lag.max = 70)	
# q = 4

# Fitting auto ARIMA model
auto <-  auto.arima(yy)	
auto.predict <- forecast:::forecast.Arima(auto, h = 20, level = c(68, 90))
plot(auto.predict)
auto.predict
auto

# 1st model ARIMA(4,1,4)
m1 = arima(yy, order=c(4,1,4))
m1.predict <- forecast:::forecast.Arima(m1, h = 10, level = c(68, 90))
plot(m1.predict)
m1.predict
m1
#aic = 1267.58 

# 2nd model ARIMA(5,1,4)
m2 = arima(yy, order=c(5,1,4))
m2.predict <- forecast:::forecast.Arima(m2, h = 10, level = c(68, 90))
plot(m2.predict)
m2.predict
m2
# aic = 1266.4

# 3rd model ARIMA(3,2,4)
m3 = arima(yy, order=c(3,2,4))
m3.predict <- forecast:::forecast.Arima(m3, h = 10, level = c(68, 90))
plot(m3.predict)
m3.predict
m3
# aic = 1271.83

# 4th model ARIMA(4,1,3)
m4 = arima(yy, order=c(4,1,3))
m4.predict <- forecast:::forecast.Arima(m4, h = 10, level = c(68, 90))
plot(m4.predict)
m4.predict
m4
# aic = 1266.57

# 5th model ARIMA(4,1,5)
m5 = arima(yy, order=c(4,1,5))
m5.predict <- forecast:::forecast.Arima(m5, h = 10, level = c(68, 90))
plot(m5.predict)
m5.predict
m5
# aic = 1272.4

m6 = arima(yy, order=c(5,1,5))
m6.predict <- forecast:::forecast.Arima(m6, h = 10, level = c(68, 90))
plot(m6.predict)
m6.predict
m6
# aic = 1265.07

m7 = arima(yy, order=c(5,1,5), seasonal = list(order = c(35,1,8), period = 4))
m7
# aic = 1257.7

m8 = arima(yy, order=c(5,1,5), seasonal = list(order = c(0,1,2), period = 12))
m8
# aic = 1238.91

m9 = arima(yy, order=c(5,1,5), seasonal = list(order = c(0,1,1), period = 12))
m9.predict <- forecast:::forecast.Arima(m9, h = 100, level = c(68, 90))
plot(m9.predict)
m9.predict
m9
# aic = 1237.66

m10 = arima(yy, order=c(6,1,4), seasonal = list(order = c(0,1,1), period = 52))
m10.predict <- forecast:::forecast.Arima(m10, h = 100, level = c(68, 90))
plot(m10.predict)
m10.predict
m10
#aic = 1070.26

Acf(residuals(m10))

apple_data <- read.csv("AAPL.csv")
head(apple_data)
tail(apple_data)
apple_price <- apple_data$Close
nrow(apple_data)

yy <-  ts(apple_price, frequency = 251, start = c(2019,42), end = c(2020,38))
plot.ts(yy)

adf.test(yy)
yd <-  diff(yy,differences = 1)			
adf.test(yd)	
# d = 1

Pacf(yd, lag.max = 251)	
# p = 4

Acf(yd, lag.max = 251)	
# q = 2


auto <-  auto.arima(yy)	
auto.predict <- forecast:::forecast.Arima(auto, h = 100, level = c(68, 90))
plot(auto.predict)
auto.predict
auto
# AIC=1297.74

m1 = arima(yy, order=c(3,2,1))
m1
m1.predict <- forecast:::forecast.Arima(m1, h = 100, level = c(68, 90))
plot(m1.predict)
m1.predict
# aic = 1311.51
# 4,1,1
# aic = 1307.82
# 3,2,1

m2 = arima(yy, order=c(4,1,1), seasonal = list(order = c(0,1,1), period = 52))
m2
m2.predict <- forecast:::forecast.Arima(m2, h = 21, level = c(68, 90))
plot(m2.predict)
m2.predict
# aic = 1101.16
# 411 011

# aic = 1115.75
# 311 011
# aic = 1105.36
# 311 012
# aic = 1103.78
# 311 011



amazon_data <- read.csv("AMZN.csv")
head(amazon_data)
tail(amazon_data)
amazon_price <- amazon_data$Close
nrow(amazon_data)

yy <-  ts(amazon_price, frequency = 251, start = c(2019,42), end = c(2020,38))
plot.ts(yy)

adf.test(yy)
yd <-  diff(yy,differences = 1)			
adf.test(yd)	
# d = 1

Pacf(yd, lag.max = 251)	
# p = 1

Acf(yd, lag.max = 251)	
# q = 1


auto <-  auto.arima(yy)	
auto.predict <- forecast:::forecast.Arima(auto, h = 100, level = c(68, 90))
plot(auto.predict)
auto.predict
auto
# AIC=2280.6 

m1 = arima(yy, order=c(1,1,0), seasonal = list(order = c(0,1,0), period =124))
m1
m1.predict <- forecast:::forecast.Arima(m1, h = 21, level = c(68, 90))
plot(m1.predict)
m1.predict
# aic = 1249.61

m1 = arima(yy, order=c(1,1,0), seasonal = list(order = c(0,1,1), period =240))
m1
m1.predict <- forecast:::forecast.Arima(m1, h = 100, level = c(68, 90))
plot(m1.predict)
m1.predict


# aic = 1452.45 110 011 m = 100
acf(residuals(m1))
# aic = 1884.01
# aic = 1881.99 110 011
# aic = aic = 1880.01 010 011

netflix_data <- read.csv("NFLX.csv")
head(netflix_data)
tail(netflix_data)
netflix_price <- netflix_data$Close
nrow(netflix_data)

yy <-  ts(netflix_price, frequency = 251, start = c(2019,42), end = c(2020,38))
plot.ts(yy)

adf.test(yy)
yd <-  diff(yy,differences = 1)			
adf.test(yd)	
# d = 1

Pacf(yd, lag.max = 251)	
# p = 3

Acf(yd, lag.max = 251)	
# q = 3


auto <-  auto.arima(yy)	
auto.predict <- forecast:::forecast.Arima(auto, h = 100, level = c(68, 90))
plot(auto.predict)
auto.predict
auto
# AIC=1654.33

m1 = arima(yy, order=c(3,1,2), seasonal = list(order = c(0,1,1), period =240))
m1
m1.predict <- forecast:::forecast.Arima(m1, h = 21, level = c(68, 90))
plot(m1.predict)
m1.predict
# aic = 1388.51
# 312 011

google_data <- read.csv("GOOG.csv")
head(google_data)
tail(google_data)
google_price <- google_data$Close
nrow(google_data)

yy <-  ts(google_price, frequency = 251, start = c(2019,42), end = c(2020,38))
plot.ts(yy)

adf.test(yy)
yd <-  diff(yy,differences = 1)			
adf.test(yd)	
# d = 1

Pacf(yd, lag.max = 251)	
# p = 5

Acf(yd, lag.max = 251)	
# q = 2


auto <-  auto.arima(yy)	
auto.predict <- forecast:::forecast.Arima(auto, h = 100, level = c(68, 90))
plot(auto.predict)
auto.predict
auto
# AIC=2125.56 

m1 = arima(yy, order=c(4,1,3), seasonal = list(order = c(0,1,1), period = 52))
m1
m1.predict <- forecast:::forecast.Arima(m1, h = 21, level = c(68, 90))
plot(m1.predict)
m1.predict
# aic = 1757
# 413 011
