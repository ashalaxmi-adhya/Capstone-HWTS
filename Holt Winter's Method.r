##################### Holt Winter's Method #################

library(forecast)
library(fpp)
library(TTR)
library(ggplot2)


y<-read.csv("C:/Users/aadhya/Documents/ASSIGNMENTS/GraphicTeeData.csv")
summary(y)
plot(y)
head(y)

## Create the time series object
y_ts <- ts(y$Full.Price.Sales.Qty, start = c(2017, 2), end = c(2019, 12), frequency = 12)
plot(y_ts)
class(y_ts)

options(repr.plot.width = 6, repr.plot.height = 5)
yDecomp <- decompose(y_ts)
plot(yDecomp)

## Perform Holt-Winters exponential smoothing
y_ts_hw <- HoltWinters(y_ts)
y_ts_hw
plot(y_ts_hw,
     col = "black",
     col.predicted = "blue")
x<- fitted(y_ts_hw)


## checking the model parameters
y_ts_hw$model

fitted(y_ts_hw$model)
forecast(y_ts_hw, h = 6)

## Model Accuracy
summary(y_ts_hw)
accuracy(y_ts_hw$fitted, y_ts)

plot(forecast(y_ts_hw, h = 6))


