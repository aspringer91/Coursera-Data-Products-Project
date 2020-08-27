setwd("/home/andspringer17/Developing Data Products/Shiny App Final Project/")
library(data.table)
library(zoo)
library(stringr)
library(ggplot2)
library(forecast)
library(plotly)
df <- fread("/home/andspringer17/Developing Data Products/Shiny App Final Project/quarterly_apt_rent.csv")
names(df) <- c("Quarter", "Average","One_One","Two_Two")
df[, c("Average", "One_One", "Two_Two") := list(str_remove(Average, ","),
                                                str_remove(One_One,","),
                                                str_remove(Two_Two, ","))]

df[, c("Average", "One_One", "Two_Two") := list(as.numeric(Average),
                                                as.numeric(One_One),
                                                as.numeric(Two_Two))]
df[, Quarter := as.yearqtr(Quarter)]

plot(x=df$Quarter,y=df$One_One)
g <- ggplot() +
  geom_line(data=df, aes(x=Quarter, y=One_One), color="red") +
  geom_line(data=df, aes(x=Quarter, y=Two_Two), color="blue") +
  geom_line(data=df, aes(x=Quarter, y=Average), color="purple") +
  xlab('Quarter') +
  ylab("Rental Rate") +
  theme(legend.justification = "left")
g

#make time series
oneOneTs <- ts(df[,"One_One"], start=c(1999,1),end=c(2016,2),frequency=4)
twoTwoTs <- ts(df[,"Two_Two"], start=c(1999,1),end=c(2016,2),frequency=4)
seasonalFit <- decompose(twoTwoTs)
plot(seasonalFit)

plot_ly(data=df[,c("Quarter","One_One")], x=~Quarter, y=~One_One, mode="lines")


last_date <- tail(df$Quarter, n=1)
current_date <- as.yearqtr("2020 Q3")
periods <- seq(1:((current_date - last_date)/.25))

oneOneFcst <- ses(oneOneTs,h=max(periods))
oneOneFit <- ets(oneOneTs, beta=0.1)
oneOneFcst <- forecast(oneOneFit,17)
plot(oneOneFcst)
arimaFit <- auto.arima(oneOneTs, D=2, seasonal = FALSE)
plot(forecast(arimaFit), h=17)

oneOneSma <- ma(housingTs[,"One_One"],order=12)
plot(oneOneSma)
?ets
library(tseries)
adf.test(oneOneTs)

#Predict on test set
housingTsTrain <- window(oneOneTs, start=1999, end=2012)
housingTsTest <- window(oneOneTs, start=2012, end=2016)
housingTsTrain
#simple moving average
library(forecast)
plot(housingTsTrain)
lines(ma(housingTsTrain,order=3),col="red")
#exponential smoothing
ets1 <- ets(housingTsTrain)
fcast <- forecast(ets1)
plot(fcast); lines(housingTsTest,col="red")
accuracy(fcast,ts1Test)
ets2 <- ets(oneOneTs)
fcast <- forecast(ets2)
plot(fcast)

startPeriod <- 2010
oneOneWindow <- window(oneOneTs, start = startPeriod, end=2016.25)
hwFit <- hw(oneOneWindow, h= 17)
plot(hwFit)
hwFit

#Predict since 2010
housingTsTrain <- window(twoTwoTs, start=1999, end=2016.25)
plot(housingTsTrain)
ets1 <- ets(twoTwoTs)
fcast <- forecast(ets1,h=17)
plot(fcast)

fcast

housingTsTrain
oneOneTs
mod <- lm(One_One ~ Quarter, data=df)
ses()
last_date <- tail(df$Quarter, n=1)
current_date <- as.yearqtr("2020 Q3")
periods <- seq(1:((current_date - last_date)/.25))
dates <- as.yearqtr(sapply(periods,function(x) {last_date + x/4}))
predDf <- data.frame(Quarter=dates)
predictions <- data.table(Quarter=dates, One_One = predict(mod, predDf))
plot(x=predictions$Quarter, y=predictions$One_One)
all <- rbind(df[,c("Quarter","One_One")],predictions)
plot(all$Quarter, all$One_One)
?predict
