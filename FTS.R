library(DT)
library(readxl)
library(tidyverse)
packages =  c("ggplot2", "dplyr", "tidyr", "data.table", 'corrplot', 'gridExtra', 
              'forecast', 'tseries', 'TSA', 'tibble', 'TTR', "fpp2", "TSstudio", 
              "zoo", "xts", "recipes")


my.install <- function(pkg, ...){
  if (!(pkg %in% installed.packages()[,1])) {
    install.packages(pkg)
  }
  return (library(pkg, ...))
}

purrr::walk(packages, my.install, character.only = TRUE, warn.conflicts = FALSE)

# Import the dataset subsetted as KQ.


data <- read_excel("C:/Users/jerem/Downloads/Daily Average Prices2010-2013....xls")
head(data, 10)

data[is.na(data)] <- 0
data$DATE_TRADE <- as.Date(data$DATE_TRADE, format = "%Y-%m-%d")
summary(data)

str(data)

data %>%
  datatable()


options(repr.plot.width=12, repr.plot.height=12) 

p1 <- ggplot(data, aes(`AVG PRICE`)) + 
  geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) +
  geom_density()# + xlim(c(0, 1000))
p1



# Kolmogorov Smirnov Test


# Kolmogorov-Smirnov Test
ks.test(data$`AVG PRICE`, "pnorm")

# Jarque Bera Test


# Jarque Bera Test
jarque.bera.test(data$`AVG PRICE`)
# The data is normally distributed as the p-value is less than 0.05

# Time Series Decomposition


# Create time series
inds <- seq(as.Date("2010-01-04"), as.Date("2010-12-31"), by = "month")

# Decomposition of the time series
istart <- c( 2010, as.numeric(format(inds[1], "%j")))

msts( as.numeric(data$`AVG PRICE`), 
          start = istart,
          seasonal.periods = c(7,365.25) ) %>% 
  mstl(., lambda = 'auto')  %>% autoplot()

# Dickey-Fuller Test for Stationarity.


# Stationarity using Dickey-Fuller test
adf.test(data$`AVG PRICE`, alternative = "stationary", k = 0)

# Convert the AVG PRICE variable of data to ts data.


tsdata <- SMA(data$`AVG PRICE`, n=5)

# Selecting a candidate arima model and Autocorrelation plot


tsdata_diff <- diff(tsdata, differences=1) # Differencing the tsdata
acf(na.omit(tsdata_diff), lag.max = 30)

# Partial autocorrelation plot

pacf(na.omit(tsdata_diff), lag.max = 30)

# Creating an ARIMA(2,1,2) model


# ARMIA model
arima_model <- arima(log(tsdata), order=c(2,1,2))## seasonal = list(order = c(2,1,2), period = 12))
arima_model

arima_mdl <- auto.arima(tsdata, max.p = 3, max.q =3, max.d = 3)
summary(arima_mdl)

# Forecasting using the created arima_mdl model.

ts_forecast <- forecast(arima_mdl, h = 80, level=c(99.5))
autoplot(ts_forecast) + theme_bw()

# The p-value of the ljung box text is 2.2e-16 less than 0.05 indicating there is non-zero autocorrelations in lags 1-30


Box.test(arima_mdl$residuals, lag = 30, type = "Ljung-Box")


# Plot the Residuals of arima_mdl model.


autoplot(arima_mdl$residuals)


# Plot a histogram of the arima model (arima_mdl) residuals.



plotForecastErrors <- function(forecasterrors){
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}


plotForecastErrors(arima_mdl$residuals)


ggplot(data.frame(residuals = arima_mdl$residuals), aes(residuals)) + 
  geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + 
  geom_density()# make a histogram

The forecast errors have a negative mean rather than a zero mean. We can confirm this by finding the mean of residuals.


mean(arima_mdl$residuals)

