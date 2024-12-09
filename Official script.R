dataset <- read.csv("dataset.csv")


range(dataset$Date)

library(knitr)

kable(head(dataset, 10), caption = "Table 1: First 10 Exchange Rates")

inds <- seq(as.Date("2005-01-03"), as.Date("2023-11-23"), by = "day")

range(dataset$Date)
 
## Create a time series object
rate <- dataset[3]

rate.ts <- ts(rate, start = c(2005, as.numeric(format(inds[1], "%j"))),
              frequency = 365)
plot.ts(rate.ts, main = "Figure 1: Time Plot of Kenyan Shillings - US Dollar Exchange Rate", col = "blue")

decomposed.series <- decompose(rate.ts)

# Define a plot function to allows us change the title

decomp.plot <- function(x, main = NULL, ...)
{
  if(is.null(main))
    main <- paste("Decomposition of", x$type, "time series")
  plot(cbind(observed = x$random + if (x$type == "additive")
    x$trend + x$seasonal
    else x$trend * x$seasonal, trend = x$trend, seasonal = x$seasonal,
    random = x$random), main = main, ...)
}

decomp.plot(decomposed.series, col = "blue", main = " Decomposed Series Plot")

library(tseries)

adf.test(rate.ts, alternative = "stationary")

differenced.series <- diff(rate.ts, differences = 1)

decomp.plot(decompose(differenced.series), col = "red", main = " Decomposed Differences Series Plot")

adf.test(differenced.series, alternative = "stationary")

model <- HoltWinters(differenced.series ,beta=FALSE,gamma=FALSE)

plot(model, main =" Holts-Winters Filtering", col = "blue")

library(forecast)

forecasts1 <- forecast:::forecast.HoltWinters(model,h=60)

plot(forecasts1, main = " Forecasts from HoltWinters")

hist(forecasts1$residuals, col="blue", main = " Histogram of Forecast Residuals", xlab = "Residuals")
