# 1. Load packages -----
library(readxl)
library(forecast)
library(ggplot2)
library(tseries)
library(lmtest)

# 2. Import your dataset -----
data <- read_excel("tidy data set.xlsx")
View(data)

# 3. Convert Labels to a time series object ----
ts_data <- ts(data$Index, start = c(1980,1), end = c(2025,3), frequency = 12)
ts_data

# 4. Plot the original series -----
autoplot(ts_data, colour = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 0.5) +
  ggtitle("Time Series Plot of Commodity Net Export Price Index") +
  ylab("Commodity Net Export Price Index") +
  xlab("Year") +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title.x = element_text(size = 13, margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, margin = margin(r = 10)),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank()
  )

# 5. Selecting training and test set ------
n <- length(ts_data)
n
train_size <- floor(0.8 * n)

train_ts <- window(ts_data, end = c(time(ts_data)[train_size]))
test_ts  <- window(ts_data, start = c(time(ts_data)[train_size + 1]))

length(train_ts)  # 80% data
length(test_ts)   # 20% data

# 6. Plot the training set -----
 #time series plot
 autoplot(train_ts, colour = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 0.5) +
  ggtitle("Time Series Plot of Commodity Net Export Price Index") +
  ylab("Commodity Net Export Price Index") +
  xlab("Year") +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title.x = element_text(size = 13, margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, margin = margin(r = 10)),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank()
  )

 #seasonal plot ----
 ggseasonplot(train_ts, 
             year.labels = TRUE, 
             year.labels.left = TRUE) +
  geom_point(size = 0.5) +
  ggtitle("Seasonal Plot:Commodity Net Export Price Index ") +
  ylab("Commodity Net Export Price Index") +
  xlab("Month") +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title.x = element_text(size = 13, margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, margin = margin(r = 10)),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank()
  )

# 7. ACF for check seasonality and stationary -----
ggAcf(train_ts, lag.max = 72, col = "steelblue", fill = "steelblue", lwd = 1.1) +
  ggtitle("Autocorrelation Function (ACF)") +
  ylab("ACF") +
  xlab("Lag") +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title.x = element_text(size = 13, margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, margin = margin(r = 10)),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank()
  )

#  8. ADF test as a standard test to check stationary -----
adf_result <- adf.test(train_ts)
adf_result

# 9. Lag 1 differencing -----
train_ts_diff <- diff(train_ts, lag = 1)
train_ts_diff


# 10. ADF test on lag 1 differences of Commodity Net Export Price Index ------
adf_diff <- adf.test(train_ts_diff )
adf_diff

# 11. Plot the lag 1 differences ------
autoplot(train_ts_diff, colour = "steelblue", size = 0.7) +
  geom_point(color = "steelblue", size = 0.5) +
  ggtitle("Time Series Plot of Differenced Commodity Net Export Price Index") +
  ylab("Commodity Net Export Price Index") +
  xlab("Year") +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title.x = element_text(size = 13, margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, margin = margin(r = 10)),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank()
  )


# 12. ACF plot of lag 1 differences -----
ggAcf(train_ts_diff, lag.max = 72, col = "steelblue", fill = "steelblue", lwd = 1.1) +
  ggtitle("Autocorrelation Function (ACF)") +
  ylab("ACF") +
  xlab("Lag") +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title.x = element_text(size = 13, margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, margin = margin(r = 10)),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank()
  )


# 13. PACF plot for differenced data -----
ggPacf(train_ts_diff, lag.max = 72, col = "steelblue", lwd = 1.1) +
  ggtitle("Partial Autocorrelation Function (PACF)") +
  ylab("PACF") +
  xlab("Lag") +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title.x = element_text(size = 13, margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, margin = margin(r = 10)),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank()
  )

auto.arima(train_ts) # only for testing purpose. any result of this is not used in our study 

# here I identify that ARIMA(1,1,1) is a suitable model. But we compare it with some another possible models also


# 14. Estimating chosen models and finding a better model ------
model1 <- Arima(train_ts, order = c(1,1,1),include.drift = TRUE)
summary(model1)

model2 <- Arima(train_ts, order = c(0,1,1),include.drift = TRUE)
summary(model2)

model3 <- Arima(train_ts, order = c(1,1,0),include.drift = TRUE)
summary(model3)

model4 <- Arima(train_ts, order = c(0,1,0),include.drift = TRUE)
summary(model4)

model5 <- Arima(train_ts, order = c(2,1,2),include.drift = TRUE)
summary(model5)

model6 <- Arima(train_ts, order = c(2,1,1),include.drift = TRUE)
summary(model6)

model7 <- Arima(train_ts, order = c(1,1,2),include.drift = TRUE)
summary(model7)

model8 <- Arima(train_ts, order = c(2,1,0),include.drift = TRUE)
summary(model8)

model9 <- Arima(train_ts, order = c(0,1,2),include.drift = TRUE)
summary(model9)

model10 <- Arima(train_ts, order = c(1,1,3),include.drift = TRUE)
summary(model10)

model11 <- Arima(train_ts, order = c(3,1,1),include.drift = TRUE)
summary(model11)

model12 <- Arima(train_ts, order = c(3,1,2),include.drift = TRUE)
summary(model12)

model13 <- Arima(train_ts, order = c(2,1,3),include.drift = TRUE)
summary(model13)

model14 <- Arima(train_ts, order = c(3,1,3),include.drift = TRUE)
summary(model14)

model15 <- Arima(train_ts, order = c(3,1,0),include.drift = TRUE)
summary(model15)


# 15. Forecast for the length of the test set ------
fcast1 <- forecast(model1, h = length(test_ts))

fcast2 <- forecast(model2, h = length(test_ts))

fcast3 <- forecast(model3, h = length(test_ts))

fcast4 <- forecast(model4, h = length(test_ts))

fcast5 <- forecast(model5, h = length(test_ts))

fcast6 <- forecast(model6, h = length(test_ts))

fcast7 <- forecast(model7, h = length(test_ts))

fcast8 <- forecast(model8, h = length(test_ts))

fcast9 <- forecast(model9, h = length(test_ts))

fcast10 <- forecast(model10, h = length(test_ts))

fcast11 <- forecast(model11, h = length(test_ts))

fcast12 <- forecast(model12, h = length(test_ts))

fcast13 <- forecast(model13, h = length(test_ts))

fcast14 <- forecast(model14, h = length(test_ts))

fcast15 <- forecast(model15, h = length(test_ts))



# 16. Forecasting accuracy comparison with testing set ------
acc1 <- accuracy(fcast1, test_ts)
print(acc1)

acc2 <- accuracy(fcast2, test_ts)
print(acc2)

acc3 <- accuracy(fcast3, test_ts)
print(acc3)

acc4 <- accuracy(fcast4, test_ts)
print(acc4)

acc5 <- accuracy(fcast5, test_ts)
print(acc5)

acc6 <- accuracy(fcast6, test_ts)
print(acc6)

acc7 <- accuracy(fcast7, test_ts)
print(acc7)

acc8 <- accuracy(fcast8, test_ts)
print(acc8)

acc9 <- accuracy(fcast9, test_ts)
print(acc9)

acc10 <- accuracy(fcast10, test_ts)
print(acc10)

acc11 <- accuracy(fcast11, test_ts)
print(acc11)

acc12 <- accuracy(fcast12, test_ts)
print(acc12)

acc13 <- accuracy(fcast13, test_ts)
print(acc13)

acc14 <- accuracy(fcast14, test_ts)
print(acc14)

acc15 <- accuracy(fcast15, test_ts)
print(acc15)




# 17. Residual diagnostics ------

#ARIMA(2,1,3)
#Test assumption of independence of residuals
Box.test(residuals(model13), lag=24, type="Ljung" )

#Test normality of residuals
shapiro.test(residuals(model13))


# Extract residuals
resid_values <- residuals(model13)


#ACF plot of residuals
ggAcf(resid_values, lag.max = 72, col = "steelblue", fill = "steelblue", lwd = 1.1) +
  ggtitle("Autocorrelation Function (ACF)") +
  ylab("ACF") +
  xlab("Lag") +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title.x = element_text(size = 13, margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, margin = margin(r = 10)),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank()
  )


#PACF plot of residuals
ggPacf(resid_values, lag.max = 72, col = "steelblue", lwd = 1.1) +
  ggtitle("Partial Autocorrelation Function (PACF)") +
  ylab("PACF") +
  xlab("Lag") +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title.x = element_text(size = 13, margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, margin = margin(r = 10)),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank()
  )


#plot of residuals
autoplot(resid_values, colour = "steelblue", size = 0.7) +
  geom_point(color = "steelblue", size = 0.5) +
  ggtitle("Time Series Plot of Residuals") +
  ylab("Commodity Net Export Price Index") +
  xlab("Year") +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title.x = element_text(size = 13, margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, margin = margin(r = 10)),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank()
  )


# 18. Forecasting with the model -----

model_manual <- Arima(ts_data, order = c(2,1,3),
                      include.drift = TRUE,
                      fixed = (c(0.1130,0.8485,0.1279,-0.9217,-0.2062,-0.0022)))

# Forecast 12 periods ahead
fcast_manual <- forecast(model_manual, h = 12)
fcast_manual


autoplot(fcast_manual, series = "Forecast", size = 1.1) +   # Forecast line thicker
  autolayer(fitted(fcast_manual), series = "Fitted", size = 0.7) +  # Fitted line
  autolayer(fcast_manual$x, series = "Actual", size = 0.7) +        # Actual line
  scale_color_manual(
    values = c("Actual" = "steelblue", 
               "Fitted" = "firebrick", 
               "Forecast" = "darkgreen")
  ) +
  ggtitle("Time Series Plot of Commodity Net Export Price Index") +
  ylab("Commodity Net Export Price Index") +
  xlab("Year") +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title.x = element_text(size = 13, margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, margin = margin(r = 10)),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank()
  )
