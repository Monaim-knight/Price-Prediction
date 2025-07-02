# 05_model_development.R
# Model development: ARIMA and Prophet for ALV.DE

if (!require(forecast)) install.packages('forecast')
if (!require(prophet)) install.packages('prophet')
library(forecast)
library(prophet)
library(tidyverse)

df <- read_csv('alvde_features.csv', show_col_types = FALSE)

ts_data <- ts(df$adjusted, frequency = 252)
train_size <- floor(0.8 * length(ts_data))
train_ts <- ts_data[1:train_size]
test_ts <- ts_data[(train_size+1):length(ts_data)]

# ARIMA
fit_arima <- auto.arima(train_ts)
forecast_arima <- forecast(fit_arima, h = length(test_ts))
rmse_arima <- sqrt(mean((forecast_arima$mean - test_ts)^2, na.rm=TRUE))
mape_arima <- mean(abs((forecast_arima$mean - test_ts)/test_ts), na.rm=TRUE)
cat('ARIMA RMSE:', rmse_arima, 'MAPE:', mape_arima, '\n')

# Prophet
df_prophet <- df %>% select(ds = date, y = adjusted) %>% na.omit()
m <- prophet(df_prophet[1:train_size, ])
future <- make_future_dataframe(m, periods = length(test_ts))
forecast_prophet <- predict(m, future)
preds <- tail(forecast_prophet$yhat, length(test_ts))
rmse_prophet <- sqrt(mean((preds - test_ts)^2, na.rm=TRUE))
mape_prophet <- mean(abs((preds - test_ts)/test_ts), na.rm=TRUE)
cat('Prophet RMSE:', rmse_prophet, 'MAPE:', mape_prophet, '\n')

write_csv(data.frame(date = df$date[(train_size+1):length(ts_data)],
                     actual = test_ts,
                     arima = as.numeric(forecast_arima$mean),
                     prophet = preds),
          'alvde_forecasts.csv') 