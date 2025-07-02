# 06_prediction_visualization.R
# Plot actual vs predicted prices for ALV.DE

library(tidyverse)
library(ggplot2)
library(forecast)

# Load forecasts
df <- read_csv('alvde_forecasts.csv', show_col_types = FALSE)

# Actual vs ARIMA
p1 <- ggplot(df, aes(x = date)) +
  geom_line(aes(y = actual, color = 'Actual')) +
  geom_line(aes(y = arima, color = 'ARIMA')) +
  labs(title = 'ALV.DE: Actual vs ARIMA Predicted', y = 'Price') +
  scale_color_manual(values = c('Actual' = 'black', 'ARIMA' = 'blue'))

ggsave('prediction_arima_vs_actual_alvde.png', plot = p1)

# Actual vs Prophet
p2 <- ggplot(df, aes(x = date)) +
  geom_line(aes(y = actual, color = 'Actual')) +
  geom_line(aes(y = prophet, color = 'Prophet')) +
  labs(title = 'ALV.DE: Actual vs Prophet Predicted', y = 'Price') +
  scale_color_manual(values = c('Actual' = 'black', 'Prophet' = 'red'))

ggsave('prediction_prophet_vs_actual_alvde.png', plot = p2) 