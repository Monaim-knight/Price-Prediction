# 04_feature_engineering.R
# Feature engineering: add technical indicators

if (!require(TTR)) install.packages('TTR')
library(TTR)
library(tidyverse)

# Load preprocessed data
df <- read_csv('german_stock_data_preprocessed.csv', show_col_types = FALSE)

df_alv <- df %>% filter(symbol == 'ALV.DE') %>% arrange(date)

# Calculate indicators outside mutate
sma_20 <- SMA(df_alv$adjusted, n = 20)
rsi_14 <- RSI(df_alv$adjusted, n = 14)
macd_list <- MACD(df_alv$adjusted, nFast = 12, nSlow = 26, nSig = 9)
bbands <- BBands(df_alv$adjusted, n = 20)

df_features <- df_alv %>%
  mutate(
    SMA_20 = sma_20,
    RSI_14 = rsi_14,
    MACD = macd_list[, "macd"],
    MACD_signal = macd_list[, "signal"],
    BB_upper = bbands[, "up"],
    BB_lower = bbands[, "dn"]
  )

# Save engineered features
write_csv(df_features, "alvde_features.csv") 