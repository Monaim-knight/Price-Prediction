# 02_preprocessing.R
# Clean and preprocess stock data

library(tidyverse)
library(lubridate)

# Load data
df <- read_csv('german_stock_data.csv', show_col_types = FALSE)

# Handle missing values: Remove rows with NA in price columns
df_clean <- df %>%
  filter(!is.na(adjusted))

# Fill missing dates for each ticker
df_filled <- df_clean %>%
  group_by(symbol) %>%
  complete(date = seq.Date(min(date), max(date), by = 'day')) %>%
  fill(adjusted, .direction = 'downup') %>%
  ungroup()

# Normalize prices (Min-Max scaling)
df_filled <- df_filled %>%
  group_by(symbol) %>%
  mutate(price.norm = (adjusted - min(adjusted, na.rm=TRUE)) /
                       (max(adjusted, na.rm=TRUE) - min(adjusted, na.rm=TRUE))) %>%
  ungroup()

# Save preprocessed data
write_csv(df_filled, 'german_stock_data_preprocessed.csv') 