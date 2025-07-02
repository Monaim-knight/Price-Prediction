# 03_eda.R
# Exploratory Data Analysis for German stock data

library(tidyverse)
library(ggplot2)
library(corrplot)
library(zoo)

# Load preprocessed data
df <- read_csv('german_stock_data_preprocessed.csv', show_col_types = FALSE)

# Line plot of normalized prices for a sample ticker
ggplot(df %>% filter(symbol == 'ALV.DE'), aes(x = date, y = price.norm)) +
  geom_line() +
  labs(title = 'Normalized Price of ALV.DE', x = 'Date', y = 'Normalized Price')

ggsave('eda_lineplot_alvde.png')

# Calculate daily returns
df <- df %>%
  group_by(symbol) %>%
  mutate(return = (adjusted / lag(adjusted)) - 1) %>%
  ungroup()

# Volatility (rolling standard deviation)
df <- df %>%
  group_by(symbol) %>%
  mutate(roll_vol = zoo::rollapply(return, width = 21, FUN = sd, fill = NA, align = 'right')) %>%
  ungroup()

# Correlation heatmap of returns (latest 1 year)
returns_wide <- df %>%
  filter(date > (max(date) - 365)) %>%
  select(date, symbol, return) %>%
  spread(symbol, return)

corr_matrix <- cor(returns_wide[,-1], use = 'pairwise.complete.obs')
corrplot(corr_matrix, method = 'color', tl.cex = 0.7)

ggsave('eda_corrplot.png') 