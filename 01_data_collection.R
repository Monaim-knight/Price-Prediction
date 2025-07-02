# 01_data_collection.R
# Collect historical stock data for DAX companies
# For reproducibility, use the provided CSV file. To fetch new data, uncomment the relevant section.

# Install required packages if not already installed
if (!require(BatchGetSymbols)) install.packages('BatchGetSymbols')
if (!require(tidyverse)) install.packages('tidyverse')

library(BatchGetSymbols)
library(tidyverse)

# Read provided data
stock_data <- read_csv('german_stock_data.csv', show_col_types = FALSE)

# Uncomment below to fetch new data from Yahoo Finance
# dax_tickers <- c('ADS.DE', 'ALV.DE', 'BAS.DE', 'BAYN.DE', 'BEI.DE', 'BMW.DE', 'CON.DE', '1COV.DE', 'DAI.DE', 'DB1.DE', 'DBK.DE', 'DPW.DE', 'DTE.DE', 'EOAN.DE', 'FME.DE', 'FRE.DE', 'HEI.DE', 'HEN3.DE', 'IFX.DE', 'LIN.DE', 'MRK.DE', 'MTX.DE', 'MUV2.DE', 'RWE.DE', 'SAP.DE', 'SIE.DE', 'VOW3.DE', 'VNA.DE', 'ZAL.DE', 'QIA.DE')
# first_date <- as.Date('2018-01-01')
# last_date <- Sys.Date()
# stock_data <- BatchGetSymbols(tickers = dax_tickers,
#                               first.date = first_date,
#                               last.date = last_date,
#                               thresh.bad.data = 0.5)$df.tickers
# write_csv(stock_data, 'german_stock_data.csv') 