# run_all_steps.R
# Master script to run the full German stock price prediction workflow

cat('Step 1: Data Collection...\n')
source('01_data_collection.R')
cat('Step 1 complete.\n\n')

cat('Step 2: Data Preprocessing...\n')
source('02_preprocessing.R')
cat('Step 2 complete.\n\n')

cat('Step 3: Exploratory Data Analysis (EDA)...\n')
source('03_eda.R')
cat('Step 3 complete.\n\n')

cat('Step 4: Feature Engineering...\n')
source('04_feature_engineering.R')
cat('Step 4 complete.\n\n')

cat('Step 5: Model Development...\n')
source('05_model_development.R')
cat('Step 5 complete.\n\n')

cat('Step 6: Prediction and Visualization...\n')
source('06_prediction_visualization.R')
cat('Step 6 complete.\n\n')

cat('All steps complete!\n')

# To launch the Shiny dashboard, uncomment the line below:
source('07_shiny_dashboard.R') 



library(shiny)
library(alphavantager)
library(TTR)
library(dplyr)

