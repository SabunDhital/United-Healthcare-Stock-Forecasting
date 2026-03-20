# UNH Stock Price Forecasting

## Project Overview
This project analyzes and forecasts UnitedHealth Group (UNH) stock closing prices using time series methods in R. The analysis includes exploratory time series visualization, seasonality assessment, stationarity testing, correlation analysis, baseline forecasting, model comparison, and final forecasting with ARIMA.

## Objectives
- Explore historical UNH stock price behavior over time
- Identify trend, seasonality, and short-term movement patterns
- Test for stationarity and apply differencing when needed
- Compare forecasting approaches using training and validation data
- Build a final forecasting model for future stock prices

## Dataset
- Source: Historical UNH stock price data
- Frequency: Daily trading data
- Key variables:
  - Date
  - Open
  - High
  - Low
  - Close
  - Volume

## Tools and Technologies
- Programming Language: R
- Libraries:
  - tidyverse
  - lubridate
  - tseries
  - forecast
  - ggplot2
  - corrplot
  - zoo
  - patchwork

## Methodology

### 1. Data Preparation
- Imported stock price data from CSV
- Converted the date field into proper date format
- Checked data structure and missing values
- Created a daily time series object for closing prices

### 2. Exploratory Time Series Analysis
- Plotted daily closing prices
- Added a trend line to visualize long-term movement
- Created monthly seasonal plots
- Built yearly seasonal plots by month
- Calculated and plotted 50-day and 100-day moving averages

### 3. Time Series Decomposition
- Aggregated daily prices into monthly average closing prices
- Decomposed the monthly series into:
  - Trend
  - Seasonal
  - Residual
- Used decomposition plots to understand underlying patterns

### 4. Stationarity and Correlation Analysis
- Conducted Augmented Dickey-Fuller (ADF) tests
- Applied first differencing to address non-stationarity
- Plotted ACF and PACF for the differenced series
- Examined correlations among Open, High, Low, Close, and Volume

### 5. Forecasting Models
The dataset was split into training and validation sets to compare forecasting accuracy across models:
- Naive Forecast
- Random Walk with Drift
- Auto ARIMA
- ETS

Each model was evaluated using forecast accuracy metrics on the validation set.

### 6. Final Forecasting Model
- Selected a final ARIMA(2,1,2) model
- Forecasted the next 252 trading days
- Generated 80% and 95% prediction intervals
- Exported forecast results to CSV

## Key Outputs
- Time series trend plots
- Seasonal plots
- Moving average visualization
- Decomposition plots
- ACF and PACF plots
- Correlation matrix
- Forecast comparison plots
- Residual diagnostics
- Final 252-day forecast with confidence intervals

## Key Insights
- UNH stock prices show a strong long-term trend over time
- Differencing was needed to improve stationarity for modeling
- Baseline models provided a benchmark for forecast comparison
- ARIMA and ETS offered more structured forecasting approaches
- The final ARIMA(2,1,2) model was used to generate future price forecasts

## Business Value
This project demonstrates how time series forecasting can support financial analysis and decision-making by:
- Identifying historical stock behavior patterns
- Comparing multiple forecasting techniques
- Providing a data-driven view of potential future price movement
- Supporting investment research and forecasting workflows

## Files Included
- `UNH Stock Data Final.csv` — input dataset
- `UNH_ARIMA_212_Forecast.csv` — final forecast output
- `time_series_forecasting.R` — main analysis script

## Author
Sabun Dhital  
MS in Business Analytics  
University of South Dakota  
Email: sabundhital@gmail.com
