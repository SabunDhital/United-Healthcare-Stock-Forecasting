# ============================================================================
# TIME SERIES FORECASTING OF UNH STOCK PRICES
# Sabun Dhital - University of South Dakota
# ============================================================================

# STEP 1: LOAD REQUIRED PACKAGES
library(tidyverse)
library(lubridate)
library(tseries)
library(forecast)
library(dplyr)
library(ggplot2)
library(urca)
library(corrplot)
library(zoo)
library(patchwork)

# STEP 2: LOAD THE DATA
data <- read.csv("C:/Users/lenovo/Desktop/Time Series Forecasting/dataset/UHC Stock Data Final.csv")

# STEP 3: INSPECT THE DATE COLUMN AND CONVERT IT TO DATE FORMAT
head(data$Date, 10)
data$Date <- as.Date(data$Date, format = "%m/%d/%Y")

# STEP 4: REVIEW DATA STRUCTURE AND CHECK FOR MISSING VALUES
str(data$Date)
str(data)
head(data)
colSums(is.na(data))

# STEP 5: CREATE A DAILY TIME SERIES OBJECT FOR CLOSING PRICE
ts_close <- ts(data$Close, frequency = 252)

# STEP 6: PLOT THE DAILY CLOSING PRICE OVER TIME
basic_plot <- ggplot(data, aes(x = Date, y = Close)) +
  geom_line(color = "blue") +
  labs(
    title = "UNH Stock Closing Prices Over Time",
    x = "Date",
    y = "Closing Price ($)"
  ) +
  theme_minimal()

print(basic_plot)

# STEP 7: ADD A TREND LINE TO VISUALIZE LONG-TERM MOVEMENT
trend_plot <- basic_plot +
  geom_smooth(method = "loess", span = 0.2, se = FALSE, color = "red") +
  labs(title = "UNH Stock Closing Prices with Trend")

print(trend_plot)

# STEP 8: CREATE YEAR AND MONTH VARIABLES FOR SEASONAL ANALYSIS
data <- data %>%
  mutate(
    Year = year(Date),
    Month = month(Date)
  )

# STEP 9: AGGREGATE DAILY DATA INTO MONTHLY AVERAGE CLOSING PRICE
monthly_data <- data %>%
  group_by(Year, Month) %>%
  summarise(Avg_Close = mean(Close, na.rm = TRUE), .groups = "drop")

# STEP 10: CONVERT MONTH INTO LABELED FACTOR FOR BETTER PLOTTING
monthly_data$Month <- factor(monthly_data$Month, levels = 1:12, labels = month.abb)

print(monthly_data)

# STEP 11: CREATE A MONTHLY SEASONAL PLOT BY YEAR
monthly_plot <- ggplot(monthly_data, aes(x = Month, y = Avg_Close, group = Year, color = factor(Year))) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Monthly Seasonal Plot of UNH Stock",
    x = "Month",
    y = "Average Close Price ($)",
    color = "Year"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.background = element_rect(fill = "white"))

print(monthly_plot)

# STEP 12: ADD DAY VARIABLE FOR WITHIN-MONTH SEASONAL COMPARISON
data <- data %>%
  mutate(
    Year = year(Date),
    Month = month(Date),
    Day = day(Date)
  )

# STEP 13: CREATE A YEARLY SEASONAL PLOT FACETED BY MONTH
yearly_plot <- ggplot(data, aes(x = Day, y = Close, group = Year, color = as.factor(Year))) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~Month, scales = "free_y") +
  labs(
    title = "Yearly Seasonal Plot of UNH Stock by Month",
    x = "Day of Month",
    y = "Closing Price ($)",
    color = "Year"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.background = element_rect(fill = "white")
  )

print(yearly_plot)

# STEP 14: CALCULATE 50-DAY AND 100-DAY MOVING AVERAGES
data <- data %>%
  arrange(Date) %>%
  mutate(
    SMA_50 = zoo::rollmean(Close, k = 50, fill = NA, align = "right"),
    SMA_100 = zoo::rollmean(Close, k = 100, fill = NA, align = "right")
  )

# STEP 15: PLOT CLOSING PRICE WITH MOVING AVERAGES
ggplot(data, aes(x = Date)) +
  geom_line(aes(y = Close), color = "black", linewidth = 0.7, alpha = 0.7) +
  geom_line(aes(y = SMA_50, color = "50-Day MA"), linewidth = 1) +
  geom_line(aes(y = SMA_100, color = "100-Day MA"), linewidth = 1) +
  scale_color_manual(values = c("50-Day MA" = "blue", "100-Day MA" = "red")) +
  labs(
    title = "UNH Stock Closing Prices with Moving Averages",
    x = "Date",
    y = "Closing Price ($)",
    color = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 11, face = "bold"),
    plot.title = element_text(face = "bold", size = 13, hjust = 0)
  )

# STEP 16: CONVERT MONTHLY DATA INTO A MONTHLY TIME SERIES OBJECT
monthly_data$Month <- as.numeric(monthly_data$Month)

ts_monthly <- ts(
  monthly_data$Avg_Close,
  start = c(min(monthly_data$Year), min(monthly_data$Month)),
  frequency = 12
)

# STEP 17: DECOMPOSE THE MONTHLY SERIES INTO TREND, SEASONAL, AND RESIDUAL PARTS
decomp <- decompose(ts_monthly)

decomp_df <- data.frame(
  Time = time(ts_monthly),
  Data = decomp$x,
  Seasonal = decomp$seasonal,
  Residual = decomp$random,
  Trend = decomp$trend
)

decomp_long <- decomp_df %>%
  pivot_longer(
    cols = c(Data, Trend, Seasonal, Residual),
    names_to = "Component",
    values_to = "Value"
  ) %>%
  mutate(Component = factor(Component, levels = c("Data", "Trend", "Seasonal", "Residual")))

# STEP 18: PLOT THE DECOMPOSED COMPONENTS
ggplot(decomp_long, aes(x = Time, y = Value)) +
  geom_line() +
  facet_wrap(~Component, ncol = 1, scales = "free_y") +
  labs(
    title = "Decomposition of Monthly UNH Stock Prices",
    x = "Time",
    y = NULL
  ) +
  theme_minimal()

# STEP 19: RUN ADF TEST ON THE ORIGINAL SERIES TO CHECK STATIONARITY
adf_result <- adf.test(ts_close)
print(adf_result)

# STEP 20: APPLY FIRST DIFFERENCING TO REMOVE NON-STATIONARITY
ts_close_diff1 <- diff(ts_close)

# STEP 21: PLOT THE FIRST-DIFFERENCED SERIES
plot(ts_close_diff1, main = "1st Order Differenced Close Price", ylab = "Differenced Price")

# STEP 22: RUN ADF TEST AGAIN ON THE DIFFERENCED SERIES
adf_result_diff1 <- adf.test(ts_close_diff1)
print(adf_result_diff1)

# STEP 23: RESET PLOT SETTINGS BEFORE ACF AND PACF
par(mfrow = c(1, 1))
par(mar = c(5, 4, 4, 2) + 0.1)

# STEP 24: PLOT ACF AND PACF OF THE DIFFERENCED SERIES
acf(ts_close_diff1, main = "ACF Plot: UNH Stock Closing Prices")
pacf(ts_close_diff1, main = "PACF Plot: UNH Stock Closing Prices")

# STEP 25: SELECT NUMERIC VARIABLES FOR CORRELATION ANALYSIS
cor_data <- data[, c("Open", "High", "Low", "Close", "Volume")]

# STEP 26: COMPUTE AND PLOT THE CORRELATION MATRIX
cor_matrix <- cor(cor_data, use = "complete.obs")

corrplot(
  cor_matrix,
  method = "shade",
  type = "lower",
  tl.col = "black",
  tl.cex = 1.2,
  number.cex = 0.9,
  addCoef.col = "black",
  diag = FALSE,
  col = colorRampPalette(c("red", "white", "blue"))(200),
  title = "Correlation Matrix of Stock Variables",
  mar = c(0, 0, 2, 0)
)

# STEP 27: SPLIT THE SERIES INTO TRAINING AND VALIDATION SETS
n_total <- length(ts_close)
n_train <- 626
n_valid <- n_total - n_train

train_ts <- ts_close[1:n_train]
valid_ts <- ts_close[(n_train + 1):n_total]
full_dates <- data$Date

# STEP 28: FIT A NAIVE FORECAST MODEL
naive_model <- naive(train_ts, h = n_valid)

# STEP 29: CREATE DATA FRAMES FOR TRAINING, VALIDATION, AND NAIVE FORECAST
train_df <- data.frame(
  Date = full_dates[1:n_train],
  Value = as.numeric(train_ts),
  Type = "Training"
)

valid_df <- data.frame(
  Date = full_dates[(n_train + 1):n_total],
  Value = as.numeric(valid_ts),
  Type = "Validation"
)

naive_df <- data.frame(
  Date = full_dates[(n_train + 1):n_total],
  Value = as.numeric(naive_model$mean),
  Type = "Naive Forecast"
)

naive_plot_df <- rbind(train_df, valid_df, naive_df)

# STEP 30: PLOT THE NAIVE FORECAST AGAINST THE VALIDATION DATA
ggplot(naive_plot_df, aes(x = Date, y = Value, color = Type)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(values = c(
    "Training" = "black",
    "Validation" = "red",
    "Naive Forecast" = "blue"
  )) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Naive Forecast vs Validation",
    x = "Year",
    y = "Close Price ($)",
    color = "Legend"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = c(0.01, 0.99),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "gray80"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, "lines")
  )

# STEP 31: EVALUATE NAIVE FORECAST ACCURACY
naive_accuracy <- accuracy(naive_model, valid_ts)
print(naive_accuracy)

# STEP 32: FIT A RANDOM WALK WITH DRIFT MODEL
rwf_drift_model <- rwf(train_ts, h = n_valid, drift = TRUE)

# STEP 33: CREATE FORECAST DATA FOR THE DRIFT MODEL
rwf_drift_df <- data.frame(
  Date = full_dates[(n_train + 1):n_total],
  Value = as.numeric(rwf_drift_model$mean),
  Type = "RWF with Drift"
)

rwf_drift_plot_df <- rbind(train_df, valid_df, rwf_drift_df)

# STEP 34: PLOT RANDOM WALK WITH DRIFT AGAINST VALIDATION DATA
ggplot(rwf_drift_plot_df, aes(x = Date, y = Value, color = Type)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(values = c(
    "Training" = "black",
    "Validation" = "red",
    "RWF with Drift" = "darkorange"
  )) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Random Walk Forecast with Drift vs Validation",
    x = "Year",
    y = "Close Price ($)",
    color = "Legend"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = c(0.04, 0.99),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "gray80"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, "lines")
  )

# STEP 35: EVALUATE RANDOM WALK WITH DRIFT ACCURACY
rwf_drift_accuracy <- accuracy(rwf_drift_model, valid_ts)
print(rwf_drift_accuracy)

# STEP 36: CALCULATE RESIDUALS FOR NAIVE AND DRIFT MODELS
naive_res <- as.numeric(valid_ts) - as.numeric(naive_model$mean)
rwf_res <- as.numeric(valid_ts) - as.numeric(rwf_drift_model$mean)

res_dates <- full_dates[(n_train + 1):n_total]

naive_res_df <- data.frame(Date = res_dates, Residual = naive_res)
rwf_res_df <- data.frame(Date = res_dates, Residual = rwf_res)

# STEP 37: PLOT RESIDUALS FOR NAIVE AND DRIFT MODELS
naive_plot <- ggplot(naive_res_df, aes(x = Date, y = Residual)) +
  geom_col(fill = "darkgreen", width = 1) +
  labs(title = "Residuals: Naive Forecast", x = "Date", y = "Residual") +
  theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )

rwf_plot <- ggplot(rwf_res_df, aes(x = Date, y = Residual)) +
  geom_col(fill = "darkblue", width = 1) +
  labs(title = "Residuals: Drift Forecast", x = "Date", y = "Residual") +
  theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )

naive_plot + rwf_plot + plot_layout(ncol = 2)

# STEP 38: FIT AN AUTO-ARIMA MODEL ON THE TRAINING DATA
arima_model <- auto.arima(train_ts)

# STEP 39: FORECAST USING THE ARIMA MODEL
arima_forecast <- forecast(arima_model, h = n_valid)

arima_df <- data.frame(
  Date = full_dates[(n_train + 1):n_total],
  Value = as.numeric(arima_forecast$mean),
  Type = "ARIMA Forecast"
)

arima_plot_df <- rbind(train_df, valid_df, arima_df)

# STEP 40: PLOT ARIMA FORECAST AGAINST VALIDATION DATA
ggplot(arima_plot_df, aes(x = Date, y = Value, color = Type)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(values = c(
    "Training" = "black",
    "Validation" = "red",
    "ARIMA Forecast" = "darkgreen"
  )) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "ARIMA Forecast vs Validation",
    x = "Year",
    y = "Close Price ($)",
    color = "Legend"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = c(0.04, 0.99),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "gray80"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, "lines")
  )

# STEP 41: EVALUATE ARIMA FORECAST ACCURACY
arima_accuracy <- accuracy(arima_forecast, valid_ts)
print(arima_accuracy)

# STEP 42: EXAMINE ARIMA MODEL RESIDUALS AND HIGHLIGHT OUTLIERS
residuals_arima <- residuals(arima_model)

arima_residuals_df <- data.frame(
  Date = full_dates[1:n_train],
  Residual = as.numeric(residuals_arima)
)

threshold <- 3 * sd(arima_residuals_df$Residual, na.rm = TRUE)
outliers <- abs(arima_residuals_df$Residual) > threshold

ggplot(arima_residuals_df, aes(x = Date, y = Residual)) +
  geom_line(color = "steelblue", linewidth = 0.5) +
  geom_point(
    data = arima_residuals_df[outliers, ],
    aes(x = Date, y = Residual),
    color = "red",
    size = 2
  ) +
  geom_hline(yintercept = c(-threshold, threshold), linetype = "dashed", color = "gray") +
  scale_x_date(
    breaks = as.Date(c("2022-01-01", "2023-01-01", "2024-01-01")),
    date_labels = "%Y",
    limits = as.Date(c("2022-01-01", "2024-12-31")),
    expand = c(0, 0)
  ) +
  labs(title = "ARIMA Residuals with Outliers", x = "Year", y = "Residuals") +
  theme_minimal()

# STEP 43: FIT AN ETS MODEL ON THE TRAINING DATA
ets_model <- ets(train_ts)

# STEP 44: FORECAST USING THE ETS MODEL
ets_forecast <- forecast(ets_model, h = n_valid)

ets_df <- data.frame(
  Date = full_dates[(n_train + 1):n_total],
  Value = as.numeric(ets_forecast$mean),
  Type = "ETS Forecast"
)

ets_plot_df <- rbind(train_df, valid_df, ets_df)

# STEP 45: PLOT ETS FORECAST AGAINST VALIDATION DATA
ggplot(ets_plot_df, aes(x = Date, y = Value, color = Type)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(values = c(
    "Training" = "black",
    "Validation" = "red",
    "ETS Forecast" = "purple"
  )) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "ETS Forecast vs Validation",
    x = "Year",
    y = "Close Price ($)",
    color = "Legend"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = c(0.04, 0.99),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "gray80"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, "lines")
  )

# STEP 46: EVALUATE ETS FORECAST ACCURACY
ets_accuracy <- accuracy(ets_forecast, valid_ts)
print(ets_accuracy)

# STEP 47: EXAMINE ETS MODEL RESIDUALS AND HIGHLIGHT OUTLIERS
residuals_ets <- residuals(ets_model)

ets_residuals_df <- data.frame(
  Date = full_dates[1:n_train],
  Residual = as.numeric(residuals_ets)
)

threshold_ets <- 3 * sd(ets_residuals_df$Residual, na.rm = TRUE)
ets_outliers_df <- ets_residuals_df[abs(ets_residuals_df$Residual) > threshold_ets, ]

ggplot(ets_residuals_df, aes(x = Date, y = Residual)) +
  geom_line(color = "steelblue", linewidth = 0.5) +
  geom_point(data = ets_outliers_df, aes(x = Date, y = Residual), color = "red", size = 2) +
  geom_hline(yintercept = c(threshold_ets, -threshold_ets), linetype = "dashed", color = "gray") +
  geom_vline(xintercept = as.numeric(full_dates[n_train]), linetype = "dotted", color = "gray40") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "ETS Model Residuals with Outliers",
    x = "Date",
    y = "Residuals"
  ) +
  theme_minimal(base_size = 13)

# STEP 48: FIT THE FINAL ARIMA(2,1,2) MODEL ON THE FULL SERIES
final_arima_model <- Arima(ts_close, order = c(2, 1, 2))

# STEP 49: GENERATE A 252-DAY FUTURE FORECAST
future_forecast <- forecast(final_arima_model, h = 252)

# STEP 50: CREATE FUTURE BUSINESS-DAY DATES FOR THE FORECAST
start_date <- max(data$Date) + 1
forecast_dates <- seq.Date(from = start_date, by = "day", length.out = 400)
forecast_dates <- forecast_dates[weekdays(forecast_dates) %in%
                                   c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")]
forecast_dates <- forecast_dates[1:252]

# STEP 51: BUILD A FORECAST DATA FRAME WITH CONFIDENCE INTERVALS
forecast_df <- data.frame(
  Date = forecast_dates,
  Forecast = as.numeric(future_forecast$mean),
  Lo80 = future_forecast$lower[, 1],
  Hi80 = future_forecast$upper[, 1],
  Lo95 = future_forecast$lower[, 2],
  Hi95 = future_forecast$upper[, 2]
)

head(forecast_df)

# STEP 52: PLOT THE FINAL FORECAST WITH 80% AND 95% INTERVALS
ggplot(forecast_df, aes(x = Date, y = Forecast)) +
  geom_line(color = "darkblue", linewidth = 1) +
  geom_ribbon(aes(ymin = Lo95, ymax = Hi95), fill = "gray80", alpha = 0.4) +
  geom_ribbon(aes(ymin = Lo80, ymax = Hi80), fill = "gray60", alpha = 0.4) +
  labs(
    title = "UNH Stock Forecast Using ARIMA(2,1,2)",
    x = "Date",
    y = "Forecasted Price ($)"
  ) +
  theme_minimal(base_size = 13)

# STEP 53: SAVE THE FINAL FORECAST RESULTS TO CSV
write.csv(forecast_df, "UNH_ARIMA_212_Forecast.csv", row.names = FALSE)

# STEP 54: PLOT THE ACF OF FINAL MODEL RESIDUALS
acf(residuals(final_arima_model), main = "ACF of Residuals - ARIMA(2,1,2)")

# STEP 55: PLOT FINAL MODEL RESIDUALS AND HIGHLIGHT OUTLIERS
residuals_arima_final <- residuals(final_arima_model)

residuals_df <- data.frame(
  Date = data$Date[1:length(residuals_arima_final)],
  Residual = as.numeric(residuals_arima_final)
)

threshold_final <- 3 * sd(residuals_df$Residual, na.rm = TRUE)
outliers_final <- abs(residuals_df$Residual) > threshold_final

ggplot(residuals_df, aes(x = Date, y = Residual)) +
  geom_line(color = "steelblue", linewidth = 0.5) +
  geom_point(
    data = residuals_df[outliers_final, ],
    aes(x = Date, y = Residual),
    color = "red",
    size = 2
  ) +
  geom_hline(yintercept = c(-threshold_final, threshold_final), linetype = "dashed", color = "gray") +
  labs(
    title = "ARIMA(2,1,2) Residuals with Outliers",
    x = "Date",
    y = "Residuals"
  ) +
  theme_minimal(base_size = 13)