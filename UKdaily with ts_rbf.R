library(UKgrid)
library(tidyverse)
library(plotly)

# Functions ====
ts_rbf <- function(date, alpha, month) {
  if(!is.numeric(month)) stop("'month' argument must be numeric")
  
  date <- as_date(date)
  year <- year(date)
  
  start_of_month <- ymd(paste0(year, "-" , month, "-01"))
  yday_of_month_start <- yday(start_of_month)
  mid_month <- yday_of_month_start + days_in_month(start_of_month) / 2
  
  day_of_year <- yday(date)
  
  return(exp(-1 / (2 * alpha) * (day_of_year - mid_month)^2))
}

# Data ====
UKdaily <- extract_grid(type = "data.frame",
                        columns = "ND",
                        aggregate = "daily")

# Feature engineering ====
UKdaily <- UKdaily %>%
  mutate(wday = wday(TIMESTAMP, label = TRUE),
         month = month(TIMESTAMP, label = TRUE),
         lag365 = dplyr::lag(ND, 365)) %>%
  filter(!is.na(lag365)) %>%
  arrange(TIMESTAMP)
str(UKdaily)

# Create RBF feature for each month
alpha = 500
for(i in 1:12) {
  col_name <- paste0("rbf_", month(i, label = TRUE))
  UKdaily[col_name] <- ts_rbf(df_UKdaily$TIMESTAMP, alpha = alpha, i)
}

# Plot RBF ====
df_plot <- UKdaily %>%
  pivot_longer(cols = contains("rbf_"),
               names_to = "rbf",
               values_to = "rbf_value")

p <- ggplot(df_plot, aes(x = TIMESTAMP, y = rbf_value, color = rbf)) +
  geom_line()

ggplotly(p)

# Test model ====
df_ts <- UKdaily

df_ts_train <- df_ts[1:3457,]
df_ts_test <- df_ts[3457:4939,]

ts_train <- df_ts_train$ND

lm.fit <- lm(ND~., data = df_ts[1:3457,])
summary(lm.fit)

predict(lm.fit, df_ts[3457:4939,]) %>%
  plot()

# -------- Code Chank 28 --------
start_date <- min(UKdaily$TIMESTAMP)

UK_ts <- ts(df_ts$ND, 
            start = c(year(start_date), yday(start_date)),
            frequency = 365)

# -------- Code Chank 29 --------
h <-  365
UKpartitions <- ts_split(UK_ts, sample.out = h)
train_ts <- UKpartitions$train
test_ts <- UKpartitions$test

train_df <- df_ts[1:(nrow(df_ts) - h), ]
test_df <- df_ts[(nrow(df_ts) - h + 1):nrow(df_ts), ]
# -------- Code Chank 30 --------
md_tslm1 <- tslm(train_ts ~ season + trend)
fc_tslm1 <- forecast(md_tslm1, h = h)
test_forecast(actual = UK_ts,
              forecast.obj = fc_tslm1,
              test = test_ts)
accuracy(fc_tslm1, test_ts)
# -------- Code Chank 31 --------
md_tslm2 <- tslm(train_ts ~wday+lag365+trend+rbf_Jan+rbf_Feb+rbf_Mar+rbf_Apr+rbf_May+rbf_Jun+rbf_Jul+rbf_Aug+rbf_Sep+rbf_Oct+rbf_Nov+rbf_Dec, data = train_df)
fc_tslm2 <- forecast(md_tslm2, h = h, newdata = test_df)
test_forecast(actual = UK_ts,
              forecast.obj = fc_tslm2,
              test = test_ts)
accuracy(fc_tslm2, test_ts)
