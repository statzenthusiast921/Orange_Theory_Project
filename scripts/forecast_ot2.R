#----------------------------------------
# Orange Theory Multi-Response Forecast
# Random Forest + Holiday-Aware Future Dates
#----------------------------------------
message('Loading packages...')
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(lubridate)
  library(tidyr)
  library(timetk)
  library(modeltime)
  library(tidymodels)
  library(timeDate)
  library(ranger)
  library(ggplot2)
  library(purrr)
  library(janitor)
})
set.seed(123)
#----------------------------------------
# Load & clean data
df <- read_excel("~/Desktop/Data Projects/OT Data Analysis/data/Orange_Theory.xlsx") |>
  clean_names() |>
  mutate(
    month_start = floor_date(date, "month"),
    class_num = 1,
    class_type = case_when(
      grepl("2G", class_type, ignore.case = TRUE) ~ "2G",
      grepl("3G", class_type, ignore.case = TRUE) ~ "3G",
      TRUE ~ "2G"
    )
  )
current_year <- year(Sys.Date())
#----------------------------------------
# Compute case weights (historical weighting, used for modeling)
df <- df |>
  mutate(
    dec_month_start = decimal_date(month_start),
    case_raw = exp(max(dec_month_start) - dec_month_start)
  ) |>
  group_by(month_start) |>
  mutate(case_raw = max(case_raw)) |>
  ungroup() |>
  mutate(
    case_wts = (case_raw - min(case_raw)) / (max(case_raw) - min(case_raw)),
    case_wts = 1 - case_wts
  )
response <- c("class_num", "calories", "splat")
#----------------------------------------
# Holiday Week Detection
get_thanksgiving <- function(year) {
  first_day <- as.Date(paste0(year, "-11-01"))
  offset <- (4 - wday(first_day) + 7) %% 7
  first_thursday <- first_day + offset
  first_thursday + 21
}
get_christmas <- function(year) {
  as.Date(paste0(year, "-12-25"))
}
is_holiday_week <- function(date_vec) {
  yrs <- unique(year(date_vec))
  holiday_days <- c()
  for (y in yrs) {
    tg_week <- seq(get_thanksgiving(y) - 2, get_thanksgiving(y) + 2, by = "day")
    xm_week <- seq(get_christmas(y) - 2, get_christmas(y) + 2, by = "day")
    holiday_days <- c(holiday_days, tg_week, xm_week)
  }
  date_vec %in% holiday_days
}
#----------------------------------------
# Historical monthly class counts
monthly_counts <- df |>
  filter(!is.na(calories) & year(date) >= 2021) |>
  mutate(year = year(date), month = month(date)) |>
  group_by(year, month) |>
  summarise(n_classes = n(), .groups = "drop") |>
  group_by(month) |>
  summarise(median_classes = median(n_classes), .groups = "drop") |>
  mutate(median_classes = floor(median_classes))
#----------------------------------------
# Generate realistic future class dates (REVISED LOGIC HERE)
holiday_factor <- 0.5  # reduce classes in holiday weeks by 50%

# Determine the last date in the historical data
max_hist_date <- max(df$date)
start_date_future <- max_hist_date + 1

# Define the sequence of *month start dates* for which we need to generate classes, 
# starting from the beginning of the month following the last historical date.
months_future <- seq.Date(
  from = floor_date(start_date_future, "month"), 
  to   = as.Date(paste0(current_year+1, "-12-31")),
  by   = "month"
)

df_ext <- purrr::map_df(months_future, function(m_start) {
  
  m_month <- month(m_start)
  m_year <- year(m_start)
  
  # --- 1. Define the pool of days to sample from ---
  m_days <- seq.Date(
    from = m_start,
    to   = ceiling_date(m_start, "month") - 1,
    by   = "day"
  )
  
  # If we are in the *current* month where historical data exists, 
  # only consider days from *after* the last historical class date.
  if (m_start == floor_date(max_hist_date, "month")) {
    m_days <- m_days[m_days >= start_date_future]
  }
  
  # If m_days is empty (e.g., if max_hist_date is the last day of a month), skip.
  if (length(m_days) == 0) return(tibble())
  
  # --- 2. Determine how many classes to generate ---
  
  # Historical median classes
  n_classes_total <- monthly_counts |> filter(month == m_month) |> pull(median_classes)
  
  # If it's a *full future month* (i.e., not the current partial month), 
  # the remaining classes is the total median.
  if (m_start > floor_date(max_hist_date, "month")) {
    
    n_classes_remaining <- n_classes_total
    
  } else {
    # If it's the *current partial month*, we need to subtract classes already taken
    # from the total median to get the number of remaining classes to forecast.
    
    classes_taken <- df |>
      filter(
        month(date) == m_month,
        year(date) == m_year
      ) |>
      nrow()
    
    n_classes_remaining <- max(n_classes_total - classes_taken, 0)
  }
  
  # Ensure we don't try to generate more classes than available days in the remaining period
  n_classes_remaining <- min(n_classes_remaining, length(m_days))
  
  # --------------------------
  # Holiday scaling (proportion of total classes, not number of days)
  holiday_mask <- is_holiday_week(m_days)
  n_holiday_classes <- round(n_classes_remaining * holiday_factor)
  # Limit holiday classes to the number of available holiday days
  n_holiday_classes <- min(n_holiday_classes, sum(holiday_mask))
  n_normal_classes <- n_classes_remaining - n_holiday_classes
  
  # Sample class days
  class_days <- c(
    sample(m_days[holiday_mask], size = n_holiday_classes, replace = FALSE),
    sample(m_days[!holiday_mask], size = n_normal_classes, replace = FALSE)
  )
  
  tibble(date = class_days)
}) |>
  mutate(
    class_type = sample(c("2G","3G"), n(), replace = TRUE),
    case_wts   = 1
  )
#----------------------------------------
# Run forecasting loop for each response
all_results <- list()
for (resp in response) {
  message(paste0("Running forecast for ", resp, "..."))
  
  splits <- time_series_split(df, assess = 365, cumulative = TRUE)
  
  rec <- recipe(as.formula(paste(resp, "~ date + class_type + case_wts")),
                data = training(splits)) |>
    step_holiday(date, holidays = timeDate::listHolidays()) |>
    step_date(date, features = c("dow", "month")) |>
    step_mutate(
      dow = factor(wday(date)),
      month = factor(month(date))
    ) |>
    step_rm(date) |>
    update_role(case_wts, new_role = "case_weight") |>
    step_dummy(all_nominal_predictors()) |>
    step_zv(all_predictors())
  
  model_spec_rf <- rand_forest(trees = 1000, min_n = 5) |>
    set_engine("ranger") |>
    set_mode("regression")
  
  wflw_rf <- workflow() |>
    add_model(model_spec_rf) |>
    add_recipe(rec)
  
  # Note: drop_na is necessary for multi-response forecasting where some responses might 
  # be NA for certain historical dates (e.g., if 'splat' wasn't recorded).
  fit_rf <- fit(wflw_rf, training(splits) |> drop_na(!!sym(resp)))
  
  models_tbl <- modeltime_table(fit_rf)
  calibrated_tbl <- modeltime_calibrate(models_tbl, testing(splits))
  
  forecast_tbl <- modeltime_forecast(
    calibrated_tbl,
    new_data = df_ext,
    actual_data = df
  ) |>
    mutate(response_var = resp)
  
  all_results[[resp]] <- forecast_tbl
}
#----------------------------------------
# Combine all results
final_forecast_tbl <- bind_rows(all_results) |>
  dplyr::select(key = .key, date = .index, value = .value, response_var) |>
  arrange(response_var, date)
#----------------------------------------
final_forecast_tbl |>
  filter(
    lubridate::year(date)==current_year) |>
  group_by(response_var, key) |>
  summarise(value = sum(value)) |>
  pivot_wider(
    names_from = key,
    values_from = value
  ) |>
  mutate(total = actual + prediction)
write.csv(final_forecast_tbl |> filter(response_var == "calories"), "~/Desktop/Data Projects/OT Data Analysis/data/ot_predictions.csv", row.names = FALSE)
