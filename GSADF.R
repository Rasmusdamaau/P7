GSADF <- function(ticker, x = NULL, min_window = 30,
                  step_length = 5, window_increase = 10,
                  date_from = "1900-01-01", date_to = Sys.Date(),
                  drift = F, trend = F, risk_free_rate = 0.01) {
  if (is.null(x)) {
    stock_data <- tq_get(ticker, from = date_from, to = date_to) %>%
      select(date, adjusted) %>%
      na.omit()
    x <- discount(p = stock_data$adjusted, r = risk_free_rate)
    date_x <- stock_data$date
  }
  nrow_x <- length(x)
  window_size <- min_window
  
  t_val_model_nr <- case_when(
    drift == F & trend == F ~ 1,
    drift == T & trend == F ~ 2,
    drift == T & trend == T ~ 3
  )
  distri_tibbles <- c(
    "DF_distribution.RData",
    "DF_distribution_drift.RData",
    "DF_distribution_drift_trend.RData",
    "DF_distribution_trend.RData"
  )
  load(distri_tibbles[t_val_model_nr])
  distribution_tibble <- if (t_val_model_nr == 1) {
    df_distri
  } else if (t_val_model_nr == 2) {
    df_distri_drift
  } else if (t_val_model_nr == 3) {
    df_distri_drift_trend
  }
  
  
  result <- tibble()
  k_ind <- 1
  while (window_size < (nrow_x - step_length)) {
    max_i <- floor((nrow_x - window_size) / step_length)
    for (i in 0:max_i) {
      x_window <- x[(step_length * i):((step_length * i) + window_size)]
      plot_data <- tibble(t = seq_along(x_window), x_window)
      if (drift == F & trend == F) {
        model <- lm(diff(x_window) ~ 0 + na.omit(lag(x_window)), data = plot_data)
      }
      if (drift == T & trend == F) {
        model <- lm(diff(x_window) ~ na.omit(lag(x_window)), data = plot_data)
      }
      if (drift == T & trend == T) {
        model <- lm(diff(x_window) ~ t[-1] + na.omit(lag(x_window)), data = plot_data)
      }
      coef_model <- coefficients(summary(model))
      result[k_ind, "estimate"] <- coef_model[t_val_model_nr, 1]
      result[k_ind, "std_error"] <- coef_model[t_val_model_nr, 2]
      result[k_ind, "t_value"] <- coef_model[t_val_model_nr, 3]
      result[k_ind, "start_day"] <- step_length * i
      result[k_ind, "end_day"] <- ((step_length * i) + window_size)
      result[k_ind, "interval_length"] <- window_size
      k_ind <- k_ind + 1
    }
    window_size <- window_size + window_increase
  }
  result <- result %>%
    rowwise() %>%
    mutate(p_val = p_val_fun(
      t_val = t_value,
      interval_length = interval_length,
      distribution_tibble = distribution_tibble
    ))
  result_list <- list(
    stock = tibble(
      date = date_x,
      price = x
    ),
    result = result
  )
  return(result_list)
}