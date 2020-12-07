Tabel_GSADF <- function (gsadf) 
{
  max_data <- tibble::tibble(gsadf$result) %>% dplyr::filter(p_val == 
                                                               base::max(p_val))
  start_date <- gsadf$stock$date[1]
  count <- max_data %>% base::nrow()
  value <- max_data$p_val[1]
  interval_longest <- max_data %>% dplyr::slice_max(interval_length)
  interval_longest_values <- c(interval_longest$start_day + 
                                 lubridate::ymd(start_date), interval_longest$end_day + 
                                 lubridate::ymd(start_date))
  price_change_longest <- ((gsadf$stock$price[interval_longest$end_day] - 
                              gsadf$stock$price[base::max(interval_longest$start_day, 
                                                          1)])/gsadf$stock$price[base::max(interval_longest$start_day, 
                                                                                           1)]) * 100
  price_change_longest <- format(round(price_change_longest, 
                                       2), nsmall = 2)
  mean_startday <- base::mean(max_data$start_day) + lubridate::ymd(start_date)
  mean_endday <- base::mean(max_data$end_day) + lubridate::ymd(start_date)
  output <- base::list(count = count, value = value, interval_longest = interval_longest_values, 
                       price_change_longest = paste(price_change_longest, "%", 
                                                    sep = " "), mean_startday = mean_startday, mean_endday = mean_endday)
  base::return(output)
}
