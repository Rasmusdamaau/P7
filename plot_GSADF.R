plot_GSADF <- function(u, d = NULL, d_t = NULL, p_restrict = 0.95, start_date_tq_get = "2020-01-01") {
  plot_data_combined <- combined_count(u = u, d_t = d_t, d = d, p_restrict = p_restrict)
  
  plot_data_rect <- u$result %>%
    filter(p_val == max(u$result$p_val)) %>%
    mutate(
      date_start = ymd(start_date_tq_get) + start_day,
      date_end = ymd(start_date_tq_get) + end_day
    )
  
  
  
  stock <- ggplot(data = u$stock, aes(x = date, y = price)) +
    geom_line() +
    geom_rect(
      data = plot_data_rect, aes(xmin = date_start, xmax = date_end),
      ymin = -Inf, ymax = Inf, alpha = 0.1, inherit.aes = F
    ) +
    scale_x_date(date_labels = "%Y %b", date_breaks = "2 month")
  
  
  
  bubble <- ggplot(data = plot_data_combined, aes(x = x, y = value, color = name)) +
    geom_line(size = 1) +
    scale_x_date(date_labels = "%Y %b", date_breaks = "2 month") +
    theme(legend.position = "bottom")
  
  grid.arrange(stock, bubble, nrow = 2)
}