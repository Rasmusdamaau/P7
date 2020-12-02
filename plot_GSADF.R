plot_GSADF <- function(u, d = NULL, d_t = NULL, p_restrict = 0.95, start_date_tq_get = "2020-01-01",
                       image_name = NULL, valuta = "valuta", aktie = "aktie") {
  
  model_nr <- case_when(is.null(d_t) & is.null(d) ~ 1,
                        is.null(d_t) ~ 2,
                        !is.null(d_t) ~ 3)
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
    scale_x_date(date_labels = "%Y %b", date_breaks = "2 month") +
    xlab("") +
    ylab(paste("Price in", valuta)) +
    labs(title = aktie) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1))
  
  
  
  
  bubble <- ggplot(data = plot_data_combined, aes(x = x, y = value, color = name)) +
    geom_line(size = 1) +
    scale_x_date(date_labels = "%Y %b", date_breaks = "2 month") +
    theme(legend.position = "bottom",
          axis.text.x = ggplot2::element_text(angle = 60, hjust = 1)) +
    xlab("") +
    ylab("Count")
  
  if (model_nr == 1) {
    bubble <- bubble + theme(legend.position = "none")
  }
  
  if (!is.null(image_name)) {
    file_name <- paste(image_name, ".pdf", sep = "")
    pdf(file = file_name, height = 4,width = 8)
    grid.arrange(stock, bubble, nrow = 2)
    dev.off()
    cat("image found in ", file_name)
  } else {
  grid.arrange(stock, bubble, nrow = 2) }
}
