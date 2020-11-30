discount <- function(p, r = 0.01) {
  p / (1 + r / 365)^(seq_along(p))
}