map_to_interval <- function(x, x_max, x_min, u=1, l=0) {

  m <- (u-l)/(x_max-x_min)
  b <- l - (x_min / (x_max - x_min))*(u-l)

  return(m*x+b)
}
