# utils.R
#
# misc utility functions

#' Linear scaling of an interval
#'
#' Map points in the interval \[x_max, x_min\] to the interval \[l, u\].
#'
#' @param x a numeric, point to be scaled
#' @param x_max upper bound of the domain
#' @param x_min lower bound of the domain
#' @param u upper bound of the range
#' @param l lower bound of the range
#'
#' @return numeric scaled between upper and lower interval bounds
map_to_interval <- function(x, x_max, x_min, u=1, l=0) {

  m <- (u-l)/(x_max-x_min)
  b <- l - (x_min / (x_max - x_min))*(u-l)

  return(m*x+b)
}
