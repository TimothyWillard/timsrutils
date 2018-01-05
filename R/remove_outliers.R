#' Remove outliers from a vector
#'
#' Removes the outliers from a numeric vector & ignores NA values.
#'
#' @param x A numeric vector
#' @param multiplier step multiplier, defaults to 1.5
#'
#' @return A numeric vector without outliers & NA values
#'
#' @importFrom stats na.omit
#' @importFrom stats quantile
#' @export
#'
#' @examples
#' x <- c(4,5,5,8,8,9,5,2,5,6,7,8,4,5,99)
#' remove.outliers(x)
remove.outliers <- function(x, multiplier = 1.5) {
  x <- na.omit(x)
  q1 <- quantile(x, probs = 0.25)
  q3 <- quantile(x, probs = 0.75)
  step <- multiplier * (q3 - q1)
  high <- q3 + step
  low <- q1 - step
  x[x < high & x > low]
}
