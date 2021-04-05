#' ordinary least squares model
#'
#' @param formula
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(cars) + aes(x = speed, y = dist) + geom_point() + geom_lm()
geom_lm <- function(formula = y ~ x, ...)  {
  ggplot2::geom_smooth(formula = formula, method = "lm", se = FALSE,
                       ...)
}
