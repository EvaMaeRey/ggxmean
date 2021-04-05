
StatOlsinterceptcoords <- ggplot2::ggproto("StatOlsinterceptcoords",
                                           ggplot2::Stat,
                                           compute_group = function(data, scales) {

                                             options(digits = 2)

                                             model <- lm(data$y ~ data$x)

                                             data.frame(y = model$coefficients[1],
                                                        x = 0,
                                                        label = paste0("(0, ",
                                                                       model$coefficient[1] %>%
                                                                         good_digits(), ")"))

                                           },

                                           required_aes = c("x", "y")
)



#' Label ols linear model intercept
#'
#' @param mapping
#' @param data
#' @param position
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(cars) + aes(x = speed, y = dist) +
#' geom_point() + geom_lm() + geom_lm_intercept(color = "blue") +
#' geom_lm_intercept_label(hjust = 0)
geom_lm_intercept_label <- function(mapping = NULL, data = NULL,
                                    position = "identity", na.rm = FALSE, show.legend = NA,
                                    inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatOlsinterceptcoords, geom = ggplot2::GeomLabel, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
