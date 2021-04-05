#### residuals #####



StatOlsresiduals <- ggplot2::ggproto("StatOlsresiduals",
                                     ggplot2::Stat,
                                     compute_group = function(data, scales) {

                                       model <- lm(data$y ~ data$x)

                                       data.frame(x = data$x,
                                                  xend = data$x,
                                                  y = model$fitted.values,
                                                  yend = data$y)
                                     },

                                     required_aes = c("x", "y")
)


#' Drawing residuals from ols linear model
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
#' geom_point() + geom_lm() + geom_lm_residuals(color = "darkred")
geom_lm_residuals <- function(mapping = NULL, data = NULL,
                              position = "identity", na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatOlsresiduals, geom = ggplot2::GeomSegment, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
