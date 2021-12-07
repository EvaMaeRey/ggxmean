#### fitted #####

StatOlsfitted <- ggplot2::ggproto("StatOlsfitted",
                                  ggplot2::Stat,
                                  compute_group = function(data, scales) {

                                    model <- lm(data$y ~ data$x)
                                    data$x %>%
                                      data.frame(x = data$x,
                                                 y = model$fitted.values)
                                  },

                                  required_aes = c("x", "y")
)


#' Fitted values along ols line, points
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
#' geom_point() + geom_lm() + geom_lm_fitted(color = "blue")
geom_lm_fitted <- function(mapping = NULL, data = NULL,
                           position = "identity", na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatOlsfitted, geom = ggplot2::GeomLine, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
