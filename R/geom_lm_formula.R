StatOlsformula <- ggplot2::ggproto("StatOlsformula",
                                   ggplot2::Stat,
                                   compute_group = function(data, scales) {

                                     options(digits = 2)

                                     model <- lm(data$y ~ data$x)

                                     data.frame(x = mean(data$x),
                                                y = mean(data$y),
                                                label = paste0("y = ",
                                                               model$coefficients[2] %>% good_digits(),
                                                               "x + ",
                                                               model$coefficients[1] %>% good_digits()))
                                   },

                                   required_aes = c("x", "y")
)

#' Write formula for ols linear model
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
#' geom_point() + geom_lm() + geom_lm_formula()
geom_lm_formula <- function(mapping = NULL, data = NULL,
                            position = "identity", na.rm = FALSE, show.legend = NA,
                            inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatOlsformula, geom = ggplot2::GeomLabel, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
