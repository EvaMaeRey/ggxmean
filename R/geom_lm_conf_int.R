#### confint #####


StatOlsconfint <- ggplot2::ggproto("StatOlsconfint",
                                   ggplot2::Stat,
                                   compute_group = function(data, scales, level = .95, num_breaks = 100) {

                                     model <- lm(y ~ x, data = data)

                                     new_x_df <- seq(min(data$x), max(data$x),
                                                     length.out = num_breaks) %>%
                                       data.frame(x = .)

                                     predict(model,
                                             newdata = new_x_df,
                                             interval = "confidence",
                                             level = level
                                     ) ->
                                       predict_df

                                     data.frame(x = new_x_df$x,
                                                xend = new_x_df$x,
                                                xmin = new_x_df$x,
                                                xmax = new_x_df$x,
                                                y = predict_df[,2],
                                                yend = predict_df[,3],
                                                ymin = predict_df[,2],
                                                ymax = predict_df[,3],
                                                alpha = .3)
                                   },

                                   required_aes = c("x", "y")
)


#' Drawing prediction interval for OLS linear model
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
#' geom_point() + geom_lm() + geom_lm_conf_int(level = .8)
geom_lm_conf_int <- function(mapping = NULL, data = NULL,
                             position = "identity", na.rm = FALSE, show.legend = NA,
                             inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatOlsconfint, geom = ggplot2::GeomRibbon, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}



#' Drawing prediction interval for OLS linear model as segments
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
#' geom_point() + geom_lm() + geom_lm_conf_int_segments(num_breaks = 100)
geom_lm_conf_int_segments <- function(mapping = NULL, data = NULL,
                                      position = "identity", na.rm = FALSE, show.legend = NA,
                                      inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatOlsconfint, geom = ggplot2::GeomSegment, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
