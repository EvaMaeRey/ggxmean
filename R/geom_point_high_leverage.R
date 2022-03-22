#' Function to find Points with High leverage Distances
#'
#' @param data the data set to be used
#' @param scales
#'
#' @return the points with high leverage values
#' @export
#'
#' @examples
#' library(dplyr)
#' library(magrittr)
#' cars%>%
#' mutate(x = speed, y = dist) %>%
#' select(x,y) %>%
#' compute_group_leverage_points()
compute_group_leverage_points <- function(data, scales) {

  model.lm <- lm(formula = data$y ~ data$x)

  leverage = hatvalues(model = model.lm)

  data.frame(x = data$x,
             y = data$y)[leverage >
                           quantile(leverage, .90),]
  # [] filter out the points that we want [rows, columns]

}

StatLeveragePoint <- ggplot2::ggproto(`_class` = "StatLeveragePoint",
                                   `_inherit` = ggplot2::Stat,
                                   required_aes = c("x", "y"),
                                   compute_group = compute_group_leverage_points)

#' Returns a scatter plot with points that are colored if they have a high leverage distance
#'
#' @inheritParams geom_point
#'
#' @return a scatter plot with points that are colored if they have a high leverage distance
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(magrittr)
#' cars %>%
#' ggplot(aes(x = speed, y = dist)) +
#' geom_point() +
#' geom_point_high_leverage(color = 'red')
geom_point_high_leverage<- function(mapping = NULL, data = NULL,
                                 position = "identity", na.rm = FALSE,
                                 show.legend = NA,
                                 inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatLeveragePoint,
    geom = ggplot2::GeomPoint,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
