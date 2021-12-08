#' Function to find Points with High Cooks Distances
#'
#' @param data the data set to be used
#' @param scales
#'
#' @return the points with high cooks values
#' @export
#'
#' @examples
#' library(dplyr)
#' library(magrittr)
#' cars%>%
#' mutate(x = speed, y = dist) %>%
#' select(x,y) %>%
#' compute_group_cooks_points()
compute_group_cooks_points <- function(data, scales) {

  model.lm <- lm(formula = data$y ~ data$x)

  cooks = cooks.distance(model = model.lm)

  data.frame(x = data$x,
             y = data$y)[cooks >
                           quantile(cooks, .90),]
  # [] filter out the points that we want [rows, columns]

}

StatCooksPoint <- ggplot2::ggproto(`_class` = "StatCooksPoint",
                              `_inherit` = ggplot2::Stat,
                              required_aes = c("x", "y"),
                              compute_group = compute_group_cooks_points)

#' Returns a scatter plot with points that are colored if they have a high cooks distance
#'
#' @inheritParams geom_point
#'
#' @return a scatter plot with points that are colored if they have a high cooks distance
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(magrittr)
#' cars %>%
#' ggplot(aes(x = speed, y = dist)) +
#' geom_point() +
#' geom_point_high_cooks(color = 'red')
geom_point_high_cooks<- function(mapping = NULL, data = NULL,
                           position = "identity", na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatCooksPoint,
    geom = ggplot2::GeomPoint,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
