
StatYdiff <- ggplot2::ggproto("StatYdiff",
                              ggplot2::Stat,
                              compute_group = function(data, scales) {
                                data.frame(y = data$y, yend = mean(data$y),
                                           x = data$x, xend = data$x,
                                           color = ifelse((data$y - mean(data$y))  > 0,
                                                          "plum4", "goldenrod2"))
                              },

                              required_aes = c("x", "y")
)



#' Title
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
geom_xy_xymean <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatYdiff, geom = ggplot2::GeomSegment, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
