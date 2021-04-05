StatXdiff <- ggplot2::ggproto("StatXdiff",
                              ggplot2::Stat,
                              compute_group = function(data, scales) {
                                data.frame(x = data$x, xend = mean(data$x),
                                           y = data$y, yend = data$y,
                                           color = ifelse((data$x - mean(data$x))  > 0,
                                                          "plum4", "goldenrod2"))
                              },

                              required_aes = c("x", "y")
)




geom_x_xmean <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatXdiff, geom = ggplot2::GeomSegment, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
