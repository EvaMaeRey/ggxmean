#####  Xmin ######

StatXmin <- ggplot2::ggproto("StatXmin", ggplot2::Stat,
                     compute_group = function(data, scales) {
                       min(data$x) %>%
                         data.frame(x = ., xend = ., y = -Inf, yend = Inf)
                     },

                     required_aes = c("x")
)

GeomSegmentdashed <- ggplot2::ggproto("GeomSegmentdashed", ggplot2::GeomSegment,
  default_aes = ggplot2::aes(colour = "black", size = 0.5, linetype = "dashed",
    alpha = NA)
  )

geom_xmin <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatXmin, geom = GeomSegmentdashed, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
