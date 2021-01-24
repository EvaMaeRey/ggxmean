#####  Xq2575 ######

StatXq2575 <- ggplot2::ggproto("StatXq2575", ggplot2::Stat,
                     compute_group = function(data, scales) {
                       quantile(data$x, probs = c(.25,.75)) %>%
                         data.frame(x = ., xend = ., y = -Inf, yend = Inf)
                     },

                     required_aes = c("x")
)

# GeomSegmentdashed <- ggplot2::ggproto("GeomSegmentdashed", ggplot2::GeomSegment,
#   default_aes = ggplot2::aes(colour = "black", size = 0.5, linetype = "dashed",
#     alpha = NA)
#   )

geom_xq2575 <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatXq2575, geom = ggplot2::GeomSegment, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
