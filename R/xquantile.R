#####  Xq2575 ######

StatXquantile <- ggplot2::ggproto("StatXquantile", ggplot2::Stat,
                     compute_group = function(data, scales, quantile) {

                       quantile(data$x, probs = quantile) %>%
                         data.frame(x = ., xend = ., y = -Inf, yend = Inf)

                     },

                     required_aes = c("x")
)


geom_xquantile <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatXquantile, geom = ggplot2::GeomSegment, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# ggplot(cars, aes(dist)) +
#   geom_histogram() +
#   geom_xquantile(quantile = c(0, .25,.5, .75, 1),
#                  color = "magenta",
#                  linetype = "dashed",
#                  size = 1) +
#   geom_boxplot(fill = "plum3", color = "magenta", size = 1) +
#   geom_rug()
