# #####  Xmax ######
#
# StatBrick <- ggplot2::ggproto("StatBrick", ggplot2::Stat,
#                      compute_group = function(data, scales) {
#
#                          data.frame(x = data$x,
#                                     group = 1:nrow(data))
#
#                        },
#
#                      required_aes = c("x")
# )
#
# # GeomSegmentdashed <- ggplot2::ggproto("GeomSegmentdashed", ggplot2::GeomSegment,
# #   default_aes = ggplot2::aes(colour = "black", size = 0.5, linetype = "dashed",
# #     alpha = NA)
# #   )
#
# geom_brick <- function (mapping = NULL, data = NULL, stat = "bin", position = "stack",
#                         ..., binwidth = NULL, bins = NULL, na.rm = FALSE, orientation = NA,
#                         show.legend = NA, inherit.aes = TRUE)
# {
#   layer(data = data, mapping = mapping, stat = stat, geom = GeomBar,
#         position = position, show.legend = show.legend, inherit.aes = inherit.aes,
#         params = list(binwidth = binwidth, bins = bins, na.rm = na.rm,
#                       orientation = orientation, pad = FALSE, ...))
# }
#
#
# # ggplot(cars) +
# #   aes(x = dist) +
# #   geom_brick(group = -1L)
#
# # geom_xmax <- function(mapping = NULL, data = NULL,
# #                        position = "identity", na.rm = FALSE, show.legend = NA,
# #                        inherit.aes = TRUE, ...) {
# #   ggplot2::layer(
# #     stat = StatXmax, geom = ggplot2::GeomSegmentdashed, data = data, mapping = mapping,
# #     position = position, show.legend = show.legend, inherit.aes = inherit.aes,
# #     params = list(na.rm = na.rm, ...)
# #   )
# # }
