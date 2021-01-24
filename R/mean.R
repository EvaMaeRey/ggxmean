
#####  Xmean ######

StatXmean <- ggplot2::ggproto("StatXmean", ggplot2::Stat,
                              compute_group = function(data, scales) {
                                mean(data$x) %>%
                                  data.frame(x = .,    xend = .,
                                             y = -Inf, yend = Inf)
                              },

                              required_aes = c("x")
)

# GeomSegmentdashed <- ggplot2::ggproto("GeomSegmentdashed", ggplot2::GeomSegment,
#   default_aes = ggplot2::aes(colour = "black", size = 0.5, linetype = "dashed",
#     alpha = NA)
#   )


#' Draw vertical line at the mean of x
#'
#' @param mapping provide you own mapping, x is required
#' @param data provide you own data
#' @param position change geom
#' @param na.rm remove missing values without warning, default is F
#' @param show.legend show legend in plot, default is T
#' @param inherit.aes should the geom inherits aesthetics from global, default is true
#' @param ... other arguments to be passed to the geom, see geom segment
#'
#' @return a ggplot2 layer
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(data = cars, mapping = aes(x = speed)) + geom_histogram() + geom_xmean()
geom_xmean <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatXmean, geom = ggplot2::GeomSegment, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

##### Ymean ######


StatYmean <- ggplot2::ggproto("StatYmean",
                              ggplot2::Stat,
                              compute_group = function(data, scales) {
                                mean(data$y) %>%
                                  data.frame(y = .,    yend = .,
                                             x = -Inf, xend = Inf)
                              },

                              required_aes = c("y")
)

#' Draw vertical line at the mean of y
#'
#' @param mapping provide you own mapping, y is required
#' @param data provide you own data
#' @param position change geom
#' @param na.rm remove missing values without warning, default is F
#' @param show.legend show legend in plot, default is T
#' @param inherit.aes should the geom inherits aesthetics from global, default is true
#' @param ... other arguments to be passed to the geom, see geom segment
#'
#' @return a ggplot2 layer
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(data = cars, mapping = aes(x = speed, y = dist)) + geom_point() + geom_ymean()
geom_ymean <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatYmean, geom = ggplot2::GeomSegment, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


