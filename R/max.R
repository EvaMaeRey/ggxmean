#####  Xmax ######

StatXmax <- ggplot2::ggproto("StatXmax", ggplot2::Stat,
                     compute_group = function(data, scales) {
                       max(data$x) %>%
                         data.frame(x = ., xend = ., y = -Inf, yend = Inf)
                     },

                     required_aes = c("x")
)

#' Draw vertical line at the max of x
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
#' ggplot(data = cars, mapping = aes(x = dist)) + geom_histogram() + geom_xmax()
geom_xmax <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatXmax, geom = ggplot2::GeomSegment, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
