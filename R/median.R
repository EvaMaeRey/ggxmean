#####  Xmedian ######

StatXmedian <- ggplot2::ggproto("StatXmedian", ggplot2::Stat,
                     compute_group = function(data, scales) {

                       median(data$x) %>%
                         data.frame(x = ., xend = ., y = -Inf, yend = Inf)
                     },

                     required_aes = c("x")
)

#' Draw vertical line at the median of x
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
#' ggplot(data = cars, mapping = aes(x = speed)) + geom_histogram() + geom_xmedian()
geom_xmedian <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatXmedian, geom = ggplot2::GeomSegment, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
