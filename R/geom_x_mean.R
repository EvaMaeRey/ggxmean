### xmean

StatXmean <- ggplot2::ggproto("StatXmean",
                              ggplot2::Stat,
                              compute_group = function(data, scales) {

                                data.frame(x = mean(data$x))

                              },

                              required_aes = c("x")
)


#' Place point at mean of x and mean of y
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
#' library(ggplot2)
#' ggplot(cars) + aes(x = speed, y = dist) +
#' geom_point() + geom_x_mean(size = 1.5) + aes(color = speed > 14)
geom_x_mean <- function(mapping = NULL, data = NULL,
                        position = "identity", na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE, ...) {

  ggplot2::layer(
    stat = StatXmean, geom = GeomXline, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )

}






