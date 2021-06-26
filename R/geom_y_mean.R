### ymean

StatYmean <- ggplot2::ggproto("StatYmean",
                              ggplot2::Stat,
                              compute_group = function(data, scales) {

                                data.frame(y = mean(data$y))

                              },

                              required_aes = c("y")
)


#' Place line at mean of y
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
#' geom_point() + geom_y_mean(size = 1.5) + aes(color = speed > 14)
geom_y_mean <- function(mapping = NULL, data = NULL,
                        position = "identity", na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE, ...) {

  ggplot2::layer(
    stat = StatYmean, geom = GeomYline, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )

}




