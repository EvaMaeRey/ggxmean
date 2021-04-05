
StampDnorm <- ggplot2::ggproto("StampDnorm",
                              ggplot2::Stat,
                              compute_group = function(data, scales, mean = 0,
                                                       sd = 1, height = 1,
                                                       sd_min = -4, sd_max = 4) {

                                seq(sd_min, sd_max, .01) %>%
                                  tibble(x = .) %>%
                                  mutate(y = dnorm(x)*height) %>%
                                  mutate(x = x*sd + mean) %>%
                                  mutate(alpha = .3)

                              },

                              required_aes = c("x")
)

#' Title
#'
#' @param mapping
#' @param data
#' @param position
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#' @param outline.type
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(cars, aes(x = dist)) +
#'   stamp_normal_dist(alpha = .5, height = 1, fill = "magenta") +
#'   stamp_normal_dist(sd_min = -5, sd_max = -1.96, height = 1) +
#'   stamp_normal_dist(sd_min = 1.98, sd_max = 5, height = 1)
#'
#' ggplot(cars, aes(x = dist)) +
#'   stamp_normal_dist(sd_min = -1, sd_max = 1, color = "slateblue",
#'              color = "grey34", outline.type = "full") +
#'   stamp_normal_dist(sd_min = -2, sd_max = 2, color = "slateblue", outline.type = "full") +
#'   stamp_normal_dist(sd_min = -3, sd_max = 3, color = "slateblue", outline.type = "full") +
#'   stamp_normal_dist(sd_min = -4, sd_max = 4, color = "slateblue", outline.type = "full") +
#'   stamp_normal_dist(sd_min = -5, sd_max = 5, color = "slateblue", outline.type = "full")
stamp_normal_dist <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, outline.type = "upper", ...) {
  ggplot2::layer(
    stat = StampDnorm, geom = ggplot2::GeomArea, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,

    params = list(na.rm = na.rm, outline.type = outline.type,...)
  )
}




