
StampDcauchy <- ggplot2::ggproto("StampDcauchy",
                              ggplot2::Stat,
                              compute_group = function(data, scales, mean = 0,
                                                       sd = 1, height = 1,
                                                       x_min = -15, x_max = 15) {

                                seq(x_min, x_max, .01) %>%
                                  tibble(x = .) %>%
                                  mutate(y = dcauchy(x)*height) %>%
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
#'   stamp_cauchy_dist(alpha = .5, height = 1, fill = "magenta") +
#'   stamp_cauchy_dist(x_min = -5, x_max = -1.96, height = 1) +
#'   stamp_cauchy_dist(x_min = 1.98, x_max = 5, height = 1)
#'
#' ggplot(cars, aes(x = dist)) +
#'   stamp_cauchy_dist(x_min = -1, x_max = 1, color = "slateblue",
#'              color = "grey34", outline.type = "full") +
#'   stamp_cauchy_dist(x_min = -2, x_max = 2, color = "slateblue", outline.type = "full") +
#'   stamp_cauchy_dist(x_min = -3, x_max = 3, color = "slateblue", outline.type = "full") +
#'   stamp_cauchy_dist(x_min = -4, x_max = 4, color = "slateblue", outline.type = "full") +
#'   stamp_cauchy_dist(x_min = -15, x_max = 15, color = "slateblue", outline.type = "full")
stamp_cauchy_dist <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, outline.type = "upper", ...) {
  ggplot2::layer(
    stat = StampDcauchy, geom = ggplot2::GeomArea, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,

    params = list(na.rm = na.rm, outline.type = outline.type,...)
  )
}




StampPcauchy <- ggplot2::ggproto("StampPcauchy",
                               ggplot2::Stat,
                               compute_group = function(data, scales, mean = 0,
                                                        sd = 1, height = 1,
                                                        x_min = -15, x_max = 15) {

                                 seq(x_min, x_max, .01) %>%
                                   tibble(x = .) %>%
                                   mutate(y = pcauchy(x)*height) %>%
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
#'   stamp_cauchy_prob(alpha = .5, height = 1, fill = "magenta") +
#'   stamp_cauchy_prob(x_min = -5, x_max = -1.96, height = 1) +
#'   stamp_cauchy_prob(x_min = 1.98, x_max = 5, height = 1)
#'
#' ggplot(cars, aes(x = dist)) +
#'   stamp_cauchy_prob(x_min = -1, x_max = 1, color = "slateblue",
#'              color = "grey34", outline.type = "full") +
#'   stamp_cauchy_prob(x_min = -2, x_max = 2, color = "slateblue", outline.type = "full") +
#'   stamp_cauchy_prob(x_min = -3, x_max = 3, color = "slateblue", outline.type = "full") +
#'   stamp_cauchy_prob(x_min = -4, x_max = 4, color = "slateblue", outline.type = "full") +
#'   stamp_cauchy_prob(x_min = -5, x_max = 5, color = "slateblue", outline.type = "full")
stamp_cauchy_prob <- function(mapping = NULL, data = NULL,
                              position = "identity", na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE, outline.type = "upper", ...) {
  ggplot2::layer(
    stat = StampPcauchy, geom = ggplot2::GeomLine, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,

    params = list(na.rm = na.rm, outline.type = outline.type,...)
  )
}





