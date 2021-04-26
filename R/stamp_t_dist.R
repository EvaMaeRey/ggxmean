
StampDnorm <- ggplot2::ggproto("StampDnorm",
                              ggplot2::Stat,
                              compute_group = function(data, scales, mean = 0,
                                                       sd = 1, height = 1, df = 10,
                                                       x_min = -4, x_max = 4) {

                                seq(x_min, x_max, .01) %>%
                                  tibble(x = .) %>%
                                  mutate(y = dt(x, df = df)*height) %>%
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
#'   stamp_t_dist(alpha = .5, height = 1, fill = "magenta") +
#'   stamp_t_dist(x_min = -5, x_max = -1.96, height = 1) +
#'   stamp_t_dist(x_min = 1.98, x_max = 5, height = 1)
#'
#' ggplot(cars, aes(x = dist)) +
#'   stamp_t_dist(x_min = -1, x_max = 1, color = "slateblue",
#'              color = "grey34", outline.type = "full") +
#'   stamp_t_dist(x_min = -2, x_max = 2, color = "slateblue", outline.type = "full") +
#'   stamp_t_dist(x_min = -3, x_max = 3, color = "slateblue", outline.type = "full") +
#'   stamp_t_dist(x_min = -4, x_max = 4, color = "slateblue", outline.type = "full") +
#'   stamp_t_dist(x_min = -5, x_max = 5, color = "slateblue", outline.type = "full")
stamp_t_dist <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, outline.type = "upper", ...) {
  ggplot2::layer(
    stat = StampDnorm, geom = ggplot2::GeomArea, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,

    params = list(na.rm = na.rm, outline.type = outline.type,...)
  )
}




StampPt <- ggplot2::ggproto("StampPt",
                               ggplot2::Stat,
                               compute_group = function(data, scales, mean = 0,
                                                        sd = 1, height = 1, df = 15,
                                                        x_min = -4, x_max = 4) {

                                 seq(x_min, x_max, .01) %>%
                                   tibble(x = .) %>%
                                   mutate(y = pt(x, df = df)*height) %>%
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
#'   stamp_t_prob(alpha = .5, height = 1, fill = "magenta") +
#'   stamp_t_prob(x_min = -5, x_max = -1.96, height = 1) +
#'   stamp_t_prob(x_min = 1.98, x_max = 5, height = 1)
#'
#' ggplot(cars, aes(x = dist)) +
#'   stamp_t_prob(x_min = -1, x_max = 1, color = "slateblue",
#'              color = "grey34", outline.type = "full") +
#'   stamp_t_prob(x_min = -2, x_max = 2, color = "slateblue", outline.type = "full") +
#'   stamp_t_prob(x_min = -3, x_max = 3, color = "slateblue", outline.type = "full") +
#'   stamp_t_prob(x_min = -4, x_max = 4, color = "slateblue", outline.type = "full") +
#'   stamp_t_prob(x_min = -5, x_max = 5, color = "slateblue", outline.type = "full")
stamp_t_prob <- function(mapping = NULL, data = NULL,
                              position = "identity", na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE, outline.type = "upper", ...) {
  ggplot2::layer(
    stat = StampPnorm, geom = ggplot2::GeomLine, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,

    params = list(na.rm = na.rm, outline.type = outline.type,...)
  )
}





