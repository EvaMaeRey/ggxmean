#' #' Title
#' #'
#' #' @param mapping
#' #' @param data
#' #' @param position
#' #' @param na.rm
#' #' @param show.legend
#' #' @param inherit.aes
#' #' @param ...
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' #' library(ggplot2)
#' #' mtcars %>%
#' #' mutate(heavy = wt > 3) %>%
#' #' ggplot() +
#' #'   aes(x = factor(cyl)) +
#' #'   geom_brick() +
#' #'   aes(fill = heavy) +
#' #'   aes(group = heavy) +
#' #'   scale_fill_manual(values = c("tan1",
#' #'   "steelblue3"))
#' #'
#' #'
#'
#' geom_brick <- function(mapping = NULL, data = NULL,
#'                        position = "stack", na.rm = FALSE, show.legend = NA,
#'                        inherit.aes = TRUE, ...) {
#'   ggplot2::layer(
#'     stat = StatBarbrick, geom = GeomBarrr, data = data, mapping = mapping,
#'     position = position, show.legend = show.legend, inherit.aes = inherit.aes,
#'     params = list(na.rm = na.rm, ...)
#'   )
#' }
#'
#'
#' StatBarbrick <- ggplot2::ggproto("StatBarbrick", ggplot2::Stat,
#'
#'                                  compute_group = function(data, scales) {
#'                                    data$x %>%
#'                                      data.frame(x = .) %>%
#'                                      arrange(-x) %>%
#'                                      mutate(y = 1) %>%
#'                                      mutate(group = 1:n()) %>%
#'                                      arrange(group)
#'                                  },
#'
#'                                  required_aes = c("x")
#' )
#'
#'
#' GeomBarrr <- ggplot2::ggproto("GeomBarrr",
#'                               ggplot2::GeomBar,
#'                               default_aes = ggplot2::aes(colour = "grey35",
#'                                                          size = 0.3,
#'                                                          linetype = "solid",
#'                                                          alpha = 1,
#'                                                          fill = "grey85")
#' )
#'
#'
#'
#'
#'
