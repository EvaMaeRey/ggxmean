compute_group_dnorm <- function(data, scales, height = 1,
         sd_min = -4, sd_max = 4) {

  sd <- sd(data$x)
  mean <- mean(data$x)

  seq(sd_min, sd_max, .01) %>%
    tibble(x = .) %>%
    mutate(y = dnorm(x)*height/sd) %>%
    mutate(x = x*sd + mean) %>%
    mutate(alpha = .4)

}

StatDnorm <- ggplot2::ggproto("StatDnorm",
                              ggplot2::Stat,
                              compute_group = compute_group_dnorm,
                              required_aes = c("x"))

#' Fits normal distribution based on standard deviation
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
#' ggplot(faithful, aes(x = waiting)) +
#'   geom_rug() +
#'   geom_histogram(aes(y = ..density..)) +
#'   geom_normal_dist(fill = "magenta") +
#'   facet_wrap(facets = vars( eruptions > 3 ), ncol = 1)
geom_normal_dist <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, outline.type = "upper", ...) {
  ggplot2::layer(
    stat = StatDnorm, geom = ggplot2::GeomArea, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,

    params = list(na.rm = na.rm, outline.type = outline.type,...)
  )
}
