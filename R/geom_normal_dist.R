compute_group_dnorm <- function(data, scales, height = 1,
         sd_min = -4, sd_max = 4, q_min = NULL, q_max = NULL) {

  sd <- sd(data$x)
  mean <- mean(data$x)

  if(!is.null(q_min) | !is.null(q_max)){

    if(is.null(q_min)){q_min = .00003} # about 4sd below
    if(is.null(q_max)){q_max = .99997} # about 4sd above

    sd_min = qnorm(q_min)
    sd_max = qnorm(q_max)
  }

  seq(sd_min, sd_max, .01) %>%
    tibble::tibble(x = .) %>%
    dplyr::mutate(y = dnorm(x)*height/sd) %>%
    dplyr::mutate(x = x*sd + mean) %>%
    dplyr::mutate(alpha = .4)

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
#'   facet_wrap(facets = vars( eruptions > 3 ), ncol = 1) +
#'   geom_normal_dist(q_min = .99)
#'
#' ggplot(faithful, aes(x = waiting)) +
#'   geom_density() +
#'   geom_normal_dist(q_min = .05, q_max = 1-.05)
geom_normal_dist <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, outline.type = "upper", ...) {
  ggplot2::layer(
    stat = StatDnorm,
    geom = ggplot2::GeomArea,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,

    params = list(na.rm = na.rm, outline.type = outline.type,...)
  )
}



########

compute_group_dnorm_zlines <- function(data, scales, height = 1,
                                       sd_min = -4, sd_max = 4) {

  sd <- sd(data$x)
  mean <- mean(data$x)


  -4:4 %>%
    tibble::tibble(x = .) %>%
    dplyr::mutate(y = dnorm(x)*height/sd) %>%
    dplyr::mutate(yend = 0) %>%
    dplyr::mutate(x = x*sd + mean) %>%
    dplyr::mutate(xend = x) %>%
    dplyr::mutate(linetype = "dashed")

}


StatDnormzlines <- ggplot2::ggproto("StatDnormzlines",
                                    ggplot2::Stat,
                                    compute_group = compute_group_dnorm_zlines,
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
#'   geom_normal_dist_zlines() +
#'   facet_wrap(facets = vars( eruptions > 3 ), ncol = 1) +
#'   geom_normal_dist(q_min = .99)
#'
#' ggplot(faithful, aes(x = waiting)) +
#'   geom_density() +
#'   geom_normal_dist(q_min = .05, q_max = 1-.05)
geom_normal_dist_zlines <- function(mapping = NULL, data = NULL,
                             position = "identity", na.rm = FALSE, show.legend = NA,
                             inherit.aes = TRUE, outline.type = "upper", ...) {
  ggplot2::layer(
    stat = StatDnormzlines,
    geom = ggplot2::GeomSegment,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,

    params = list(na.rm = na.rm, outline.type = outline.type,...)
  )
}
