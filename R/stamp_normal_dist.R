compute_group_norm_dist_stamp <- function(data, scales, mean = 0,
         sd = 1, height = 1,
         sd_min = -4, sd_max = 4, q_min = NULL, q_max = NULL) {

  if(!is.null(q_min) | !is.null(q_max)){

    if(is.null(q_min)){q_min = .00003} # about 4sd below
    if(is.null(q_max)){q_max = .99997} # about 4sd above

    sd_min = qnorm(q_min)
    sd_max = qnorm(q_max)
  }

  seq(sd_min, sd_max, .01) %>%
    tibble::tibble(x = .) %>%
    dplyr::mutate(y = dnorm(x)*height) %>%
    dplyr::mutate(x = x*sd + mean) %>%
    dplyr::mutate(alpha = .3)

}




StampDnorm <- ggplot2::ggproto("StampDnorm",
                              ggplot2::Stat,
                              compute_group = compute_group_norm_dist_stamp,
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
#' library(tidyverse)
#' ggxmean:::stamp_space() +
#'   stamp_normal_dist(alpha = .5, height = 1, fill = "magenta") +
#'   stamp_normal_dist(sd_min = -5, sd_max = -1.96, height = 1) +
#'   stamp_normal_dist(q_min = 0.25, height = 1)
#'
#' ggxmean:::stamp_space() +
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

    params = list(na.rm = na.rm, outline.type = outline.type, ...)
  )
}


compute_group_norm_dist_zlines_stamp <- function(data, scales, mean = 0,
                                                 sd = 1, height = 1,
                                                 sd_min = -4, sd_max = 4) {


  -4:4 %>%
    tibble::tibble(x = .) %>%
    dplyr::mutate(y = dnorm(x)*height/sd) %>%
    dplyr::mutate(yend = 0) %>%
    dplyr::mutate(x = x*sd + mean) %>%
    dplyr::mutate(xend = x) %>%
    dplyr::mutate(linetype = "dashed")

  # seq(sd_min, sd_max, .01) %>%
  #   tibble::tibble(x = .) %>%
  #   dplyr::mutate(y = dnorm(x)*height) %>%
  #   dplyr::mutate(x = x*sd + mean) %>%
  #   dplyr::mutate(alpha = .3)

}

StampDnormzlines <- ggplot2::ggproto("StampDnormzlines",
                                     ggplot2::Stat,
                                     compute_group = compute_group_norm_dist_zlines_stamp,
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
#' library(tidyverse)
#' ggxmean:::stamp_space() +
#'   stamp_normal_dist(alpha = .5, height = 1, fill = "magenta") +
#'   stamp_normal_dist(sd_min = -5, sd_max = -1.96, height = 1) +
#'   stamp_normal_dist(q_min = 0.25, height = 1) +
#'   stamp_normal_dist_zlines()
#'
#' ggxmean:::stamp_space() +
#'   stamp_normal_dist(sd_min = -1, sd_max = 1, color = "slateblue",
#'              color = "grey34", outline.type = "full") +
#'   stamp_normal_dist(sd_min = -2, sd_max = 2, color = "slateblue", outline.type = "full") +
#'   stamp_normal_dist(sd_min = -3, sd_max = 3, color = "slateblue", outline.type = "full") +
#'   stamp_normal_dist(sd_min = -4, sd_max = 4, color = "slateblue", outline.type = "full") +
#'   stamp_normal_dist(sd_min = -5, sd_max = 5, color = "slateblue", outline.type = "full")
stamp_normal_dist_zlines <- function(mapping = NULL, data = NULL,
                              position = "identity", na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE, outline.type = "upper", ...) {
  ggplot2::layer(
    stat = StampDnormzlines, geom = ggplot2::GeomSegment, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,

    params = list(na.rm = na.rm, outline.type = outline.type, ...)
  )
}


StampPnorm <- ggplot2::ggproto("StampPnorm",
                               ggplot2::Stat,
                               compute_group = function(data, scales, mean = 0,
                                                        sd = 1, height = 1,
                                                        sd_min = -4, sd_max = 4) {

                                 seq(sd_min, sd_max, .01) %>%
                                   tibble::tibble(x = .) %>%
                                   dplyr::mutate(y = pnorm(x)*height) %>%
                                   dplyr::mutate(x = x*sd + mean) %>%
                                   dplyr::mutate(alpha = .3)

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
#' ggxmean:::stamp_space() +
#'   stamp_normal_prob(alpha = .5, height = 1, fill = "magenta") +
#'   stamp_normal_prob(sd_min = -5, sd_max = -1.96, height = 1) +
#'   stamp_normal_prob(sd_min = 1.98, sd_max = 5, height = 1)
#'
#' ggxmean:::stamp_space() +
#'   stamp_normal_prob(sd_min = -1, sd_max = 1, color = "slateblue",
#'              color = "grey34", outline.type = "full") +
#'   stamp_normal_prob(sd_min = -2, sd_max = 2, color = "slateblue", outline.type = "full") +
#'   stamp_normal_prob(sd_min = -3, sd_max = 3, color = "slateblue", outline.type = "full") +
#'   stamp_normal_prob(sd_min = -4, sd_max = 4, color = "slateblue", outline.type = "full") +
#'   stamp_normal_prob(sd_min = -5, sd_max = 5, color = "slateblue", outline.type = "full")
stamp_normal_prob <- function(mapping = NULL, data = NULL,
                              position = "identity", na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE, outline.type = "upper", ...) {
  ggplot2::layer(
    stat = StampPnorm, geom = ggplot2::GeomLine, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,

    params = list(na.rm = na.rm, outline.type = outline.type,...)
  )
}







