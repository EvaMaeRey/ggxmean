#' A function to find cooks distance of a point
#'
#' @param data the data set to be used
#' @param scales
#'
#' @return Cooks distance of a certain point
#' @export
#'
#' @examples
#' library(dplyr)
#' library(magrittr)
#' cars %>%
#' mutate(x = speed, y = dist) %>%
#' select(x,y) %>%
#' compute_group_cooks_distance(digits = 2)
#'
compute_group_cooks_distance <- function(data, scales, digits = 3) {

  model.lm <- lm(formula = data$y ~ data$x)

  cooks = cooks.distance(model = model.lm)

  data.frame(x = data$x,
             y = data$y,
             label =
               good_digits(x = cooks,
                     digits = digits)
  )
}

StatCooks <- ggplot2::ggproto(`_class` = "StatCooks",
                                   `_inherit` = ggplot2::Stat,
                                   required_aes = c("x", "y"),
                                   compute_group = compute_group_cooks_distance)


#' Returns a scatter plot with points that are labeled
#'
#' @inheritParams geom_text
#'
#' @return a scatter plot with points that are labeled
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(magrittr)
#' cars %>%
#' ggplot(aes(x = speed, y = dist)) +
#' geom_point() +
#' geom_lm() +
#' geom_text_cooks(check_overlap = TRUE,
#' digits = 1)
geom_text_cooks<- function(mapping = NULL, data = NULL,
                                 position = "identity", na.rm = FALSE,
                                 show.legend = NA,
                                 inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatCooks,
    geom = ggplot2::GeomText,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}



