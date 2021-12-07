## Madison put this together

#' A Function to Label Points
#'
#' @param data this is the data set where the points are found
#' @param scales
#'
#' @return this function returns the coordinates of the point as: (x,y)
#' @export
#'
#' @examples
#' library(dplyr)
#' library(magrittr)
#' cars %>%
#'   mutate(x = speed, y = dist) %>%
#'   select(x,y) %>%
#'   compute_group_coordinates()
compute_group_coordinates <- function(data, scales) {

  data.frame(x = data$x,
             y = data$y,
             label = paste0("(", data$x, ",", data$y, ")")

  )
}

StatCoordinate <- ggplot2::ggproto(`_class` = "StatCoordinate",
                 `_inherit` = ggplot2::Stat,
                 required_aes = c("x", "y"),
                 compute_group = compute_group_coordinates)



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
#' geom_text_coordinate(check_overlap = TRUE,
#' nudge_x = .3)
geom_text_coordinate <- function(mapping = NULL, data = NULL,
                                 position = "identity", na.rm = FALSE,
                                 show.legend = NA,
                                 inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatCoordinate,
    geom = ggplot2::GeomText,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
