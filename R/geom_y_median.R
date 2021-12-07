draw_panel_ymedianline <- function(data, panel_params, coord) {

  ranges <- coord$backtransform_range(panel_params)

  data$y    <- median(data$y)
  data$yend <- median(data$y)
  data$x    <- ranges$x[1]
  data$xend <- ranges$x[2]

  GeomSegment$draw_panel(unique(data), panel_params, coord)

}

GeomYmedianline <- ggplot2::ggproto(`_class` = "GeomYmedianline",
                                    `_inherit` = ggplot2::Geom,
                                    required_aes = "y",
                                    draw_panel = draw_panel_ymedianline,
                                    default_aes = ggplot2::aes(colour = "black", size = 0.5,
                                                         linetype = 1, alpha = NA),
                                    draw_key = ggplot2::draw_key_vline
)


#' Lines defined by median value of y
#'
#'
#' @inheritParams ggplot2::geom_line
#'
#' @return
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(data = cars, mapping = aes(x = speed, y = dist)) +
#' geom_point() +
#' geom_y_median()
geom_y_median <- function(mapping = NULL, data = NULL,
                       ...,
                       y,
                       na.rm = FALSE,
                       show.legend = NA) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = ggplot2::StatIdentity,
    geom = GeomXmedianline,
    position = ggplot2::PositionIdentity,
    show.legend = show.legend,
    inherit.aes = TRUE,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

