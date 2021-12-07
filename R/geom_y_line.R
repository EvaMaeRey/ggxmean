GeomYline <- ggplot2::ggproto("GeomYline", ggplot2::Geom,
                     draw_panel = function(data, panel_params, coord) {

                       ranges <- coord$backtransform_range(panel_params)

                       data$y    <- data$y
                       data$yend <- data$y
                       data$x    <- ranges$x[1]
                       data$xend <- ranges$x[2]

                       GeomSegment$draw_panel(unique(data), panel_params, coord)

                     },

                     default_aes = ggplot2::aes(colour = "black",
                                                size = 0.5,
                                                linetype = 1,
                                                alpha = NA),
                     required_aes = "y",

                     draw_key = ggplot2::draw_key_vline
)


#' Lines defined by values of y
#'
#'
#' @param mapping
#' @param data
#' @param ...
#' @param y
#' @param na.rm
#' @param show.legend
#'
#' @return
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(data = cars, mapping = aes(x = speed, y = dist)) +
#' geom_point() + geom_y_line()
geom_y_line <- function(mapping = NULL, data = NULL,
                       ...,
                       y,
                       na.rm = FALSE,
                       show.legend = NA) {

  # # Act like an annotation
  # if (!missing(x)) {
  #   # Warn if supplied mapping and/or data is going to be overwritten
  #   if (!is.null(mapping)) {
  #     warn_overwritten_args("geom_vline()", "mapping", "x")
  #   }
  #   if (!is.null(data)) {
  #     warn_overwritten_args("geom_vline()", "data", "x")
  #   }
  #
  #   # data <- new_data_frame(list(x = x))
  #   mapping <- aes(x = x)
  #   show.legend <- FALSE
  # }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = ggplot2::StatIdentity,
    geom = GeomYline,
    position = ggplot2::PositionIdentity,
    show.legend = show.legend,
    inherit.aes = TRUE,  # I changed this to true. yay
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}
