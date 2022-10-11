draw_panel_xlabel = function(data, panel_params, coord) {

  ranges <- coord$backtransform_range(panel_params)

  data$x    <- data$x
  data$y    <- mean(ranges$y)
  data$label <- good_digits(data$x, 3)

  GeomLabel$draw_panel(data, panel_params, coord)

}


GeomXlabel <- ggplot2::ggproto("GeomXlabel", ggplot2::Geom,
                     draw_panel = draw_panel_xlabel,

                     default_aes = ggplot2::aes(colour = "black", size = 0.5,
                                       linetype = 1, alpha = 1),
                     required_aes = "x",

                     draw_key = ggplot2::draw_key_label
)



#' Labels defined by values of x
#'
#'
#' @param mapping
#' @param data
#' @param ...
#' @param x
#' @param na.rm
#' @param show.legend
#'
#' @return
#' @export
#'
#' @examples
geom_x_label <- function(mapping = NULL, data = NULL,
                       ...,
                       x,
                       na.rm = FALSE,
                       show.legend = NA) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = ggplot2::StatIdentity,
    geom = GeomXlabel,
    position = ggplot2::PositionIdentity,
    show.legend = show.legend,
    inherit.aes = TRUE,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}



