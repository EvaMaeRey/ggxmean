GeomYmeanlabel <- ggplot2::ggproto("GeomYmeanlabel", ggplot2::Geom,
                              draw_panel = function(data, panel_params, coord) {

                                ranges <- coord$backtransform_range(panel_params)

                                data$y    <- mean(data$y)
                                # data$yend <- mean(data$y)
                                data$x    <- (ranges$x[1] + ranges$x[2])/2
                                # data$xend <- ranges$x[2]
                                data$label <- mean(data$y)

                                GeomLabel$draw_panel(unique(data), panel_params, coord)

                              },

                              default_aes = ggplot2::aes(colour = "black", size = 5,
                                                         linetype = 1, fill = "white", alpha = 1),
                              required_aes = "y",

                              draw_key = ggplot2::draw_key_label
)



#' Lines defined by mean value of y
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
#' geom_point() + geom_y_mean_label() + geom_y_mean()
geom_y_mean_label <- function(mapping = NULL, data = NULL,
                       ...,
                       y,
                       na.rm = FALSE,
                       show.legend = NA) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = ggplot2::StatIdentity,
    geom = GeomYmeanlabel,
    position = ggplot2::PositionIdentity,
    show.legend = show.legend,
    inherit.aes = TRUE,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

