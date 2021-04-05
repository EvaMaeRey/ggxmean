GeomXquantile <- ggplot2::ggproto("GeomXquantile", ggplot2::Geom,
                                  draw_panel = function(data, panel_params, coord, quantile) {

                                    ranges <- coord$backtransform_range(panel_params)

                                    data$x    <- quantile(data$x, probs = quantile)
                                    data$xend <- quantile(data$x, probs = quantile)
                                    data$y    <- ranges$y[1]
                                    data$yend <- ranges$y[2]

                                    GeomSegment$draw_panel(unique(data), panel_params, coord)

                                  },

                                  default_aes = ggplot2::aes(colour = "black", size = 0.5,
                                                             linetype = 1, alpha = NA),
                                  required_aes = "x",

                                  draw_key = ggplot2::draw_key_vline
)

#' Lines defined by quantile value of x
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
#' library(ggplot2)
#' ggplot(data = cars, mapping = aes(x = speed, y = dist)) +
#' geom_point() + geom_x_quantile(quantile = c(.25,.75), linetype = "dashed")
geom_x_quantile <- function(mapping = NULL, data = NULL,
                        ...,
                        x,
                        na.rm = FALSE,
                        show.legend = NA) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = ggplot2::StatIdentity,
    geom = GeomXquantile,
    position = ggplot2::PositionIdentity,
    show.legend = show.legend,
    inherit.aes = TRUE,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}



