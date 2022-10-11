GeomXmeanlabel <- ggplot2::ggproto("GeomXmeanlabel", ggplot2::Geom,
                                   draw_panel = function(data, panel_params, coord) {

                                     ranges <- coord$backtransform_range(panel_params)

                                     groups <- split(data, factor(data$group))
                                     data <- lapply(groups, function(group) {
                                       group$x <- mean(group$x)
                                       group <- unique(group)
                                       group$y <- (ranges$y[1] + ranges$y[2])/2
                                       group$label <- good_digits(group$x, 3)
                                       group
                                     })
                                     data <- do.call(rbind, data)

                                     GeomLabel$draw_panel(unique(data), panel_params, coord)

                                   },

                                   default_aes = ggplot2::aes(colour = "black", size = 5,
                                                              linetype = 1, fill = "white", alpha = 1),
                                   required_aes = "x",

                                   draw_key = ggplot2::draw_key_label
)



#' Lines defined by mean value of y
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
#' geom_point() + geom_x_mean() + geom_x_mean_label()
geom_x_mean_label <- function(mapping = NULL, data = NULL,
                              ...,
                              x,
                              na.rm = FALSE,
                              show.legend = NA) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = ggplot2::StatIdentity,
    geom = GeomXmeanlabel,
    position = ggplot2::PositionIdentity,
    show.legend = show.legend,
    inherit.aes = TRUE,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

