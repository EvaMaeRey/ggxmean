#####  Xmean ######

StatXmean <- ggplot2::ggproto("StatXmean", Stat,
                     compute_group = function(data, scales) {
                       mean(data$x) %>%
                         data.frame(x = ., xend = ., y = -Inf, yend = Inf)
                     },

                     required_aes = c("x")
)

GeomSegmentdashed <- ggplot2::ggproto("GeomSegmentdashed", ggplot2::GeomSegment,
  default_aes = aes(colour = "black", size = 0.5, linetype = "dashed",
    alpha = NA)
  )

geom_xmean <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatXmean, geom = GeomSegmentdashed, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

##### Ymean ######


StatYmean <- ggplot2::ggproto("StatYmean",
                              ggplot2::Stat,
                     compute_group = function(data, scales) {
                       mean(data$y) %>%
                         data.frame(y = ., yend = ., x = -Inf, xend = Inf)
                     },

                     required_aes = c("y")
)


geom_ymean <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatYmean, geom = GeomSegmentdashed, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#### Xvline #####

StatXvline <- ggplot2::ggproto("StatXvline",
                               ggplot2::Stat,
                     compute_group = function(data, scales) {
                       data$x %>%
                         data.frame(x = ., xend = ., y = -Inf, yend = Inf)
                     },

                     required_aes = c("x")
)


geom_xvline <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatXvline, geom = ggplot2::GeomSegment, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#### Yhline #####

StatYhline <- ggplot2::ggproto("StatYhline",
                               ggplot2::Stat,
                               compute_group = function(data, scales) {
                                 data$y %>%
                                   data.frame(x = -Inf, xend = Inf, y = ., yend = .)
                               },

                               required_aes = c("y")
)


geom_yhline <- function(mapping = NULL, data = NULL,
                        position = "identity", na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE, ...) {
  layer(
    stat = StatYhline, geom = ggplot2::GeomSegment, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

### XDiff


StatXdiff <- ggplot2::ggproto("StatXdiff",
                               ggplot2::Stat,
                               compute_group = function(data, scales) {
                                   data.frame(x = data$x, xend = mean(data$x),
                                              y = data$y, yend = data$y)
                               },

                               required_aes = c("x", "y")
)



geom_xdiff <- function(mapping = NULL, data = NULL,
                        position = "identity", na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE, ...) {
  layer(
    stat = StatXdiff, geom = ggplot2::GeomSegment, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


### YDiff

StatYdiff <- ggplot2::ggproto("StatYdiff",
                              ggplot2::Stat,
                              compute_group = function(data, scales) {
                                data.frame(x = data$x, xend = data$x,
                                           y = data$y, yend = mean(data$y))
                              },

                              required_aes = c("x", "y")
)




geom_ydiff <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatYdiff, geom = ggplot2::GeomSegment, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


### xDiffTimeDiffy

StatXdifftimesydiff <- ggplot2::ggproto("StatYdiff",
                              ggplot2::Stat,
                              compute_group = function(data, scales) {
                                data.frame(xmin = data$x, xmax = mean(data$x),
                                           ymin = data$y, ymax = mean(data$y),
                                           fill = ifelse((data$x - mean(data$x)) *
                                                           (data$y - mean(data$y)) >0 ,
                                                         "plum4", "goldenrod2"))
                              },

                              required_aes = c("x", "y")
)



GeomRecttransparent <- ggplot2::ggproto("GeomRecttransparent",
                                      ggplot2::GeomRect,
                                      default_aes = aes(colour = "black",
                                                        size = 0.3,
                                                        linetype = "dashed",
                                                        alpha = .25, fill = "grey35")
)



geom_diffsmultiplied <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatXdifftimesydiff, geom = GeomRecttransparent, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

