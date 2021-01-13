#####  Xmean ######

StatXmean <- ggplot2::ggproto("StatXmean", ggplot2::Stat,
                     compute_group = function(data, scales) {
                       mean(data$x) %>%
                         data.frame(x = ., xend = ., y = -Inf, yend = Inf)
                     },

                     required_aes = c("x")
)

GeomSegmentdashed <- ggplot2::ggproto("GeomSegmentdashed", ggplot2::GeomSegment,
  default_aes = ggplot2::aes(colour = "black", size = 0.5, linetype = "dashed",
    alpha = NA)
  )

geom_xmean <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
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
  ggplot2::layer(
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
  ggplot2::layer(
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
  ggplot2::layer(
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
  ggplot2::layer(
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
  ggplot2::layer(
    stat = StatYdiff, geom = ggplot2::GeomSegment, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


### xDiffTimeDiffy

StatXdifftimesydiff <- ggplot2::ggproto("StatYdiff",
                              ggplot2::Stat,
                              compute_group = function(data, scales) {

                                data.frame(xmin = data$x,
                                           xmax = mean(data$x),
                                           ymin = data$y,
                                           ymax = mean(data$y),
                                           fill = ifelse((data$x - mean(data$x)) *
                                                           (data$y - mean(data$y)) > 0,
                                                         "plum4", "goldenrod2"))

                              },

                              required_aes = c("x", "y")
)



GeomRecttransparent <- ggplot2::ggproto("GeomRecttransparent",
                                      ggplot2::GeomRect,
                                      default_aes = ggplot2::aes(colour = "black",
                                                        size = 0.3,
                                                        linetype = "solid",
                                                        alpha = .25,
                                                        fill = "grey35")
)



geom_diffsmultiplied <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatXdifftimesydiff, geom = GeomRecttransparent, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )

}

### x 1 sd

StatX1sd <- ggplot2::ggproto("StatX1sd", ggplot2::Stat,
                              compute_group = function(data, scales) {

                                mean(data$x) + sd(data$x) %>%
                                  data.frame(x = ., xend = ., y = -Inf, yend = Inf) ->
                                upper

                                mean(data$x) - sd(data$x) %>%
                                  data.frame(x = ., xend = ., y = -Inf, yend = Inf) ->
                                lower

                                rbind(upper, lower)

                              },

                              required_aes = c("x")
)

GeomSegmentdashed <- ggplot2::ggproto("GeomSegmentdashed", ggplot2::GeomSegment,
                                      default_aes = ggplot2::aes(colour = "black", size = 0.5, linetype = "dashed",
                                                                 alpha = NA)
)

geom_x1sd <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatX1sd, geom = GeomSegmentdashed, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}



### y 1 sd

StatY1sd <- ggplot2::ggproto("StatY1sd", ggplot2::Stat,
                             compute_group = function(data, scales) {

                               mean(data$y) + sd(data$y) %>%
                                 data.frame(y = ., yend = ., x = -Inf, xend = Inf) ->
                                 upper

                               mean(data$y) - sd(data$y) %>%
                                 data.frame(y = ., yend = ., x = -Inf, xend = Inf) ->
                                 lower

                               rbind(upper, lower)

                             },

                             required_aes = c("y")
)

GeomSegmentdashed <- ggplot2::ggproto("GeomSegmentdashed", ggplot2::GeomSegment,
                                      default_aes = ggplot2::aes(colour = "black", size = 0.5, linetype = "dashed",
                                                                 alpha = NA)
)

geom_y1sd <- function(mapping = NULL, data = NULL,
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatY1sd, geom = GeomSegmentdashed, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


### r squared equals 1

StatRsq1 <- ggplot2::ggproto("StatRsq1", ggplot2::Stat,
                             compute_group = function(data, scales) {

                                 data.frame(ymin = mean(data$y),
                                            ymax = mean(data$y) + sd(data$y),
                                            xmin = mean(data$x),
                                            xmax = mean(data$x) + sd(data$x),
                                            fill = "plum4")

                             },

                             required_aes = c("x", "y")
)


geom_rsq1 <- function(mapping = NULL, data = NULL,
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatRsq1, geom = GeomRecttransparent, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


### xymean

StatXymean <- ggplot2::ggproto("StatXymean",
                              ggplot2::Stat,
                              compute_group = function(data, scales) {

                                  data.frame(y = mean(data$y),
                                             x = mean(data$x))

                              },

                              required_aes = c("x", "y")
)


geom_xymean <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {

  ggplot2::layer(
    stat = StatXymean, geom = ggplot2::GeomPoint, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )

}






