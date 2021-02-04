##### These are the geoms that you can use to talk about ####
### covariance, variance, standard deviation, and correlation ####



### XDiff #####


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


### YDiff #####

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


### xDiffTimeDiffy ####

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

### x 1 sd #####

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


geom_x1sd <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatX1sd, geom = ggplot2::GeomSegment, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}



### x 2 sd #####

StatX2sd <- ggplot2::ggproto("StatX2sd", ggplot2::Stat,
                             compute_group = function(data, scales) {

                               mean(data$x) + 2*sd(data$x) %>%
                                 data.frame(x = ., xend = ., y = -Inf, yend = Inf) ->
                                 upper

                               mean(data$x) - 2*sd(data$x) %>%
                                 data.frame(x = ., xend = ., y = -Inf, yend = Inf) ->
                                 lower

                               rbind(upper, lower)

                             },

                             required_aes = c("x")
)


geom_x2sd <- function(mapping = NULL, data = NULL,
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatX2sd, geom = ggplot2::GeomSegment, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


### x 2 sd #####

StatX3sd <- ggplot2::ggproto("StatX3sd", ggplot2::Stat,
                             compute_group = function(data, scales) {

                               mean(data$x) + 3*sd(data$x) %>%
                                 data.frame(x = ., xend = ., y = -Inf, yend = Inf) ->
                                 upper

                               mean(data$x) - 3*sd(data$x) %>%
                                 data.frame(x = ., xend = ., y = -Inf, yend = Inf) ->
                                 lower

                               rbind(upper, lower)

                             },

                             required_aes = c("x")
)


geom_x3sd <- function(mapping = NULL, data = NULL,
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatX3sd, geom = ggplot2::GeomSegment, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


### y 1 sd #####

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
    stat = StatY1sd, geom = ggplot2::GeomSegment, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


### r squared equals 1  #####

StatXy1sd <- ggplot2::ggproto("StatXy1sd", ggplot2::Stat,
                             compute_group = function(data, scales) {

                                 data.frame(ymin = mean(data$y),
                                            ymax = mean(data$y) + sd(data$y),
                                            xmin = mean(data$x),
                                            xmax = mean(data$x) + sd(data$x),
                                            fill = "plum4")

                             },

                             required_aes = c("x", "y")
)

geom_xy1sd <- function(mapping = NULL, data = NULL,
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatXy1sd, geom = GeomRecttransparent, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}



#### mean average
##### This is the simple average - no Bessel correction n-1, but above our sd is just base r which is

StatAveragearea <- ggplot2::ggproto("StatAveragearea", ggplot2::Stat,
                              compute_group = function(data, scales) {

                                mean_area <- mean((data$x - mean(data$x)) * (data$y - mean(data$y)))

                                data.frame(ymin = mean(data$y),
                                           ymax = mean(data$y) + mean_area/sd(data$x),
                                           xmin = mean(data$x),
                                           xmax = mean(data$x) + sd(data$x),
                                           fill = "plum4")

                              },

                              required_aes = c("x", "y")
)


geom_xydiffsmean <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatAveragearea, geom = GeomRecttransparent, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}



#### corr label

Corrlabel <- ggplot2::ggproto("Corrlabel", ggplot2::Stat,
                                    compute_group = function(data, scales) {

                                      mean_area <- mean((data$x - mean(data$x)) * (data$y - mean(data$y)))

                                      data.frame(
                                                 y = mean(data$y) + mean_area/sd(data$x)/2,
                                                 x = mean(data$x) + sd(data$x)/2,
                                                 label = cor(data$x, data$y) %>% good_digits())

                                    },

                                    required_aes = c("x", "y")
)


geom_corrlabel <- function(mapping = NULL, data = NULL,
                             position = "identity", na.rm = FALSE, show.legend = NA,
                             inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = Corrlabel, geom = ggplot2::GeomLabel, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}



### r squared equals 1 ######

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





return_md_equation_steps <- function(equation = "cov"){

if(equation == "cov"){
c("",
  "## $$\\mu_x$$",
  "## $$\\mu_y$$",
  "## $$x_i-\\mu_x$$",
  "## $$y_i-\\mu_y$$",
  "## $$\\sum_{i=1}^n (x_i-\\mu_x)(y_i-\\mu_y)$$",
  "## $$\\frac{\\sum_{i=1}^n (x_i-\\mu_x)(y_i-\\mu_y)}{n}$$")
}

var_equation <- c("",
                  "## $$\\mu_x$$",
                  "## $$\\mu_x$$",
                  "## $$x_i-\\mu_x$$",
                  "## $$x_i-\\mu_x$$",
                  "## $$\\sum_{i=1}^n (x_i-\\mu_x)^2$$",
                  "## $$\\frac{\\sum_{i=1}^n (x_i-\\mu_x)^2}{n}$$"
)


sd_equation <- c("## $$\\frac{\\sum_{i=1}^n (x_i-\\mu_x)^2}{n}$$",
                 "## $$\\sqrt\\frac{\\sum_{i=1}^n (x_i-\\mu_x)^2}{n}$$")

cov_to_cor_equation <- c("## $$\\frac{\\sum_{i=1}^n (x_i-\\mu_x)(y_i-\\mu_y)}{n}$$",
                         "## $$\\frac{\\sum_{i=1}^n (x_i-\\mu_x)(y_i-\\mu_y)}{n*\\sigma_x}$$",
                         "## $$\\frac{\\sum_{i=1}^n (x_i-\\mu_x)(y_i-\\mu_y)}{n*\\sigma_x\\sigma_y}$$",
                         "## $$\\frac{\\sum_{i=1}^n (x_i-\\mu_x)(y_i-\\mu_y)}{n*\\sigma_x\\sigma_y}$$")


}

