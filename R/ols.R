

geom_lm <- function(formula = y ~ x, ...)  {
  geom_smooth(formula = formula, method = "lm",
               ...)
}


#### fitted #####


lm(cars$speed ~ cars$dist) -> model$fitted.values

StatOlsfitted <- ggplot2::ggproto("StatOlsresiduals",
                                     ggplot2::Stat,
                                     compute_group = function(data, scales) {

                                       model <- lm(data$y ~ data$x)
                                       data$x %>%
                                         data.frame(x = data$x,
                                                    y = model$fitted.values)
                                     },

                                     required_aes = c("x", "y")
)


geom_lmfitted <- function(mapping = NULL, data = NULL,
                             position = "identity", na.rm = FALSE,
                          show.legend = NA,
                             inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatOlsfitted, geom = ggplot2::GeomPoint, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#### residuals #####


# lm(cars$speed~ cars$dist) -> model$fitted.values

StatOlsresiduals <- ggplot2::ggproto("StatOlsresiduals",
                               ggplot2::Stat,
                               compute_group = function(data, scales) {

                                 model <- lm(data$y ~ data$x)

                                   data.frame(x = data$x,
                                              xend = data$x,
                                              y = model$fitted.values,
                                              yend = data$y)
                               },

                               required_aes = c("x", "y")
)


geom_lmresiduals <- function(mapping = NULL, data = NULL,
                        position = "identity", na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatOlsresiduals, geom = ggplot2::GeomSegment, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#
#
# # residuals squared
#
# StatOlsresidualssquared <- ggplot2::ggproto("StatOlsresidualssquared",
#                                      ggplot2::Stat,
#                                      compute_group = function(data, scales) {
#
#                                        model <- lm(data$y ~ data$x)
#
#                                        data.frame(
#                                                   xmax = data$x,
#                                                   ymax = model$fitted.values,
#                                                   xmin = data$x,
#                                                   ymax = data$y,
#                                                   )
#                                      },
#
#                                      required_aes = c("x", "y")
# )
#
#
# geom_lmresidualssquared <- function(mapping = NULL, data = NULL,
#                              position = "identity", na.rm = FALSE, show.legend = NA,
#                              inherit.aes = TRUE, ...) {
#   ggplot2::layer(
#     stat = StatOlsresidualssquared, geom = ggplot2::GeomRect, data = data, mapping = mapping,
#     position = position, show.legend = show.legend, inherit.aes = inherit.aes,
#     params = list(na.rm = na.rm, ...)
#   )
# }

### text formula

good_digits <- function(x){

formatC(signif(x, digits = 3),
              digits = 3,
              format="fg",
              flag = "#")


}

StatOlsformula <- ggplot2::ggproto("StatOlsformula",
                                  ggplot2::Stat,
                                  compute_group = function(data, scales) {

                                    options(digits = 2)

                                    model <- lm(data$y ~ data$x)

                                    data.frame(x = mean(data$x),
                                               y = mean(data$y),
                                               label = paste0("y = ",
                                                              model$coefficients[2] %>% good_digits(),
                                                              "x + ",
                                                              model$coefficients[1] %>% good_digits()))
                                  },

                                  required_aes = c("x", "y")
)

geom_lmformula <- function(mapping = NULL, data = NULL,
                                   position = "identity", na.rm = FALSE, show.legend = NA,
                                   inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatOlsformula, geom = ggplot2::GeomLabel, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}




