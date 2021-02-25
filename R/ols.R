

geom_lm <- function(formula = y ~ x, ...)  {
  ggplot2::geom_smooth(formula = formula, method = "lm", se = FALSE,
               ...)
}


#### fitted #####


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




StatOlspredicty <- ggplot2::ggproto("StatOlspredicty",
                                  ggplot2::Stat,
                                  compute_group = function(data, scales) {

                                    model <- lm(data$y ~ data$x)
                                    data$x %>%
                                      data.frame(x = data$x,
                                                 xend = -Inf,
                                                 yend = model$fitted.values,
                                                 y = model$fitted.values)
                                  },

                                  required_aes = c("x", "y")
)


geom_lmpredicty <- function(mapping = NULL, data = NULL,
                          position = "identity", na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatOlspredicty, geom = ggplot2::GeomSegment, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}



StatOlspredictx <- ggplot2::ggproto("StatOlspredictx",
                                    ggplot2::Stat,
                                    compute_group = function(data, scales) {

                                      model <- lm(data$y ~ data$x)
                                      data$x %>%
                                        data.frame(x = data$x,
                                                   xend = data$x,
                                                   yend = -Inf,
                                                   y = model$fitted.values)
                                    },

                                    required_aes = c("x", "y")
)


geom_lmpredictx <- function(mapping = NULL, data = NULL,
                            position = "identity", na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatOlspredictx, geom = ggplot2::GeomSegment, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}



#### residuals #####



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
              flag = "#") %>%
    stringr::str_replace("\\.$", "")


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



### run

# lm(cars$dist ~cars$speed) -> model

StatOlsrun <- ggplot2::ggproto("StatOlsrun",
                                   ggplot2::Stat,
                                   compute_group = function(data, scales) {

                                     options(digits = 2)

                                     model <- lm(data$y ~ data$x)


                                     data.frame(x = min(data$x),
                                                y = min(data$x) * model$coefficients[2] +
                                                  model$coefficients[1],
                                                yend = min(data$x) * model$coefficients[2] +
                                                  model$coefficients[1],
                                                xend = min(data$x) + 1)

                                   },

                                   required_aes = c("x", "y")
)



geom_lmrun <- function(mapping = NULL, data = NULL,
                                           position = "identity", na.rm = FALSE, show.legend = NA,
                                           inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatOlsrun, geom = ggplot2::GeomSegment, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}



## rise


StatOlsrise <- ggplot2::ggproto("StatOlsrise",
                               ggplot2::Stat,
                               compute_group = function(data, scales) {

                                 options(digits = 2)

                                 model <- lm(data$y ~ data$x)


                                 data.frame(xend = min(data$x) + 1,
                                            yend = (min(data$x) + 1) * model$coefficients[2] +
                                              model$coefficients[1],
                                            y = min(data$x) * model$coefficients[2] +
                                              model$coefficients[1],
                                            x = min(data$x) + 1)

                               },

                               required_aes = c("x", "y")
)



geom_lmrise <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatOlsrise, geom = ggplot2::GeomSegment, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


StatOlsrise10 <- ggplot2::ggproto("StatOlsrise10",
                                ggplot2::Stat,
                                compute_group = function(data, scales) {

                                  options(digits = 2)

                                  model <- lm(data$y ~ data$x)


                                  data.frame(xend = min(data$x) + 10,
                                             yend = (min(data$x) + 10) * model$coefficients[2] +
                                               model$coefficients[1],
                                             y = min(data$x) * model$coefficients[2] +
                                               model$coefficients[1],
                                             x = min(data$x) + 10)

                                },

                                required_aes = c("x", "y")
)



geom_lmrise10 <- function(mapping = NULL, data = NULL,
                        position = "identity", na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatOlsrise10, geom = ggplot2::GeomSegment, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}





StatOlsrun10 <- ggplot2::ggproto("StatOlsrun10",
                                ggplot2::Stat,
                                compute_group = function(data, scales) {

                                  options(digits = 2)

                                  model <- lm(data$y ~ data$x)


                                  data.frame(x = min(data$x),
                                             y = min(data$x) * model$coefficients[2] +
                                               model$coefficients[1],
                                             yend = min(data$x) * model$coefficients[2] +
                                               model$coefficients[1],
                                             xend = min(data$x) + 10)

                                },

                                required_aes = c("x", "y")
)



geom_lmrun10 <- function(mapping = NULL, data = NULL,
                        position = "identity", na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatOlsrun10, geom = ggplot2::GeomSegment, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}



## intercept


StatOlsintercept <- ggplot2::ggproto("StatOlsintercept",
                                ggplot2::Stat,
                                compute_group = function(data, scales) {


                                  model <- lm(data$y ~ data$x)


                                  data.frame(y = model$coefficients[1],
                                             x = 0)

                                },

                                required_aes = c("x", "y")
)



geom_lmintercept <- function(mapping = NULL, data = NULL,
                        position = "identity", na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatOlsintercept, geom = ggplot2::GeomPoint, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}



StatOlsinterceptcoords <- ggplot2::ggproto("StatOlsinterceptcoords",
                                     ggplot2::Stat,
                                     compute_group = function(data, scales) {

                                       options(digits = 2)

                                       model <- lm(data$y ~ data$x)

                                       data.frame(y = model$coefficients[1],
                                                  x = 0,
                                                  label = paste0("(0, ",
                                                                 model$coefficient[1] %>%
                                                                   good_digits(), ")"))

                                     },

                                     required_aes = c("x", "y")
)



geom_lminterceptcoords <- function(mapping = NULL, data = NULL,
                             position = "identity", na.rm = FALSE, show.legend = NA,
                             inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatOlsinterceptcoords, geom = ggplot2::GeomLabel, data = data, mapping = mapping,
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





