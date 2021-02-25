


StatTtestconf <- ggplot2::ggproto("StatTtestconf",
                               ggplot2::Stat,
                               compute_group = function(data, scales, conf.level= .95, y = 0, mu = 0
                                                        ) {

                                 model <- t.test(x = data$x, mu = mu, conf.level = conf.level)

                                 data.frame(x = model$conf.int[1],
                                            yend = y,
                                            y = y,
                                            xend = model$conf.int[2])

                               },

                               required_aes = c("x")
)



geom_ttestconf <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatTtestconf, geom = ggplot2::GeomSegment, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}





StatTdist <- ggplot2::ggproto("StatTdist",
                                  ggplot2::Stat,
                                  compute_group = function(data, scales, height = 1) {

                                    seq(-5, 5, .01) %>%
                                      tibble(x = .) %>%
                                      mutate(y = dt(x, df = length(data$x)-1)*height) %>%
                                      mutate(x = x*(sd(data$x)/sqrt(length(data$x))) + mean(data$x))

                                  },

                                  required_aes = c("x")
)

geom_tdist <- function(mapping = NULL, data = NULL,
                           position = "identity", na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatTdist, geom = ggplot2::GeomArea, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}



