StatBarbrick <- ggplot2::ggproto("StatBarbrick", ggplot2::Stat,

                              compute_group = function(data, scales) {
                                data$x %>%
                                  data.frame(x = .) %>%
                                  mutate(y = 1) %>%
                                  mutate(group = 1:n())
                              },

                              required_aes = c("x")
)



GeomBarrr <- ggplot2::ggproto("GeomBarrr",
                                        ggplot2::GeomBar,
                                        default_aes = ggplot2::aes(colour = "grey35",
                                                                   size = 0.3,
                                                                   linetype = "solid",
                                                                   alpha = 1,
                                                                   fill = "grey85")
)


geom_barbrick <- function(mapping = NULL, data = NULL,
                       position = "stack", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatBarbrick, geom = GeomBarrr, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# ggplot(data = mtcars) +
#   aes(x = factor(cyl)) +
#   geom_barbrick() +
#   facet_wrap(~ gear)



# StatBarbrick <- ggplot2::ggproto("StatBarbrick", ggplot2::Stat,
#
#                                  compute_group = function(data, scales) {
#                                    data$x %>%
#                                      data.frame(x = .) %>%
#                                      # mutate(y = 1) %>%
#                                      mutate(group = 1:n())
#                                  },
#
#                                  required_aes = c("x")
# )



# geom_histbrick <- function (mapping = NULL, data = NULL, stat = "bin", position = "stack",
#                             ..., binwidth = NULL, bins = NULL, na.rm = FALSE, orientation = NA,
#                             show.legend = NA, inherit.aes = TRUE)
# {
#   layer(data = data, mapping = mapping, stat = stat, geom = GeomBarrr,
#         position = position, show.legend = show.legend, inherit.aes = inherit.aes,
#         params = list(binwidth = binwidth, bins = bins, na.rm = na.rm,
#                       orientation = orientation, pad = FALSE, ...))
# }


# ggplot(data = mtcars) +
#    aes(x = drat) +
#    geom_histbrick(bins = 5)
#
# ggplot(data = mtcars) +
#   aes(x = drat) +
#   stat_bin(data = . %>% mutate(group = 1:n()), aes(group = group),
#            color = "grey35", fill = "grey85")
#
# geom_histbrick <- function(){
#
#   stat_bin(data = . %>% mutate(group = 1:n()), aes(group = group),
#                             color = "grey35", fill = "grey85")
#
# }
