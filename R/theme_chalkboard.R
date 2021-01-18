


geoms_chalk <- function(){

# https://stackoverflow.com/questions/21174625/ggplot-how-to-set-default-color-for-all-geoms

  ggplot2::update_geom_defaults("point",   list(colour = "lightyellow"))
  ggplot2::update_geom_defaults("segment",   list(colour = "lightyellow"))

# params <- ls(pattern = '^geom_', env = as.environment('package:ggxmean'))
# geoms <- gsub("geom_", "", params)
#
# lapply(geoms, update_geom_defaults, list(colour = "oldlace"))
# lapply(geoms, update_geom_defaults, list(colour = "oldlace"))

}

theme_chalkboard <- function(board_color = "darkseagreen4", chalk_color = "lightyellow"){

  list(
  ggplot2::theme(rect = ggplot2::element_rect(fill =
                                       board_color)),
  ggplot2::theme(text = ggplot2::element_text(color = chalk_color,
                            face = "italic",
                            size = 15)),
  ggplot2::theme(panel.background =
                   ggplot2::element_rect(fill = board_color)),
  ggplot2::theme(legend.key = ggplot2::element_blank()),
  ggplot2::theme(legend.title = ggplot2::element_blank()),
  ggplot2::theme(axis.text =
                   ggplot2::element_text(color = chalk_color)),
  ggplot2::theme(axis.ticks =
                   ggplot2::element_line(color = chalk_color)),
  ggplot2::theme(panel.grid = ggplot2::element_blank())
  )

}

theme_chalkboard_slate <- function(){

  theme_chalkboard("lightskyblue4", "honeydew")

}