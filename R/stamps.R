# stamp_text <- function(...){
#
# annotate(geom = "text", ...)
#
# }
#
#
#
# ggplot(cars) +
#   aes(speed, dist) +
#   geom_point() +
#   stamp_text(label = "hi", x = 15:18, y = 75)
#
#
# write_stamp_function_as_string <- function(geom = "text"){
#
#   paste0('stamp_',geom, ' <- function(...){
#
#   annotate(geom = "', geom, '", ...)
#
# }')
#
# }
#
# write_stamp_function_as_string("segment")
# write_stamp_function_as_string("point")
#
#
# write_stamp_function <- function(geom = "text", function_name = paste0("stamp_", geom)){
#
# assign(x = function_name, value =
#        write_stamp_function_as_string(geom = geom) %>%
#          parse(text = .) %>%
#          eval() )
#
# }
#
# write_stamp_function("text")
# write_stamp_function("label")
#
# assign(x = "function_name", value =
#          write_stamp_function_as_string(geom = "label") %>%
#          parse(text = .) %>%
#          eval() )
#
# geoms <- c("text", "point", "rect", "tile", "segment", "curve", "label")
#
# for (i in geoms){
#   write_stamp_function(geom = i)
# }
#
#
#
