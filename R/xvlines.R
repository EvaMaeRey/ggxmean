###### Create xvlines ######

create_xvlines <- function(x){
  dplyr::tibble(
    x = x,
    xend = x,
    y = -Inf,
    yend = Inf
  )
}

####  Stat Xvlines ######

StatXvlines <- ggplot2::ggproto("StatXvlines", ggplot2::Stat,
                       setup_params = function(data, params) {
                         params
                       },
                       setup_data = function(data, params) {
                         if (anyDuplicated(data$group)) {
                           data$group <- paste(data$group, seq_len(nrow(data)), sep = "-")
                         }
                         data
                       },
                       compute_panel = function(data, scales, r = 1, n = 50) {
                         cols_to_keep <- setdiff(names(data), c("x"))
                         xvlines <- lapply(seq_len(nrow(data)), function(i) {
                           xvlines_segment <- create_xvlines(data$x[i])
                           cbind(xvlines_segment, unclass(data[i, cols_to_keep]))
                         })
                         do.call(rbind, xvlines)
                       },
                       required_aes = c("x")
)


#' Title
#'
#' @param mapping
#' @param data
#' @param stat
#' @param position
#' @param ...
#' @param r
#' @param n
#' @param arrow
#' @param lineend
#' @param linejoin
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#'
#' @return
#' @export
#'
#' @examples
geom_xvlines <- function(mapping = NULL, data = NULL, stat = "xvlines",
                         position = "identity", ..., r = 1,
                         n = 50, arrow = NULL, lineend = "butt", linejoin = "round",
                         na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSegment,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      arrow = arrow,
      lineend = lineend,
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )
}


## Collective geom at the mean ####

StatXmean <- ggplot2::ggproto("StatXmean", ggplot2::Stat,
                     setup_params = function(data, params) {
                       params
                     },
                     setup_data = function(data, params) {
                       if (anyDuplicated(data$group)) {
                         data$group <- paste(data$group, seq_len(nrow(data)), sep = "-")
                       }
                       data
                     },
                     compute_panel = function(data, scales, r = 1, n = 50) {
                       data <- data %>%
                         dplyr::summarize(x = mean(x, na.rm = T)) %>%
                         dplyr::ungroup() %>%
                         dplyr::mutate(group = dplyr::row_number())
                       cols_to_keep <- setdiff(names(data), c("x"))
                       xmean <- lapply(seq_len(nrow(data)), function(i) {
                         xmean_segment <- create_xvlines(data$x[i])
                         cbind(xmean_segment, unclass(data[i, cols_to_keep]))
                       })
                       do.call(rbind, xmean)
                     },
                     required_aes = c("x")
)



#' Title
#'
#' @param mapping
#' @param data
#' @param stat
#' @param position
#' @param ...
#' @param r
#' @param n
#' @param arrow
#' @param lineend
#' @param linejoin
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#'
#' @return
#' @export
#'
#' @examples
geom_xmean <- function(mapping = NULL, data = NULL, stat = "xmean",
                       position = "identity", ..., r = 1,
                       n = 50, arrow = NULL, lineend = "butt", linejoin = "round",
                       na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSegment,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      arrow = arrow,
      lineend = lineend,
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )
}
