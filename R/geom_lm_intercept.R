compute_group_ols_intercept <- function(data, scales) {

  # prep
  model <- lm(data$y ~ data$x)

  # output dataframe
  data.frame(y = model$coefficients[1],
             x = 0)

}


StatOlsintercept <- ggplot2::ggproto("StatOlsintercept",
                                     ggplot2::Stat,
                                     required_aes = c("x", "y"),
                                     compute_group = compute_group_ols_intercept

)



#' Intercept
#'
#' @param mapping
#' @param data
#' @param position
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(cars) + aes(x = speed, y = dist) +
#' geom_point() + geom_lm() + geom_lm_intercept(color = "blue")
geom_lm_intercept <- function(mapping = NULL, data = NULL,
                              position = "identity", na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatOlsintercept, geom = ggplot2::GeomPoint, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
