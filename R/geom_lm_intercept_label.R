#' compute a dataframe for plotting intercept coordinates label
#'
#' @param data a data frame with columns x and y
#' @param scales
#' @param digits a positive integer
#'
#' @return a data frame with intercept coordinates of x and y and coordinate label
#' @export
#'
#' @examples
#' library(dplyr)
#' library(magrittr)
#' cars %>%
#' rename(x = speed) %>%
#' rename(y = dist) %>%
#' compute_group_interceptlabel()
#'
compute_group_interceptlabel <- function(data, scales, digits = 3) {

  options(digits = digits)

  model <- lm(data$y ~ data$x)

  data.frame(y = model$coefficients[1],
             x = 0,
             label = paste0("(0, ",
                            model$coefficient[1] %>%
                              good_digits(), ")"))

}


StatOlsinterceptlabel <- ggplot2::ggproto("StatOlsinterceptlabel",
                                           ggplot2::Stat,
                                           compute_group = compute_group_interceptlabel,
                                           required_aes = c("x", "y")
)



#' Label ols linear model intercept
#'
#' @inheritParams ggplot2::geom_text
#'
#' @return
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(cars) +
#' aes(x = speed, y = dist) +
#' geom_point() +
#' geom_text(label = "hi", nudge_x = 1) +
#' geom_lm() +
#' geom_lm_intercept(color = "blue") +
#' geom_lm_intercept_label(hjust = 0, digits = 2, nudge_x = 3)
geom_lm_intercept_label <- function(mapping = NULL,
                                    data = NULL,
                                    position = "identity",
                                    na.rm = FALSE,
                                    show.legend = NA,
                                    inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatOlsinterceptlabel,
    geom = ggplot2::GeomText,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
