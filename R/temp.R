#### confint #####
my <- function(data, scales) {

  model <- lm(y ~ x + as.factor(indicator_as_numeric), data = data)

  data.frame(x = data$x,
             y = model$fitted.values,
             group1 = data$indicator_as_numeric)
}

# For most applications the grouping is set implicitly by mapping one
# or more discrete variables to x, y, colour, fill, alpha, shape, size, and/or linetype
#'
#' StatOlsindicator <- ggplot2::ggproto("StatOlsindicator",
#'                                      ggplot2::Stat,
#'                                      setup_data = function(data, params){
#'                                        if(data$group[1] == -1){
#'                                          nrows <- nrow(data)
#'                                          data$group <- 1 #seq_len(nrows)
#'                                        }
#'
#'                                        data
#'                                      },
#'                                      compute_panel = my,
#'
#'                                      required_aes = c("x", "y", "indicator_as_numeric"),
#'                                      default_aes = aes(group = after_stat(group1))
#' )
#'
#'
#'
#' #' Drawing prediction interval for OLS linear model
#' #'
#' #' @param mapping
#' #' @param data
#' #' @param position
#' #' @param na.rm
#' #' @param show.legend
#' #' @param inherit.aes
#' #' @param ...
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' #' library(ggplot2)
#' #' library(dplyr)
#' #' library(magrittr)
#' #'
#' #' starwars %>%
#' #'    filter(mass < 1000) %>%
#' #'    tidyr::drop_na(mass, height, sex) ->
#' #'  my_data
#' #'
#' #'  my_data %>%
#' #'    lm(mass ~ height + sex, data = .) ->
#' #'  model
#' #'
#' #' data.frame(x = my_data$height,
#' #' y = model$fitted.values,
#' #' group = my_data$sex) %>%
#' #' ggplot() +
#' #' aes(x = x, y = y, group = group) +
#' #' geom_point() +
#' #' geom_line()
#' #'
#' #' starwars %>%
#' #' filter(mass < 1000) %>%
#' #' mutate(sex_numeric = sex %>% as.factor() %>% as.numeric())  %>%
#' #' ggplot() +
#' #' aes(x = height, y = mass, indicator_as_numeric = sex_numeric) +
#' #' geom_point(aes(color = sex)) + geom_lm_indicator(linetype = "dashed")
#' geom_lm_indicator <- function(mapping = NULL, data = NULL,
#'                               position = "identity", na.rm = FALSE, show.legend = NA,
#'                               inherit.aes = T, ...) {
#'   ggplot2::layer(
#'     stat = StatOlsindicator, geom = ggplot2::GeomLine, data = data, mapping = mapping,
#'     position = position, show.legend = show.legend, inherit.aes = inherit.aes,
#'     params = list(na.rm = na.rm, ...)
#'   )
#' }
#'
#' starwars %>%
#' filter(mass < 1000) %>%
#' mutate(sex_numeric = sex %>% as.factor() %>% as.numeric())  %>%
#' ggplot() +
#' aes(x = height, y = mass, indicator_as_numeric = sex) +
#' geom_point(aes(color = sex)) +
#'   geom_lm_indicator(linetype = "dashed")
