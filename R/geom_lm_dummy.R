#### confint #####


# For most applications the grouping is set implicitly by mapping one
# or more discrete variables to x, y, colour, fill, alpha, shape, size, and/or linetype

StatOlsindicator1 <- ggplot2::ggproto("StatOlsindicator1",
                                   ggplot2::Stat,
                                   compute_layer = function(data, scales) {

                                     model <- lm(y ~ x, # + indicator,
                                                 data = data)

                                     data.frame(x = data$x,
                                                y = model$fitted.values#,
                                                # group = data$indicator
                                                )
                                   },

                                   required_aes = c("x", "y"
                                                    # , "indicator"
                                                    )
)


#' Drawing prediction interval for OLS linear model
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
#' library(dplyr)
#' library(magrittr)
#'
#' starwars %>%
#'    filter(mass < 1000) %>%
#'    tidyr::drop_na(mass, height, sex) ->
#'  my_data
#'
#'  my_data %>%
#'    lm(mass ~ height + sex, data = .) ->
#'  model
#'
#' data.frame(x = my_data$height,
#' y = my_data$mass,
#' yfit = model$fitted.values,
#' group = my_data$sex) %>%
#' ggplot() +
#' aes(x = x, y = y, group = group) +
#' geom_point(aes(color = group, alpha = group == "none")) +
#' geom_line(aes(y = yfit))
#'
#' starwars %>%
#' filter(mass < 1000) %>%
#' mutate(sex_numeric = sex)  %>%
#' ggplot() +
#' aes(x = height, y = mass) +
#' #geom_point(aes(color = sex)) +
#'  geom_lm_indicator1(linetype = "dashed")
geom_lm_indicator1 <- function(mapping = NULL, data = NULL,
                             position = "identity", na.rm = FALSE, show.legend = NA,
                             inherit.aes = T, ...) {
  ggplot2::layer(
    stat = StatOlsindicator1, geom = ggplot2::GeomLine, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}



