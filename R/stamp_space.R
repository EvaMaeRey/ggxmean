#' Title
#'
#' @return
#'
#' @examples
stamp_space <- function(){

  ggplot2::ggplot(data.frame(x = 0, y = 0), ggplot2::aes(x = x)) + ggplot2::labs(x = NULL)

}
