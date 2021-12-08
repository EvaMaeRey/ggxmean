### text formula

#' Title
#'
#' @param x
#' @param digits
#'
#' @return
#' @export
#'
#' @examples
#' good_digits(x = .8999, digits = 2)
good_digits <- function(x, digits = 3){

  formatC(signif(x, digits = digits),
          digits = digits,
          format="fg",
          flag = "#") %>%
    stringr::str_replace("\\.$", "")

}
