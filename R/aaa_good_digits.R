### text formula

good_digits <- function(x, digits = 3){

  formatC(signif(x, digits = digits),
          digits = digits,
          format="fg",
          flag = "#") %>%
    stringr::str_replace("\\.$", "")

}
