### text formula

good_digits <- function(x){

  formatC(signif(x, digits = 3),
          digits = 3,
          format="fg",
          flag = "#") %>%
    stringr::str_replace("\\.$", "")


}
