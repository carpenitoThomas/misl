#' Returns the mode (most occuring value). Note, this function will only return a single mode.
#'
#' @param x list
#'
#' @return num
#' @export
#'
#' @examples
#' impute_mode(c(1,1,2,3))
impute_mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
