#' Returns the mode (most occuring value)
#'
#' @param x list
#'
#' @return num
#' @export
#'
#' @examples
#' impute_mode(c(1,1,2,3))
impute_mode <- function(x) {
  ux <- stats::na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
