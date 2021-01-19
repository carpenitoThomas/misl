#' Returns a complete tibble from the multiple imputed datasets
#'
#' @param misl_imputations A list of misl imputed data objects
#' @param number Returns the \code{number} complete dataset.
#'
#' @return A tibble of the complete dataset
#' @export
#'
#' @examples
#'
#' # This will generate imputations for the built-in nhanes dataset.
#' misl_imp <- misl(nhanes)
#'
#' # You can then return the desired dataset
#' complete_dataset <- complete(misl_imp,1)
#'
complete <- function(misl_imputations, number){
  return(misl_imputations[[number]])
}
