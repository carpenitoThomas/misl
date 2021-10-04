#' Lists all learners available
#'
#' Lists all learners available for ensemble learning. This is an aliased function from SL3::sl3_list_learners
#'
#' @param datatype A character vector of one of the following: "continuous", "binomial", "categorical"
#'
#' @return A list of learners compatible with the specified datatype
#' @export
#'
#' @examples
#' list_learners("continuous")
#'
list_learners <- function(datatype){

  sl3::sl3_list_learners(datatype)
}
