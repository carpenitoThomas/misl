#' Imputes placeholders in the current dataset
#'
#' @param dataset The dataset to which placeholders are filled.
#' @param column_number The specific column within the dataset that is to be filled.
#' @param missing_default A string defining how placeholder values should be imputed with the misl algorithm. Allows for one of the following: \code{c("mean", "median")}. The default is \code{missing_default = "mean"}.
#'
#' @return
#' @export
#'
#' @examples
impute_placeholders <- function(dataset, column_number, missing_default){
  # This is a check to see if the column is a factor, requiring mode imputation
  # This means that the column should be registered as a factor.
  if(TRUE){
    sample(dataset[[column_number]][!is.na(dataset[[column_number]])], 1)
  }else{
    if(class(dataset[[column_number]]) == "factor"){
      impute_mode(dataset[[column_number]])
    }else{
      # We assume now that we have some continuous variable... BUT this variable could be binary or continuous
      # Major assumption, if the column is binary then it must ONLY have the values 0,1 (not 1,2 - for example)
      # This function is incomplete in its checks...
      if(length(levels(as.factor(dataset[[column_number]]))) == 2){
        impute_mode(dataset[[column_number]])
      }else{
        # Here, we assume a continuous variable and can use simple mean or median imputation
        get(missing_default)(dataset[[column_number]], na.rm = TRUE)
      }
    }
  }
}
