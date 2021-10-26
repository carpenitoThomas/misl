check_dataset <- function(dataset){
  if(sum(colSums(is.na(dataset))) == 0){
    stop("Your dataset is complete, no need for MISL!")
  }
}


check_datatype <- function(x){
  datatype <- class(x)
  if(class(x) == "factor"){
    return("categorical")
  }
  else{
    # We assume now that we have some continuous variable... BUT this variable could be binary or continuous
    # Major assumption, if the column is binary then it must ONLY have the values 0,1 (not 1,2 - for example)
    # This function is incomplete in its checks...
    if(sum(levels(as.factor(x)) == c("0", "1")) == 2){
      return("binomial")
    }else{
      # Here, we assume a continuous variable and can use simple mean or median imputation
      return("continuous")
    }
  }
}


