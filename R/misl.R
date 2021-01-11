# This file is going to be where the main MISL code lies.
# I expect this file to change a lot over time

misl <- function(dataset,
                 m = 5,
                 maxit = 5,
                 seed = NA,
                 con_method = c("Lrnr_mean", "Lrnr_glmnet"),
                 bin_method = c("Lrnr_mean", "Lrnr_glmnet"),
                 cat_method = c("Lrnr_mean", "Lrnr_glmnet"),
                 quiet = TRUE
                 ){

  # TO ADD: checks that we can actually begin the MISL algorithm.

  # This loop defines each of the imputed m datasets.
  for(m in seq_along(1:m)){

    # Do users want to know which dataset they are imputing?
    ifelse(quiet, NULL, print(paste("Imputing dataset:", m)))

    # Identify which order the columns should be imputed.
    # The order here specifies most missing data to least though the order should not be important (per MICE).
    column_order <- colnames(dataset)[order(colSums(is.na(dataset)))]

    # Next, we begin the iterations within each dataset.
    for(i in seq_along(1:maxit)){

      # Do users want to know which iteration they are imputing?
      ifelse(quiet, NULL, print(paste("Imputing iteration:", i)))
    }



  }
}
