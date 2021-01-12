#' Imputes missing values using multiple imputation by super learning
#'
#' @param dataset
#' @param m
#' @param maxit
#' @param seed
#' @param con_method
#' @param bin_method
#' @param cat_method
#' @param missing_default
#' @param quiet
#'
#' @return
#' @export
#'
#' @examples
misl <- function(dataset,
                 m = 5,
                 maxit = 5,
                 seed = NA,
                 con_method = c("Lrnr_mean", "Lrnr_glm"),
                 bin_method = c("Lrnr_mean", "Lrnr_glmnet"),
                 cat_method = c("Lrnr_mean", "Lrnr_glmnet"),
                 missing_default = "mean",
                 quiet = TRUE
                 ){

  # TO ADD: checks that we can actually begin the MISL algorithm.

  # Initialize the return object (or, the dataframes that we want to return)
  imputed_datasets <- vector("list", m)

  # This loop defines each of the imputed m datasets.
  for(m_loop in seq_along(1:m)){

    # Do users want to know which dataset they are imputing?
    if(!quiet){print(paste("Imputing dataset:", m_loop))}

    # Identify which order the columns should be imputed.
    # The order here specifies most missing data to least though the order should not be important (per MICE).
    column_order <- colnames(dataset)[order(colSums(is.na(dataset)))]

    # Retain a copy of the dataset for each of the new m datasets
    dataset_master_copy <- dataset

    # Next, we begin the iterations within each dataset.
    for(i in seq_along(1:maxit)){

      # Do users want to know which iteration they are imputing?
      if(!quiet){print(paste("Imputing iteration:", i))}

      # Begin the iteration column by column
      for(column in column_order){

        # Print what column we are starting with
        if(!quiet){print(paste("Imputing:", column))}

        # First, we extract all complete records with respect to the column we are imputing
        # Note, with the second iteration we should be using *all* rows of our dataframe (since the missing values were imputed on the first iteration)
        if(i == 1){
          # For the first iteration, we're using only those rows for which data exists for the variable
          full_dataframe <- dataset[!is.na(dataset[[column]]), ]
        }else{
          # After the first iteration, we can use the newly imputed dataset (which should be full)
          full_dataframe <- new_imputed_dataset
        }

        # Next identify the predictors (x vars) and outcome (y var) depending on the column imputing
        yvar <- column
        xvars <- colnames(full_dataframe[ , -which(names(full_dataframe) %in% c(column)), drop = FALSE])

        # We need to keep track of which values from the original dataframe are missing
        # This is important becuase after the first iteration NONE of the values will be classified as missing
        # Since MISL will have imputed them. When we iterate we only want to change these values per column.
        missing_yvar <- is.na(dataset[[column]])

        # For the first iteration, any missing values will need to be set to either the mean or mode of the column.
        # This will also serve as a "catch" if the algorithm chooses not to impute values for this column as well upon successive iterations.
        # Note, we include the "yvar" in this iteration though nothing should be imputed for this column (since we subsetted with respect to it being full)
        # It would be easy to define this column type as a variable.
        column_type <- NULL
        for(column_number in seq_along(full_dataframe)){
          # This is a check to see if the column is a factor, requiring mode imputation
          # This means that the column should be registered as a factor.
          if(class(full_dataframe[[column_number]]) == "factor"){
            full_dataframe[is.na(full_dataframe[,column_number]), column_number] <-  impute_mode(full_dataframe[,column_number])
            column_type <- "categorical"
          }else{
            # We assume now that we have some continuous variable... BUT this variable could be binary or continuous
            # Major assumption, if the column is binary then it must ONLY have the values 0,1 (not 1,2 - for example)
            # This function is incomplete in its checks...
            if(length(levels(as.factor(full_dataframe[,column_number]))) == 2){
              full_dataframe[is.na(full_dataframe[,column_number]), column_number] <-  impute_mode(full_dataframe[,column_number])
              column_type <- "binary"
            }else{
              # Here, we assume a continuous variable and can use simple mean or median imputation
              full_dataframe[is.na(full_dataframe[,column_number]), column_number] <-  get(missing_default)(full_dataframe[,column_number], na.rm = TRUE)
              column_type <- "continuous"
            }
          }
        }

        # We should now have a complete dataframe and can being misl.

        # First, define the task
        task <- sl3::make_sl3_Task(full_dataframe, covariates = xvars, outcome = yvar)

        # TODO: Add Screeners?

        # Depending on the outcome, we need to build out the learner
        learners <- switch(column_type,
               categorical = cat_method,
               binary = bin_method ,
               continuous = con_method)

        # Next, iterate through each of the supplied learners to build the SL3 learner list
        learner_list <- c()
        for(learner in learners){
          code.lm <- paste(learner, " <- ", learner, "$new()", sep="")
          eval(parse(text=code.lm))
          learner_list <- c(learner, learner_list)
        }

        # Next we stack the learners
        learner_stack_code <- paste("stack", " <- make_learner(Stack,",paste(learner_list, collapse = ", "), ")", sep="")
        eval(parse(text=learner_stack_code))

        # Then we make and train the Super Learner
        sl <- Lrnr_sl$new(learners = stack)
        stack_fit <- sl$train(task)

        # And finally obtain predictions from the stack on the full dataframe
        if(i == 1){
          dataset_copy <- dataset_master_copy
          for(column_number in seq_along(dataset_copy)){
            # This is a check to see if the column is a factor, requiring mode imputation
            # This means that the column should be registered as a factor.
            if(class(dataset_copy[[column_number]]) == "factor"){
              dataset_copy[is.na(dataset_copy[,column_number]), column_number] <-  impute_mode(dataset_copy[,column_number])
              column_type <- "categorical"
            }else{
              # We assume now that we have some continuous variable... BUT this variable could be binary or continuous
              # Major assumption, if the column is binary then it must ONLY have the values 0,1 (not 1,2 - for example)
              # This function is incomplete in its checks...
              if(length(levels(as.factor(dataset_copy[,column_number]))) == 2){
                dataset_copy[is.na(dataset_copy[,column_number]), column_number] <-  impute_mode(dataset_copy[,column_number])
                column_type <- "binary"
              }else{
                # Here, we assume a continuous variable and can use simple mean or median imputation
                dataset_copy[is.na(dataset_copy[,column_number]), column_number] <-  get(missing_default)(dataset_copy[,column_number], na.rm = TRUE)
                column_type <- "continuous"
              }
            }
          }
        }else{
          dataset_copy <- full_dataframe
        }


        new_prediction_task <- sl3::sl3_Task$new(dataset_copy, covariates = xvars, outcome = yvar)
        predictions <- stack_fit$predict(new_prediction_task)

        # Once we have the predictions we can replace the missing values from the original dataframe
        # Note, we add a bit of random noise here
        if(column_type == "binary"){
          predicted_values <- rbinom(length(dataset_master_copy[[column]]), 1, predictions)
          dataset_master_copy[[column]] <- ifelse(missing_yvar, predicted_values, dataset[[column]])

        }else if(column_type == "continuous"){
          dataset_master_copy[[column]]<- ifelse(missing_yvar, predictions + rnorm(n = length(predictions), mean = 0, sd = sd(predictions) ), dataset[[column]])

        }else{
          # In this instance, the column type is categorical
          # This is depedent on what the super learner returns (predictions or predicted probabilities)
          dataset_master_copy[[column]]<- ifelse(missing_yvar, predictions, dataset[[column]])
        }

        # We can set this column back in the dataframe and move on to the next.
        new_imputed_dataset <- dataset_master_copy

        # Lastly, we should remove the learners for the next column (should there be overlap)
        for(learner in learner_list){
          code.lm <- paste("rm(", learner, ")", sep="")
          eval(parse(text=code.lm))
        }


      }


    }
    # After all columns are imputed, we can save the dataset for recall later
    imputed_datasets[[m_loop]] <- new_imputed_dataset
  }
  return(imputed_datasets)
}
