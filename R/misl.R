#' Imputes missing values using multiple imputation by super learning
#'
#' @param dataset A dataframe or matrix containing the incomplete data. Missing values are represented with \code{NA}.
#' @param m The number of multiply imputed datasets to create. The default is \code{m=5}.
#' @param maxit The number of iterations for each of the \code{m} imputed datasets. The default is \code{maxit=5}.
#' @param seed Specify whether or not to include a seed for reproducible research. The default is \code{seed = NA}.
#' @param con_method A vector of strings to be supplied for building the super learner for columns containing continuous data. The default learners are \code{con_method = c("Lrnr_mean", "Lrnr_glm")}.
#' @param bin_method A vector of strings to be supplied for building the super learner for columns containing binomial data. Important to note that these values must only take on values \code{c(0,1,NA)}. The default learners are \code{bin_method = c("Lrnr_mean", "Lrnr_glm")}.
#' @param cat_method A vector of strings to be supplied for building the super learner for columns containing categorical data. The default learners are \code{bin_method = c("Lrnr_mean", "Lrnr_glmnet")}.
#' @param missing_default A string defining how placeholder values should be imputed with the misl algorithm. Allows for one of the following: \code{c("mean", "median")}. The default is \code{missing_default = "mean"}.
#' @param quiet A boolean describing if progress of the misl algorithm should be printed to the console. The default is \code{quiet = TRUE}.
#'
#' @return A list of \code{m} full tibbles.
#' @export
#'
#' @examples
#' # This will generate imputations for the built-in nhanes dataset.
#' misl_imp <- misl(nhanes, m = 1)
#'
misl <- function(dataset,
                 m = 5,
                 maxit = 5,
                 seed = NA,
                 con_method = c("Lrnr_mean", "Lrnr_glm"),
                 bin_method = c("Lrnr_mean", "Lrnr_glm"),
                 cat_method = c("Lrnr_mean"),
                 missing_default = "mean",
                 quiet = TRUE
                 ){

  # TODO: checks that we can actually begin the MISL algorithm.
  check_dataset(dataset)

  # Initialize the return object (or, the dataframes that we want to return)
  imputed_datasets <- vector("list", m)

  # This apply function defines each of the imputed m datasets
  imputed_datasets <- future.apply::future_lapply(future.stdout = NA, seq_along(1:m), function(m_loop){

    # Do users want to know which dataset they are imputing?
    if(!quiet){print(paste("Imputing dataset:", m_loop))}

    # Initializes the trace plot (for inspection of imputations)
    trace_plot <- expand.grid(statistic = c("mean", "sd"), value = NA, variable = colnames(dataset), m = m_loop, iteration = seq_along(1:maxit))

    # Identify which order the columns should be imputed.
    # The order here specifies least missing data to most though the order should not be important (per MICE).

    missing_columns <- colnames(dataset)[colSums(is.na(dataset))!=0]
    column_order <- colnames(dataset)[order(colSums(is.na(dataset)))]
    column_order <- column_order[column_order %in% missing_columns]

    # Retain a copy of the dataset for each of the new m datasets
    dataset_master_copy <- dataset

    # Next, we begin the iterations within each dataset.
    for(i_loop in seq_along(1:maxit)){

      if(!quiet){print(paste("Imputing iteration:", i_loop))}

      # Begin the iteration column by column
      for(column in column_order){

        if(!quiet){print(paste("Imputing:", column))}
        # First, we extract all complete records with respect to the column we are imputing
        # Note, with the second iteration we should be using *all* rows of our dataframe (since the missing values were imputed on the first iteration)
        full_dataframe <- dataset_master_copy[!is.na(dataset_master_copy[[column]]), ]

        # To incorporate uncertainty in the imputations we will be using bootstrap sampling
        bootstrap_sample <- sample_n(full_dataframe, size = nrow(full_dataframe), replace = TRUE)

        # Next identify the predictors (xvars) and outcome (yvar) depending on the column imputing
        xvars <- colnames(bootstrap_sample[ , -which(names(bootstrap_sample) %in% c(column)), drop = FALSE])
        yvar <- column

        # We need to keep track of which values from the original dataframe are missing
        # This is important becuase after the first iteration NONE of the values will be classified as missing
        # Since MISL will have imputed them. When we iterate we only want to change these values per column.
        #missing_yvar <- is.na(dataset[[column]])

        # For the first iteration, any missing values will need to be set to either the mean or mode of the column.
        # This will allow us to train the super learner on the bootstrap sample.
        # This will also serve as a "catch" if the algorithm chooses not to impute values for this column as well upon successive iterations.
        # Note, we include the "yvar" in this iteration though nothing should be imputed for this column (since we subsetted with respect to it being full)
        # It would be easy to define this column type as a variable.
        # Also note, the SL3 algorithm with impute missing values with either the median or mode (depending on type) so that's something to consider.
        #for(column_number in seq_along(full_dataframe)){
        #  full_dataframe[is.na(full_dataframe[[column_number]]), column_number] <-  impute_placeholders(full_dataframe, column_number, missing_default)
        #}
        for(column_number in seq_along(bootstrap_sample)){
          bootstrap_sample[is.na(bootstrap_sample[[column_number]]), column_number] <-  impute_placeholders(bootstrap_sample, column_number, missing_default)
        }

        # We can begin misl.

        # Specifying the outcome_type will be helpful for checking learners.
        outcome_type <- check_datatype(dataset[[yvar]])

        # First, define the task
        task <- sl3::make_sl3_Task(bootstrap_sample, covariates = xvars, outcome = yvar)

        # Depending on the outcome, we need to build out the learners
        learners <- switch(outcome_type,
               categorical = cat_method,
               binary = bin_method ,
               continuous = con_method)

        # Next, iterate through each of the supplied learners to build the SL3 learner list
        learner_list <- c()
        for(learner in learners){
          code.lm <- paste(learner, " <- sl3::", learner, "$new()", sep="")
          eval(parse(text=code.lm))
          learner_list <- c(learner, learner_list)
        }

        # Next we stack the learners
        learner_stack_code <- paste("stack", " <- sl3::make_learner(sl3::Stack,",paste(learner_list, collapse = ", "), ")", sep="")
        eval(parse(text=learner_stack_code))

        # Then we make and train the Super Learner
        # This was a bottleneck for past simulations and we are introducing multisession parellelization
        sl <- sl3::Lrnr_sl$new(learners = stack)

        # Technically I should be able to just include the delayed code and the plan should default to sequential?
        test <- sl3::delayed_learner_train(sl, task)

        sched <- delayed::Scheduler$new(test, delayed::FutureJob, verbose = FALSE)
        stack_fit <- sched$compute()

        ####### CAN I KEEP JUST THE FOLLOWING CODE AND REMOVE THE IF/ELSE CONDITIONAL?
        # And finally obtain predictions from the stack on the updated dataset
        # Note, this step is important becuase we want to make predictions on those rows that have the missing outcome... but we might also have missing covariate data, too!
        if(i_loop == 1){
          dataset_copy <- dataset_master_copy
          for(column_number in seq_along(dataset_copy)){
            # This is a check to see if the column is a factor, requiring mode imputation
            # This means that the column should be registered as a factor.
            column_type <- check_datatype(dataset[[column_number]])
            if(column_type == "categorical"){
              dataset_copy[is.na(dataset_copy[[column_number]]), column_number] <-  impute_mode(dataset_copy[[column_number]])
            }else{
              # Major assumption, if the column is binary then it must ONLY have the values 0,1 (not 1,2 - for example)
              # This function is incomplete in its checks...
              if(column_type == "binary"){
                dataset_copy[is.na(dataset_copy[[column_number]]), column_number] <-  impute_mode(dataset_copy[[column_number]])
              }else{
                # Here, we assume a continuous variable and can use simple mean or median imputation
                dataset_copy[is.na(dataset_copy[[column_number]]), column_number] <-  get(missing_default)(dataset_copy[[column_number]], na.rm = TRUE)
              }
            }
          }
        }else{
          dataset_copy <- full_dataframe
        }

        new_prediction_task <- sl3::sl3_Task$new(dataset_copy, covariates = xvars, outcome = yvar)
        predictions <- stack_fit$predict(new_prediction_task)

        # Once we have the predictions we can replace the missing values from the original dataframe
        # Originally, was thinking this would be a good place to add a bit of noise. The old implementation was stochastic and is complicated (based on how much variance do we add... etc?)
        # There may not be justification for the continuous outcome scenario - why are we adding random noise to the imputations?
        # If confidence intervals are too small, we can use a matching technique that is at least theoretically backed.
        if(outcome_type == "binary"){
          predicted_values <- stats::rbinom(length(dataset_master_copy[[column]]), 1, predictions)
          dataset_master_copy[[column]] <- ifelse(is.na(dataset[[column]]), predicted_values, dataset[[column]])
        }else if(outcome_type == "continuous"){
          dataset_master_copy[[column]]<- ifelse(is.na(dataset[[column]]), predictions + stats::rnorm(n = length(predictions), mean = 0, sd = stats::sd(predictions) ), dataset[[column]])
        }else if(outcome_type== "categorical"){
          # This is a built in protector because the current MISL package does not update predictions properly for mean
          if(cat_method == "Lrnr_mean" & length(cat_method == 1)){
            predicted_values <- as.character(impute_mode(dataset[[column]]))
          }else{
            predicted_values <- Hmisc::rMultinom(sl3::unpack_predictions(predictions),1)
          }
          dataset_master_copy[[column]] <-  factor(ifelse(is.na(dataset[[column]]), predicted_values, as.character(dataset[[column]])), levels = levels(dataset[[column]]))
        }
        # Append to the trace plot only if a numeric column
        if(outcome_type != "categorical" & sum(is.na(dataset[[column]])) > 0){
          trace_plot$value[trace_plot$variable == column & trace_plot$m == m_loop & trace_plot$iteration == i_loop & trace_plot$statistic == "mean"] <- mean(dataset_master_copy[[column]][is.na(dataset[[column]])])
          trace_plot$value[trace_plot$variable == column & trace_plot$m == m_loop & trace_plot$iteration == i_loop & trace_plot$statistic == "sd"] <- sd(dataset_master_copy[[column]][is.na(dataset[[column]])])
        }

      }
    }

    # After all columns are imputed, we can save the dataset and trace plot for recall later
    return_object <- list(datasets = dataset_master_copy, trace = trace_plot)
    return_object
  })
  return(imputed_datasets)
}
