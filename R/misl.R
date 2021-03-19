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
                 con_method = c("Lrnr_mean", "Lrnr_glm_fast"),
                 bin_method = c("Lrnr_mean", "Lrnr_glm_fast"),
                 cat_method = c("Lrnr_mean"),
                 missing_default = "sample",
                 quiet = TRUE
                 ){

  # TODO: checks that we can actually begin the MISL algorithm.
  check_dataset(dataset)

  # Initialize the return object (or, the dataframes that we want to return)
  imputed_datasets <- vector("list", m)

  # This apply function defines each of the imputed m datasets
  imputed_datasets <- future.apply::future_lapply(future.stdout = NA, future.seed=TRUE, seq_along(1:m), function(m_loop){

    # Do users want to know which dataset they are imputing?
    if(!quiet){print(paste("Imputing dataset:", m_loop))}

    # Initializes the trace plot (for inspection of imputations)
    trace_plot <- expand.grid(statistic = c("mean", "sd"), value = NA, variable = colnames(dataset), m = m_loop, iteration = seq_along(1:maxit))

    # Identifies which columns need to be imputed. According to van Buren, this order does not matter.
    # https://stefvanbuuren.name/fimd/sec-algoptions.html
    column_order <- colnames(dataset)[colSums(is.na(dataset))!=0]

    # Retain a copy of the dataset for each of the new m datasets
    dataset_master_copy <- dataset

    # As with all gibbs sampling methods, we will need to initialize the starting dataframe.
    # You can see in this intialize function: https://github.com/amices/mice/blob/46171f911af7c7c668b4bffc3976f5669436bafd/R/initialize.imp.R
    # This is step 2 of https://stefvanbuuren.name/fimd/sec-FCS.html#def:mice
    for(column_number in seq_along(dataset_master_copy)){
      dataset_master_copy[is.na(dataset_master_copy[[column_number]]), column_number] <-  impute_placeholders(dataset_master_copy, column_number, missing_default)
    }

    # Next, we begin the iterations within each dataset.
    for(i_loop in seq_along(1:maxit)){

      if(!quiet){print(paste("Imputing iteration:", i_loop))}

      # Begin the iteration column by column
      for(column in column_order){

        if(!quiet){print(paste("Imputing:", column))}
        # First, we extract all complete records with respect to the column we are imputing.
        # This is our y_dot_obs and x_dot_obs
        # https://stefvanbuuren.name/fimd/sec-linearnormal.html#def:normboot
        full_dataframe <- dataset_master_copy[!is.na(dataset[[column]]), ]

        # To avoid complications with variance estimates of the ensemble, we will use bootstrapping
        # See note below algorithm: https://stefvanbuuren.name/fimd/sec-pmm.html#def:pmm
        # We can also see the following: https://stefvanbuuren.name/fimd/sec-linearnormal.html#def:normboot
        bootstrap_sample <- dplyr::sample_n(full_dataframe, size = nrow(full_dataframe), replace = TRUE)

        # Next identify the predictors (xvars) and outcome (yvar) depending on the column imputing
        xvars <- colnames(bootstrap_sample[ , -which(names(bootstrap_sample) %in% c(column)), drop = FALSE])
        yvar <- column

        # We can begin defining our impuation model or, super learning
        # Information on other models can be found: https://stefvanbuuren.name/fimd/how-to-generate-multiple-imputations.html

        # Specifying the outcome_type will be helpful for checking learners.
        outcome_type <- check_datatype(dataset[[yvar]])

        # First, define the task using our bootstrap_sample (this helps with variability in imputations)
        sl3_task <- sl3::make_sl3_Task(bootstrap_sample, covariates = xvars, outcome = yvar, outcome_type = outcome_type )

        # Depending on the outcome, we need to build out the learners
        learners <- switch(outcome_type,
               categorical = cat_method,
               binomial = bin_method ,
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
        sl <- sl3::Lrnr_sl$new(learners = stack)

        # We can then go ahead and train our model
        sl_train <- sl3::delayed_learner_train(sl, sl3_task)
        # This bit of code can be used if people wanted multi-threading (depending on computer capacity)
        sl_sched <- delayed::Scheduler$new(sl_train, delayed::FutureJob, verbose = FALSE)
        sl_stack_fit <- sl_sched$compute()

        # We are now at the point where we can obtain predictions for matching candidates using X_miss

        # Here we can create the predictions and then we can match them with the hot-deck method
        # Interestingly, there are 4 different ways we can match: https://stefvanbuuren.name/fimd/sec-pmm.html#sec:pmmcomputation
        # But, we're going to follow the bootstrap matching method: https://stefvanbuuren.name/fimd/sec-cart.html#sec:cartoverview
        # Which is interesting becuase it looks like our beta hat and beta dot are one in the same: https://stefvanbuuren.name/fimd/sec-categorical.html
        predictions_task <- sl3::sl3_Task$new(dataset_master_copy, covariates = xvars, outcome = yvar, outcome_type = outcome_type )
        predictions <- sl_stack_fit$predict(predictions_task)

        # Here we can begin selection from a canidate donor
        # Note, this is unclear... becuase we are using a technique like CART but we don't have terminal nodes.
        # But also PMM didn't distinguish how one matches when using bootstrap?
        # So, we're not going to be matching with binary or categorical variables, we can just use sampling from their distribution.
        # https://stefvanbuuren.name/fimd/sec-categorical.html
        if(outcome_type == "binomial"){
          predicted_values <- stats::rbinom(length(dataset_master_copy[[column]]), 1, predictions)
          dataset_master_copy[[column]] <- ifelse(is.na(dataset[[column]]), predicted_values, dataset[[column]])
        }else if(outcome_type == "continuous"){
          # If continuous, we can do matching
          # Find the 5 closest donors and making a random draw from them
          list_of_matches <- c()
          for(value in seq_along(predictions)){
            distance <- head(order(abs(predictions[value] - ifelse(is.na(dataset[[column]]), NA, predictions))),5)
            list_of_matches[value] <- ifelse(is.na(dataset[[column]]), NA, dataset[[column]])[sample(distance,1)]
          }
          dataset_master_copy[[column]]<- ifelse(is.na(dataset[[column]]), list_of_matches, dataset[[column]])
        }else if(outcome_type== "categorical"){
          predicted_values <- Hmisc::rMultinom(sl3::unpack_predictions(predictions),1)
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
