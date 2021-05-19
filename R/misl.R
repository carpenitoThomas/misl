#' Imputes missing values using multiple imputation by super learning
#'
#' @param dataset A dataframe or matrix containing the incomplete data. Missing values are represented with \code{NA}.
#' @param m The number of multiply imputed datasets to create. The default is \code{m=5}.
#' @param maxit The number of iterations for each of the \code{m} imputed datasets. The default is \code{maxit=5}.
#' @param seed Specify whether or not to include a seed for reproducible research. The default is \code{seed = NA}.
#' @param con_method A vector of strings to be supplied for building the super learner for columns containing continuous data. The default learners are \code{con_method = c("Lrnr_mean", "Lrnr_glm")}.
#' @param bin_method A vector of strings to be supplied for building the super learner for columns containing binomial data. Important to note that these values must only take on values \code{c(0,1,NA)}. The default learners are \code{bin_method = c("Lrnr_mean", "Lrnr_glm")}.
#' @param cat_method A vector of strings to be supplied for building the super learner for columns containing categorical data. The default learners are \code{bin_method = c("Lrnr_mean", "Lrnr_glmnet")}.
#' @param ignore_predictors A vector of strings to be supplied for ignoring in the prediction of other variables. The default is \code{ignore_predictors = NA}
#' @param quiet A boolean describing if progress of the misl algorithm should be printed to the console. The default is \code{quiet = TRUE}.
#' @param delta_con An integer to specify by how much continuous values should be shifted for the delta adjustmenet method of a sensitivity analysis. If the user does not specify a value, the imputations will not be augmented. The default is \code{delta_con = 0}.
#' @param delta_cat An integer to specify by how much binary/categorical values should be scaled for the delta adjustmenet method of a sensitivity analysis. If the user does not specify a value, the imputations will not be augmented. The default is \code{delta_cat = 1}.
#' @param delta_var A character to specify which variable (if any) to be augmented with the sensitivity analysis. The default is \code{delta_var = NA}.
#'
#' @return A list of \code{m} full tibbles.
#' @export
#'
#' @examples
#' # This will generate imputations for the built-in abalone dataset.
#' misl_imp <- misl(abalone,
#' m = 1,
#' abalone, maxit = 5, m = 5, quiet = TRUE,
#' con_method = c("Lrnr_glm_fast", "Lrnr_earth", "Lrnr_ranger"),
#' bin_method = c("Lrnr_earth", "Lrnr_glm_fast", "Lrnr_ranger"),
#' cat_method = c("Lrnr_independent_binomial", "Lrnr_ranger"))
#'
misl <- function(dataset,
                 m = 5,
                 maxit = 5,
                 seed = NA,
                 con_method = c("Lrnr_mean", "Lrnr_glm_fast"),
                 bin_method = c("Lrnr_mean", "Lrnr_glm_fast"),
                 cat_method = c("Lrnr_mean"),
                 ignore_predictors = NA,
                 quiet = TRUE,
                 delta_con = 0,
                 delta_cat = 1,
                 delta_var = NA
                 ){

  # TODO: Builds out more checks to ensure the MISL algorithm can run properly
  check_dataset(dataset)

  # Initialize the return object (or, the dataframes that we want to return)
  imputed_datasets <- vector("list", m)

  # This apply function defines each of the imputed m datasets
  # The future.seed = TRUE argument ensures parallel safe random numbers are generated. See the Future package for more information https://cran.r-project.org/web/packages/future.apply/future.apply.pdf
  imputed_datasets <- future.apply::future_lapply(future.stdout = NA, future.seed=TRUE, seq_along(1:m), function(m_loop){

    # Do users want to know which dataset they are imputing?
    if(!quiet){print(paste("Imputing dataset:", m_loop))}

    # Initializes the trace plot (for inspection of imputations)
    trace_plot <- expand.grid(statistic = c("mean", "sd"), value = NA, variable = colnames(dataset), m = m_loop, iteration = seq_along(1:maxit))

    # Identifies which columns need to be imputed. According to van Buren, this order does not matter
    # https://stefvanbuuren.name/fimd/sec-algoptions.html
    # Future work should explore if this makes a difference
    column_order <- sample(colnames(dataset)[colSums(is.na(dataset))!=0])

    # Retain a copy of the dataset for each of the new m datasets
    dataset_master_copy <- dataset

    # As with all gibbs sampling methods, we will need to initialize the starting dataframe
    # This is step 2 of https://stefvanbuuren.name/fimd/sec-FCS.html#def:mice
    for(column_number in seq_along(dataset_master_copy)){
      dataset_master_copy[is.na(dataset_master_copy[[column_number]]), column_number] <-  sample(dataset[[column_number]][!is.na(dataset[[column_number]])], sum(is.na(dataset[[column_number]])), replace = TRUE)
    }

    # Next, we begin the iterations within each dataset.
    for(i_loop in seq_along(1:maxit)){

      # Provide an option for if messages should be output
      if(!quiet){print(paste("Imputing iteration:", i_loop))}

      # Begin the iteration column by column
      for(column in column_order){

        if(!quiet){print(paste("Imputing:", column))}
        # First, we extract all complete records with respect to the column we are imputing
        # This is our y_dot_obs and x_dot_obs
        # https://stefvanbuuren.name/fimd/sec-linearnormal.html#def:normboot
        full_dataframe <- dataset_master_copy[!is.na(dataset[[column]]), ]

        # This is a quick fix for the sensitivity analysis that will require further thought
        # Essentially, we are chceking to see if this is the value we need to augment or not.
        # If it's not, then no changes happen
        delta_adj = FALSE
        if(!is.na(delta_var)){
          if(column == delta_var){
            delta_adj = TRUE
          }
        }

        # To avoid complications with variance estimates of the ensemble, we will use bootstrapping
        # See note below algorithm: https://stefvanbuuren.name/fimd/sec-pmm.html#def:pmm
        # We can also see the following: https://stefvanbuuren.name/fimd/sec-linearnormal.html#def:normboot
        # Note, for this method we still need to calculate beta_hat and beta_dot, unfortunately this means super learner twice
        bootstrap_sample <- dplyr::sample_n(full_dataframe, size = nrow(full_dataframe), replace = TRUE)

        # Next identify the predictors (xvars) and outcome (yvar) depending on the column imputing
        xvars <- colnames(bootstrap_sample[ , -which(names(bootstrap_sample) %in% c(column)), drop = FALSE])
        if(!is.na(ignore_predictors[1])){
          xvars <- xvars[-which(xvars %in% ignore_predictors)]
        }
        yvar <- column

        # We can begin defining our impuation model or, super learning
        # More information on super learner can be found in the SL3 Package: https://github.com/tlverse/sl3
        # Information on other models can be found: https://stefvanbuuren.name/fimd/how-to-generate-multiple-imputations.html

        # Specifying the outcome_type will be helpful for checking learners
        outcome_type <- check_datatype(dataset[[yvar]])

        # First, define the task using our bootstrap_sample (this helps with variability in imputations) and our full_dataframe sample
        sl3_task_boot_dot <- sl3::make_sl3_Task(bootstrap_sample, covariates = xvars, outcome = yvar, outcome_type = outcome_type )
        sl3_task_full_hat <- sl3::make_sl3_Task(full_dataframe, covariates = xvars, outcome = yvar, outcome_type = outcome_type )

        # Depending on the outcome, we need to build out the learners
        learners <- switch(outcome_type,
               categorical = cat_method,
               binomial = bin_method ,
               continuous = con_method)

        # If after drawing a bootstrap sample, any of the columns DO NOT contain the same factors as in the original data, then the algorithm will fail
        # This is set up by design becuase the super learner cannot make out of sample predictions and the meta-learner will not know how
        # to deal with the different factor levels. Should this happen, we will print a message to the user letting them know that the machine learning algorithms could NOT be used
        # in this instance and instead for this iteration they must rely on the mean and a series of independent binomial samples. This will be updated should more learners become available.
        if(outcome_type == "categorical"){
          re_assign_cat_learners <- FALSE
          for(column_number in seq_along(bootstrap_sample)){
            if(is.factor(bootstrap_sample[[column_number]])){
              if(length(levels(droplevels(bootstrap_sample)[[column_number]])) != length(levels(bootstrap_sample[[column_number]]))){
                re_assign_cat_learners <- TRUE
              }
            }
          }
          if(re_assign_cat_learners){
            warning("Factor levels are not compatible between bootstrap and original dataframes. This occurs as a product of bootstrap sampling. Lrnr_mean and Lrnr_independent_binomial have been subsituted for this iteration.")
            learners <- c("Lrnr_mean", "Lrnr_independent_binomial")
          }
        }

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

        # We can then go ahead and train our model on the bootstrap data
        sl_train_boot_dot <- sl3::delayed_learner_train(sl, sl3_task_boot_dot)

        # We can finally execute the super learner
        # This bit of code can be used if people wanted multi-threading (depending on computer capacity)
        sl_sched_boot_dot <- delayed::Scheduler$new(sl_train_boot_dot, delayed::FutureJob)
        sl_stack_fit_boot_dot <- sl_sched_boot_dot$compute()

        # We are now at the point where we can obtain predictions for matching candidates using X_miss

        # Here we can create the predictions and then we can match them with the hot-deck method
        # Interestingly, there are 4 different ways we can match: https://stefvanbuuren.name/fimd/sec-pmm.html#sec:pmmcomputation
        # Original PMM uses type 1 matching, so that's what we are going to use

        predictions_task_boot_dot <- sl3::sl3_Task$new(dataset_master_copy, covariates = xvars, outcome = yvar, outcome_type = outcome_type )

        predictions_boot_dot <- sl_stack_fit_boot_dot$predict(predictions_task_boot_dot)


        # This step only needs to compute if the outcome is continuous, saving some time
        if(outcome_type == "continuous"){
          sl_train_full_hat <- sl3::delayed_learner_train(sl, sl3_task_full_hat)
          sl_sched_full_hat <- delayed::Scheduler$new(sl_train_full_hat, delayed::FutureJob)

          sl_stack_fit_full_hat <- sl_sched_full_hat$compute()

          predictions_task_full_hat <- sl3::sl3_Task$new(dataset_master_copy, covariates = xvars, outcome = yvar, outcome_type = outcome_type )
          predictions_full_hat <- sl_stack_fit_full_hat$predict(predictions_task_full_hat)
        }

        # Here we can begin imputation depending on the data type
        if(outcome_type == "binomial"){
          # Imputation for binary variables can be found from the following resources:
          # https://stefvanbuuren.name/fimd/sec-categorical.html#def:binary
          # https://github.com/cran/mice/blob/master/R/mice.impute.logreg.R

          uniform_values <- runif(length(predictions_boot_dot))

          # Here we add a bit of code for sensitivity analyses
          # Delta_cat will default be 1 so this won't change predictions
          if(delta_adj){
            predicted_values <- as.integer(uniform_values <= (predictions_boot_dot / delta_cat))
          }else{
            predicted_values <- as.integer(uniform_values <= (predictions_boot_dot / 1))
          }

          dataset_master_copy[[column]] <- ifelse(is.na(dataset[[column]]), predicted_values, dataset[[column]])
        }else if(outcome_type == "continuous"){
          # We can add a bit of augemntation here for the sensitivity analysis
          # By default, this should not affect results as we will be adding 0, otherwise, augment the imputations
          # I actually think in this instance we want to shift the *actualy* imputed values, not the predictions. Double check.
          predictions_boot_dot <- predictions_boot_dot + delta_con

          # If continuous, we can do matching
          # Find the 5 closest donors and making a random draw from them - there are a lot of ways to do matching
          # https://stefvanbuuren.name/fimd/sec-pmm.html#sec:pmmcomputation
          # Note, there are considerable slow-downs with our matching here and should be improved for efficiency
          list_of_matches <- c()
          for(value in seq_along(predictions_boot_dot)){
            distance <- head(order(abs(predictions_boot_dot[value] - ifelse(is.na(dataset[[column]]), NA, predictions_full_hat))),5)
            list_of_matches[value] <- ifelse(is.na(dataset[[column]]), NA, dataset[[column]])[sample(distance,1)]
          }
          dataset_master_copy[[column]]<- ifelse(is.na(dataset[[column]]), list_of_matches, dataset[[column]])
        }else if(outcome_type== "categorical"){
          # For categorical data we follow advice suggested by Van Buuren:
          # https://github.com/cran/mice/blob/master/R/mice.impute.polyreg.R
          uniform_values <- rep(runif(length(predictions_boot_dot)), each = length(levels(dataset[[column]])))
          post <- sl3::unpack_predictions(predictions_boot_dot)
          # We need to add a bit of code in here that allows for the delta adjustment method for sensitivity analyses
          # The idea will be to choose a random column, scale it by the delta amount, and re-normalize to 1
          # Since the default scale parameter is 1, this shouldn't change predictions
          if(delta_adj){
            sampled_delta_col <- sample(ncol(post),1)
            post[, sampled_delta_col] <- post[, sampled_delta_col] / delta_cat
            post <- t(scale(t(post), center = FALSE, scale = colSums(t(post))))
          }else{
            sampled_delta_col <- sample(ncol(post),1)
            post[, sampled_delta_col] <- post[, sampled_delta_col] / 1
            post <- t(scale(t(post), center = FALSE, scale = colSums(t(post))))
          }

          # We can then continue with imputation as normal
          draws <- uniform_values > apply(post, 1, cumsum)
          idx <- 1 + apply(draws, 2, sum)
          predicted_values <- levels(dataset[[column]])[idx]

          dataset_master_copy[[column]] <-  factor(ifelse(is.na(dataset[[column]]), predicted_values, as.character(dataset[[column]])), levels = levels(dataset[[column]]))
        }
        # Append to the trace plot only if a numeric column
        # A trace plot for categorical variable is not entirely meaningful... but imputations should be checked for plausibility
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
