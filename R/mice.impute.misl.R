#' Imputation by MISL
#'
#' The purpose of this file is to write the MISL function as if it were being implemented in the MICE package. Of importance, this method can be used for all data types with the exception of ordered factors (this algorithm returns the ordered factors as unordered).
#'
#' @aliases mice.impute.misl misl
#'
#' @inheritParams mice.impute.pmm
#' @return Vector with imputed data, same type as \code{y}, and of length
#' @param con_method A vector of strings to be supplied for building the super learner for columns containing continuous data. The default learners are \code{con_method = c("Lrnr_mean", "Lrnr_glm")}. To view all available learners, execute \code{list_learners("continuous")}
#' @param bin_method A vector of strings to be supplied for building the super learner for columns containing binomial data. Important to note that these values must only take on values \code{c(0,1,NA)}. The default learners are \code{bin_method = c("Lrnr_mean", "Lrnr_glm")}. To view all available learners, execute \code{list_learners("binomial")}
#' @param cat_method A vector of strings to be supplied for building the super learner for columns containing categorical data. The default learners are \code{bin_method = c("Lrnr_mean", "Lrnr_glmnet")}. To view all available learners, execute \code{list_learners("categorical")}
#' @author Thomas Carpenito, 2021
#' # Impute missing tv data - example taken from MICE package
#' xname <- c("age", "hgt", "wgt")
#' r <- stats::complete.cases(boys[, xname])
#' x <- boys[r, xname]
#' y <- boys[r, "tv"]
#' ry <- !is.na(y)
#'
#' yimp <- mice.impute.misl(y, ry, x)
#'
#'  # This method can also be incorporated into the default mice method as well
#'  mice(boys, method = c("misl"))
#' @export
mice.impute.misl <- function(y, ry, x, wy = NULL,
                             con_method = c("Lrnr_glm_fast", "Lrnr_earth", "Lrnr_ranger"),
                             bin_method = c("Lrnr_earth", "Lrnr_glm_fast", "Lrnr_ranger"),
                             cat_method = c("Lrnr_independent_binomial", "Lrnr_ranger"),
                             ...) {

  xobs <- data.frame(x[ry, , drop = FALSE])
  xmis <- data.frame(x[wy, , drop = FALSE])
  yobs <- y[ry]
  xobsyobs <- cbind(xobs,yobs)
  dataset_copy <- cbind(x, yobs = y)
  dataset_copy <- as.data.frame(dataset_copy)

  # Avoiding complications with variance estimates of the ensemble by using bootstrapping
  bootstrap_sample <- dplyr::sample_n(xobsyobs, size = nrow(xobs), replace = TRUE)

  # Checking the datatype for the super learner
  outcome_type <- check_datatype(yobs)

  xvars <- colnames(xobs)
  colnames(dataset_copy) <- c(xvars, "yobs" )

  # First, define the task using our bootstrap_sample (this helps with variability in imputations) and our full_dataframe sample
  sl3_task_boot_dot <- sl3::make_sl3_Task(bootstrap_sample, covariates = xvars, outcome = "yobs", outcome_type = outcome_type )
  sl3_task_full_hat <- sl3::make_sl3_Task(xobsyobs, covariates = xvars, outcome = "yobs", outcome_type = outcome_type )

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
    fy <- as.factor(y)
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

  predictions_task_boot_dot <- sl3::sl3_Task$new(dataset_copy, covariates = xvars, outcome = "yobs", outcome_type = outcome_type )

  predictions_boot_dot <- sl_stack_fit_boot_dot$predict(predictions_task_boot_dot)

  if(outcome_type == "continuous"){
    sl_train_full_hat <- sl3::delayed_learner_train(sl, sl3_task_full_hat)
    sl_sched_full_hat <- delayed::Scheduler$new(sl_train_full_hat, delayed::FutureJob)

    sl_stack_fit_full_hat <- sl_sched_full_hat$compute()

    predictions_task_full_hat <- sl3::sl3_Task$new(dataset_copy, covariates = xvars, outcome = "yobs", outcome_type = outcome_type )
    predictions_full_hat <- sl_stack_fit_full_hat$predict(predictions_task_full_hat)
  }

  if(outcome_type == "binomial"){
    # Imputation for binary variables can be found from the following resources:
    # https://stefvanbuuren.name/fimd/sec-categorical.html#def:binary
    # https://github.com/cran/mice/blob/master/R/mice.impute.logreg.R

    uniform_values <- runif(length(predictions_boot_dot))
    predicted_values <- as.integer(uniform_values <= predictions_boot_dot)

    predicted_values[!ry]
  }else if(outcome_type == "continuous"){
    predictions_boot_dot <- predictions_boot_dot

    # If continuous, we can do matching
    # Find the 5 closest donors and making a random draw from them - there are a lot of ways to do matching
    # https://stefvanbuuren.name/fimd/sec-pmm.html#sec:pmmcomputation
    # This matching was updated on 10/4 to help with speedup (15% reduction in time). We only match on missing values.
    list_of_matches <- c()
    non_na_predictions <- predictions_boot_dot[!ry]
    for(value in seq_along(non_na_predictions)){
      distance <- head(order(abs(non_na_predictions[value] - predictions_full_hat[ry])), 5)
      list_of_matches[value] <- y[ry][sample(distance,1)]
    }
    list_of_matches
  }else if(outcome_type== "categorical"){
    # For categorical data we follow advice suggested by Van Buuren:
    # https://github.com/cran/mice/blob/master/R/mice.impute.polyreg.R
    uniform_values <- rep(runif(length(predictions_boot_dot)), each = length(levels(fy)))
    post <- sl3::unpack_predictions(predictions_boot_dot)
    draws <- uniform_values > apply(post, 1, cumsum)
    idx <- 1 + apply(draws, 2, sum)
    predicted_values <- levels(fy)[idx]

    factor(predicted_values[!ry], levels = levels(fy))

  }
}
