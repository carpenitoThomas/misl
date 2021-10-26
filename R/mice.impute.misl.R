#' Imputation by MISL
#'
#' The purpose of this file is to write the MISL function as if it were being implemented in the MICE package.
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
#' @export#'
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

  # Avoiding complications with variance estimates of the ensemble by using bootstrapping
  bootstrap_sample <- dplyr::sample_n(xobsyobs, size = nrow(xobs), replace = TRUE)

  # Checking the datatype for the super learner
  outcome_type <- check_datatype(yobs_boot)

  xvars <- colnames(xobs)

  # First, define the task using our bootstrap_sample (this helps with variability in imputations) and our full_dataframe sample
  sl3_task_boot_dot <- sl3::make_sl3_Task(bootstrap_sample, covariates = xvars, outcome = "yobs", outcome_type = outcome_type )
  sl3_task_full_hat <- sl3::make_sl3_Task(xobsyobs, covariates = xvars, outcome = "yobs", outcome_type = outcome_type )

  # Depending on the outcome, we need to build out the learners
  learners <- switch(outcome_type,
                     categorical = cat_method,
                     binomial = bin_method ,
                     continuous = con_method)

  ## Error message about categorical variables should go here

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

    dataset_master_copy[[column]] <- ifelse(is.na(dataset[[column]]), predicted_values, dataset[[column]])
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
    uniform_values <- rep(runif(length(predictions_boot_dot)), each = length(levels(dataset[[column]])))
    post <- sl3::unpack_predictions(predictions_boot_dot)
    draws <- uniform_values > apply(post, 1, cumsum)
    idx <- 1 + apply(draws, 2, sum)
    predicted_values <- levels(dataset[[column]])[idx]

    dataset_master_copy[[column]] <-  factor(ifelse(is.na(dataset[[column]]), predicted_values, as.character(dataset[[column]])), levels = levels(dataset[[column]]))

  }
}



set.seed(53177)
xname <- c("age", "hgt", "wgt")
r <- stats::complete.cases(boys[, xname])
x <- boys[r, xname]
y <- boys[r, "tv"]
ry <- !is.na(y)
table(ry)

yimp_pmm <- mice.impute.pmm(y, ry, x)
yimp_misl <- mice.impute.misl(y, ry, x)




