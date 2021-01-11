### NOTE this file should be deleted and is only kept for reference.


# This file is simply the MISL function

# Include any dependencies
library("SuperLearner")
library("mice")
library("ggplot2")
library("gridExtra")
library("dplyr")

#Define the mode to be used
Mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

misl <- function(dataset){
  trace_plot_row_counter <- 1
  imputed_datasets <- c()
  trace_plot <- data.frame(mean_value = NA, sd_value = NA, variable = NA, m = NA, iteration = NA)
  for(m in 1:5){
    print(paste("M ", m))
    # To make this a multiple imputation, we will do this five times
    # Note, here we prioritize by amount of available data
    # Columns with MORE information (LESS missing data) are imputed first

    columns <- colnames(md.pattern(dataset, plot = FALSE)[,1:length(dataset)])

    dataset_master_copy <- dataset

    # Begins the interations
    for(iteration in 1:10){
      #print(paste("Iteration ", iteration))

      for(column in columns){
        #print(column)
        if(iteration ==1){
          full_dataframe <- dataset_master_copy[!is.na(dataset_master_copy[[column]]), ]
        }else{
          full_dataframe <- new_imputed_dataset
          #missing_yvar <- is.na(dataset_master_copy[[column]])
          #full_dataframe <- dataset_master_copy[!missing_yvar, ]
        }


        yvar <- full_dataframe[[column]]
        xvars <- full_dataframe[ , -which(names(full_dataframe) %in% c(column)), drop = FALSE]

        # Keep track of the missing y_values
        missing_yvar <- is.na(dataset[[column]])

        # If the previous imputation did not impute values... then we will catch with mean and mode
        for(i in 1:ncol(xvars)){
          if(length(levels(as.factor(xvars[,i]))) == 2){
            xvars[is.na(xvars[,i]), i] <-  Mode(xvars[,i])
          }else{
            xvars[is.na(xvars[,i]), i] <-  mean(xvars[,i], na.rm = TRUE)
          }
        }

        # Note, sometimes the SuperLearner assigns 0 weight - this is has been requested to be turned into a warning.
        if(length(levels(as.factor(yvar))) == 2){
          # This is going to fail if the factor is two levels but isn't 0-1.
          sl_lib = c( "SL.glm", "SL.lda", "SL.mean")
          superlearner_column <-  try(SuperLearner(Y = yvar, X = xvars ,SL.library = sl_lib, family = binomial(), method = "method.NNLS"), silent = TRUE)
          flag = ifelse(inherits(superlearner_column, "try-error"), TRUE, FALSE)
        }else{
          sl_lib = c("SL.glm", "SL.earth", "SL.loess", "SL.mean")
          superlearner_column <- try(SuperLearner(Y = yvar, X = xvars ,SL.library = sl_lib, family = gaussian(), method = "method.NNLS"), silent = TRUE)
          flag = ifelse(inherits(superlearner_column, "try-error"), TRUE, FALSE)
        }

        # This is useful for printing out the weights of the super learner per iteration
        if(!flag){
          #print(superlearner_column$coef)
        }

        # Make predictions on the dataset
        # First we need to grab the original dataset and set the columns to either the mean or mode for the first iteration
        if(iteration == 1){
          dataset_copy <- dataset_master_copy

          for(i in 1:ncol(dataset_copy)){
            if(length(levels(as.factor(dataset_copy[,i]))) == 2){
              dataset_copy[is.na(dataset_copy[,i]), i] <-  Mode(dataset_copy[,i])
            }else{
              dataset_copy[is.na(dataset_copy[,i]), i] <-  mean(dataset_copy[,i], na.rm = TRUE)
            }
          }
        }else{
          dataset_copy <- full_dataframe
        }


        new_x <- dataset_copy[ , -which(names(dataset_copy) %in% c(column)), drop = FALSE]

        # We need to add the exception catcher so we can still make predictions
        # So, if Superlearner fails we'll just use the mean
        # Also new, we add a bit of noise to our imputations.
        if(flag){
          dataset_master_copy[[column]]<- ifelse(is.na(dataset[[column]]), mean(dataset[[column]], na.rm = TRUE), dataset[[column]])
        }else{
          prediction_summary <- predict(superlearner_column, newdata = new_x, onlySL = TRUE)
          if(length(levels(as.factor(yvar))) == 2){
            #dataset_master_copy[[column]] <- ifelse(missing_yvar, prediction_summary$pred, dataset[[column]])
            predicted_values <- rbinom(length(dataset_master_copy[[column]]), 1, prediction_summary$pred)
            dataset_master_copy[[column]] <- ifelse(missing_yvar, predicted_values, dataset[[column]])

          }else{
            dataset_master_copy[[column]]<- ifelse(missing_yvar, prediction_summary$pred + rnorm(n = length(prediction_summary$pred), mean = 0, sd = sd(prediction_summary$pred) ), dataset[[column]])
          }

        }

        # If the column was binary, we should round it
        if(length(levels(as.factor(yvar))) == 2){
          #dataset_master_copy[[column]] <- dataset_master_copy[[column]] > .5
          #dataset_master_copy[[column]] <- rbinom(length(dataset_master_copy[[column]]), 1, prediction_summary$pred)
        }
        # Append to the trace plot
        trace_plot[trace_plot_row_counter,] <- c(mean(dataset_master_copy[[column]][missing_yvar]), sd(dataset_master_copy[[column]][missing_yvar]), column, m, iteration)
        # increase the trace_plot_row_counter
        trace_plot_row_counter <- trace_plot_row_counter + 1
      }

      new_imputed_dataset <- dataset_master_copy
    }
    imputed_datasets <- append(imputed_datasets, list(new_imputed_dataset))
  }
  object <- list(datasets = imputed_datasets, trace = trace_plot)
  return(list(object))
}



### Testing function




