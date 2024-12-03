#' Train Models and Summarize Results

#'

#' @param data A dataframe containing the independent and dependent variables.

#' @param independent_var A string specifying the name of the independent variable.

#' @param dependent_vars A vector of strings specifying the names of dependent variables.

#'

#' @return A dataframe summarizing RMSE and R-squared for each model (KSVM, SVM, Linear Regression and Random Forest) and dependent variable.

#' @export
#'
#' @examples train_models_summary(data = read.csv("Electricity1970.csv"), independent_var = "cost", dependent_vars = c("labor", "capital", "fuel"))
#'
#' @examples data <- read.csv("Electricity1970.csv")
#' train_models_summary(data, independent_var = "cost", dependent_vars = c("labor", "capital", "fuel"))
#'
#'
#'
#'

train_models_summary <- function(data, independent_var, dependent_vars) {

  library(tidyverse)

  library(caret)

  library(kernlab)

  library(randomForest)

  library(broom)

  library(e1071)



  # Split the data into train and test set

  set.seed(123)

  train_index <- createDataPartition(data[[dependent_vars[1]]], p = 0.7, list = FALSE)

  train_data <- data[train_index, ]

  test_data <- data[-train_index, ]



  # Define the training and evaluation function

  train_and_evaluate <- function(dep_var, train_data, test_data) {

    formula <- as.formula(paste(dep_var, "~", independent_var))



    # Linear Regression

    lm_model <- lm(formula, data = train_data) #fit linear regression model

    lm_pred <- predict(lm_model, test_data) # Set linear regression prediction

    lm_rmse <- RMSE(lm_pred, test_data[[dep_var]]) # Calculate Root Mean Square error

    lm_r2 <- R2(lm_pred, test_data[[dep_var]]) #calculate R squared



    # SVM

    svm_model <- svm(formula, data = train_data) #fit SVM model

    svm_pred <- predict(svm_model, test_data) # Set SVM prediction

    svm_rmse <- RMSE(svm_pred, test_data[[dep_var]]) # Calculate Root Mean Square error

    svm_r2 <- R2(svm_pred, test_data[[dep_var]])  # Calculate R squared


    # KSVM

    ksvm_model <- ksvm(formula, data = train_data, kernel = "rbfdot")   #fit KSVM mdoel

    ksvm_pred <- predict(ksvm_model, test_data) #set KSVM prediction

    ksvm_rmse <- RMSE(ksvm_pred, test_data[[dep_var]]) #calculate Root Mean Square error

    ksvm_r2 <- R2(ksvm_pred, test_data[[dep_var]]) #Calculate R squared



    # Random Forest

    rf_model <- randomForest(formula, data = train_data)  #fit Random Forest model

    rf_pred <- predict(rf_model, test_data) #set Random Forest prediction

    rf_rmse <- RMSE(rf_pred, test_data[[dep_var]]) #calculate Root Mean Square error

    rf_r2 <- R2(rf_pred, test_data[[dep_var]]) #Calculate R squared



    # Compile results

    results <- tibble(

      Model = c("Linear Regression", "SVM", "KSVM", "Random Forest"),

      RMSE = c(lm_rmse, svm_rmse, ksvm_rmse, rf_rmse),

      R_Squared = c(lm_r2, svm_r2, ksvm_r2, rf_r2)

    ) %>%

      mutate(Dependent_Variable = dep_var)



    return(results)

  }



  # Apply the function to all dependent variables

  all_results <- lapply(dependent_vars, function(dep) {

    train_and_evaluate(dep, train_data, test_data)

  })



  # Combine all results

  combined_results <- bind_rows(all_results) %>%

    select(Dependent_Variable, everything())



  # Find the best R2 and RMSE per dependent variable
  best_results <- combined_results %>%
    group_by(Dependent_Variable) %>%
    summarise(
      best_R2_Model = Model[which.max(R_Squared)],
      best_R2 = max(R_Squared),
      best_RMSE_Model = Model[which.min(RMSE)],
      best_RMSE = min(RMSE)
    )

  return(list(All_Results = combined_results, Best_Results = best_results))

}

