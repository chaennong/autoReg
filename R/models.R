#' This function fits standard regression model.
#' @param X A matrix of predictor variables
#' @param y A vector of response variable
#' @retrun Fitted standard regression model
#' @export
fitStandard <- function(X,y) {
  result <- check_data(X,y)
  if(result$response_type == "binary") {
    model <- glm(result$y~X, family = "binomial")
  } else {
    model <- lm(result$y~X)
  }
  return(model)
}

#' This function fits ridge regression model.
#' @param X A matrix of predictor variables
#' @param y A vector of response variable
#' @return Fitted ridge regression model
#' @import glmnet
#' @export
fitRidge <- function(X,y){
  require("glmnet")
  result <- check_data(X,y)
  cv <- glmnet::cv.glmnet(X, result$y , alpha = 0)
  model <- glmnet::glmnet(X, result$y, alpha = 0, lambda = cv$lambda.min)
  return(model)
}

#' This function fits LASSO regression model.
#' @param X A matrix of predictor variables
#' @param y A vector of response variable
#' @return Fitted LASSO regression model 
#' @import glmnet
#' @export
fitLasso <- function(X,y) {
  require("glmnet")
  result <- check_data(X,y)
  cv <- glmnet::cv.glmnet(X, result$y, alpha = 1)
  model <- glmnet::glmnet(X, result$y, alpha = 1, lambda = cv$lambda.min)
  return(model)
}

#' This function fits elastic net model.
#' @param X A matrix of predictor variables
#' @param y A vector of response variable
#' @return Fitted elastic net model
#' @import glmnet
#' @export
fitElasticNet <- function(X,y){
  require("glmnet")
  result <- check_data(X,y)
  cv <- glmnet::cv.glmnet(X, result$y, alpha = 0.5)
  model <- glmnet::glmnet(X, result$y, alpha = 0.5, lambda = cv$lambda.min)
  return(model)
}

#' This function is to evaluate performance of model obtained by single regression process. 
#' @param X A matrix of predictor variables
#' @param y A vector of response variable
#' @param p The proportion of train data
#' @param model Regression model specification ("standard, "ridge", "lasso", "elastic.net") with default of "standard"
#' @return If y is binary, it returns a list of
#'        1. confusion.matrix: A confusion matrix between predicted data and test data
#'        2. accuracy: An accuracy of prediction 
#'        If y is continuous, it returns a list of
#'        1. r_squared: R-squared(Coefficient of determination)  
#'        2. RMSE: RMSE(Root Mean Square Error) between predicted data and test data.
#' @export
evaluateModel <- function(X, y, model = "standard", p = 0.8) {
  result <- check_data(X,y)
  y <- result$y
  traintest <- train_test(X,y,p)
  train_X <- traintest$train_X
  train_y <- traintest$train_y
  test_X <- traintest$test_X
  test_y <- traintest$test_y
  valid_model <- c("standard", "ridge", "lasso", "elastic.net")
  if (!(model %in% valid_model) || is.na(model)) {
    stop(paste("Please specify model correctly. Expected one of:", paste(valid_model, collapse = ", ")))
  }
  model <- switch(model,
                  "standard" = fitStandard(train_X, train_y),
                  "ridge" = fitRidge(train_X, train_y),
                  "lasso" = fitLasso(train_X, train_y),
                  "elastic.net" = fitElasticNet(train_X, train_y),
                  stop("Model name is not recognized."))
  modelCoef <- coef(model)
  predicted_y <- cbind(1,test_X) %*% modelCoef
  if (result$response_type=="binary") {
    finalPrediction <- ifelse(predicted_y>0.5,1,0)
    cm <- table(finalPrediction, test_y)
    accuracy <- (cm[1,1]+cm[2,2])/(cm[1,1]+cm[1,2]+cm[2,1]+cm[2,2])
    return(list(confusion.matrix=cm,
                accuracy=accuracy))
  }
  if (result$response_type=="continuous") {
    sse <- sum((predicted_y - test_y)^2)
    sst <- sum((test_y - mean(test_y))^2)
    r_squared <- 1 - (sse/sst)
    RMSE <- sqrt(mean(test_y - predicted_y)^2)
    return(list(r_squared=r_squared, 
                RMSE=RMSE))
  }
}


  
