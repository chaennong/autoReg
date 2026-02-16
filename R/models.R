#' This function fit linear model.
#' @param X A matrix of predictor variables
#' @param y A vector of response variable
#' @return fitted linear model
#' @export
fitLinearModel <- function(X,y){
  result <- check_data(X,y)
  if(result$response_type =="binary"){
    stop("Response variable type should be continuous for Linear Regression")
  }else{
    model <- lm(result$y~X)
  }
  return(model)
}

#' This function fit logistic model.
#' @param X A matrix of predictor variables
#' @param y A vector of response variable
#' @return Fitted logistic model
#' @export
fitLogisticModel <- function(X,y){
  result <- check_data(X,y)
  if(result$response_type =="continuous"){
    stop("Response variable type should be binary for Logistic Regression")
  }else{
    model <- glm(result$y~X, family = "binomial")
  }
  return(model)
}

#' This function fit ridge model.
#' @param X A matrix of predictor variables
#' @param y A vector of response variable
#' @return Fitted ridge model
#' @import glmnet
#' @export
fitRidgeModel <- function(X,y){
  result <- check_data(X,y)
  cv <- glmnet::cv.glmnet(X, result$y , alpha = 0)
  model <- glmnet::glmnet(X, result$y, alpha = 0, lambda = cv$lambda.min)
  return(model)
}

#' This function fit lasso model.
#' @param X A matrix of predictor variables
#' @param y A vector of response variable
#' @return Fitted lasso model 
#' @import glmnet
#' @export
fitLassoModel <- function(X,y){
  require("glmnet")
  result <- check_data(X,y)
  cv <- glmnet::cv.glmnet(X, result$y, alpha = 1)
  model <- glmnet::glmnet(X, result$y, alpha = 1, lambda = cv$lambda.min)
  return(model)
}

#' This function fit elastic net model.
#' @param X A matrix of predictor variables
#' @param y A vector of response variable
#' @return Fitted elastic net model
#' @import glmnet
#' @export
fitElasticNetModel <- function(X,y){
  require("glmnet")
  result <- check_data(X,y)
  cv <- glmnet::cv.glmnet(X, result$y, alpha = 0.5)
  model <- glmnet::glmnet(X, result$y, alpha = 0.5, lambda = cv$lambda.min)
  return(model)
}

#' This function fit random forest model
#' @param X A matrix of predictor variables
#' @param y A vector of response variable
#' @param ntree The number of trees to grow
#' @return Fitted random forest model
#' @import randomForest
#' @export
fitRandomForestModel <- function(X, y, ntree = 500) {
  require("randomForest")
  result <- check_data(X,y)
  model <- randomForest::randomForest(X,y, ntree = ntree)
  return(model)
}

#' This function is to predict outcomes of test data of single fitted model. 
#' @param X A matrix of predictor variables
#' @param y A vector of response variable
#' @param p The proportion of train data
#' @param ntree The number of trees to grow. This parameter is used to predict outcome on random frest model.
#' @return If y is binary, it returns a list of
#'        1. confusion.matrix: A confusion matrix between predicted data and test data
#'        2. accuracy: An accuracy of prediction 
#'        If y is continuous, it returns
#'        RMSE: RMSE(Root Mean Square Error) between predicted data and test data. 
#' @import glmnet
#' @import randomForest
#' @export
modelPrediction <- function(X,y,ntree=500,p=0.8) {
  result <- check_data(X,y)
  y <- result$y
  traintest <- train_test(X,y,p)
  train_X <- traintest$train_X
  train_y <- traintest$train_y
  test_X <- traintest$test_X
  test_y <- traintest$test_y
  modelselection <- readline(prompt = "Choose model: \n 1 for linear. \n 2 for logistic, \n 3 for ridge, \n 4 for lasso, \n 5 for elastic net, \n 6 for random foreset. \n 7 to stop.")
  if (modelselection == 1) {
    if (result$response_type == "binary") 
      stop("Invalid model type. Response variable should be continuous for linear regression.")
    model <- fitLinearModel(train_X,train_y)
    modelCoef <- coef(model)
  }
  if (modelselection == 2) {
    if(result$response_type == "continuous")
      stop("Invalid model type. Response variable should be binary for logistic regression.")
    model <- fitLogisticModel(train_X,train_y)
    modelCoef <- coef(model)
  }
  if (modelselection == 3) {
    model <- fitRidgeModel(train_X,train_y)
    modelCoef <- coef(model)
  }
  if (modelselection == 4) {
    model <- fitLassoModel(train_X,train_y)
    modelCoef <- coef(model)
  }
  if (modelselection == 5) {
    model <- fitElasticNetModel(train_X,train_y)
    modelCoef <- coef(model)
  }
  if (modelselection == 6) {
    model <- fitRandomForestModel(train_X, train_y)
    modelCoef <- coef(model)
  }
  if (modelselection < 1 || modelselection > 7 || is.na(modelselection)==TRUE)
    stop("Invalid model selection.")
  finalPrediction <- cbind(1,test_X) %*% modelCoef
  if (result$response_type=="binary") {
    finalPrediction <- ifelse(finalPrediction>0.5,1,0)
    confusionMatrix <- table(finalPrediction, test_y)
    accuracy <- (confusionMatrix[1,1]+confusionMatrix[2,2])/(confusionMatrix[1,1]+confusionMatrix[1,2]+confusionMatrix[2,1]+confusionMatrix[2,2])
    return(list(confusionMatrix=confusionMatrix,accuracy=accuracy))
  }
  if (result$response_type=="continuous") {
    RMSE <- sqrt(mean(test_y-finalPrediction)^2)
    return(RMSE)
  }
}


  
