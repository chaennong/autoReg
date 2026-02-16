#' This function performs bootstrap bagging R times. R must be given by users. 
#' @param X A matrix of predictor variable
#' @param y A vector of response variable
#' @param R The number of bootstrap replicates to perform
#' @param p The proportion of train data 
#' @return This function returns final.model, which is final bagged model.
#' @export
baggingLogistic <- function(X,y,R,p) {
  result <- check_data(X,y)
  traintest <- train_test(X,y,p)
  train_X <- traintest$train_X
  train_y <- traintest$train_y
  test_X <- traintest$test_X
  test_y <- traintest$test_y
  models <- list()
  predictions <- list()
  if(is.matrix(X)==FALSE || is.na(X)==TRUE)
    stop("Invalid X. X must be matrix.")
  if(is.vector(y)==FALSE || is.na(y)==TRUE)
    stop("Invalid y. y must be vector.")
  if(nrow(X) != length(y))
    stop("The number of row in X and the length of y must be same.")
  if (R<=1 || is.na(R)==TRUE) 
    stop("Invalid input of R. R has to be positive integer greater than 1")
  if(p<=0 || p>=1 || is.na(p)==TRUE)
    stop("Invalid value of p. p must be decimal between 0 and 1.")
  if (result$response_type=="continuous") 
    stop("Not valid response variable. Response variable must be binary for logistic regression.")
  else {
    for(i in 1:R) {
      sample_indicies <- sample(1:nrow(train_X),nrow(train_X),replace=TRUE)
      X_samples <- train_X[sample_indicies,]
      y_samples <- train_y[sample_indicies]
      models[[i]] <- fitLogisticModel(X_samples,y_samples)
      predictions[[i]] <- cbind(1,test_X) %*% coef(models[[i]])
    }
    final.predictions <- rowMeans(do.call(cbind,predictions))
    final.predictions <- ifelse(final.predictions>0.5,1,0)
    cm <- table(final.predictions,test_y)
    accuracy <- (cm[1,1]+cm[2,2])/(cm[1,1]+cm[1,2]+cm[2,1]+cm[2,2])
  }
  return(list(confusionMatrix=cm, accuracy=accuracy))
}

#' This function performs bootstrap bagging R times. R must be given by users.
#' @param X A matrix of predictor variables
#' @param y A vector of response variable
#' @param R The number of bootstrap replicates to perform 
#' @return This function returns final.model, which is final bagged model.
#' @export
baggingLinear <- function(X,y,R,p) {
  result <- check_data(X,y)
  traintest <- train_test(X,y,p)
  models <- list()
  models.coef <- list()
  if (R<=1 || is.na(R)==TRUE)
    stop("Invalid input of R. R must be an integer greater than 1.")
  if (result$response_type=="binary") {
    stop("Not valid response variable. Response variable must be continuous for linear regression.")
  }
  else {
    for(i in 1:R) {
      sample_indicies <- sample(1:nrow(X),nrow(X),replace=TRUE)
      X_samples <- X[sample_indicies,]
      y_samples <- y[sample_indicies]
      models[[i]] <- fitLinearModel(X_samples,y_samples)
      predictions[[i]] <- cbind(1,test_X) %*% coef(models[[i]])
    }
    final.predictions <- rowMeans(do.call(cbind,predictions))
    RMSE <- sqrt(mean(test_y-final.predictions)^2)
  }
  return(RMSE)
}

#' This function performs bootstrap bagging of ridge model R times. R must be given by users.
#' @param X A matrix of predictor variables
#' @param y A vector of response variable
#' @param R The number of bootstrap replicates to perform 
#' @returns This function returns a list of:
#'            1. final.coef: Coefficients of final bagged model 
#'            2. var.score: Variance importance score 
#' @export
baggingRidge <- function(X,y,R) {
  result <- check_data(X,y)
  models <- list()
  models.coef <- list()
  if (R<=1 || is.na(R)==TRUE)
    stop("Invalid input of R. R must be an integer greater than 1.")
  for(i in 1:R) {
    sample_indicies <- sample(1:nrow(X),nrow(X),replace=TRUE)
    X_samples <- X[sample_indicies,]
    y_samples <- y[sample_indicies]
    models[[i]] <- fitRidgeModel(X_samples,y_samples)
    models.coef[[i]] <- as.numeric(coef(models[[i]]))
  }
  coef.avg <- rowMeans(do.call(cbind,models.coef))
  return(list(coef.avg=coef.avg,models=models))
}

#' This function performs bootstrap bagging of LASSO model R times. R must be given by users.
#' @param X A matrix of predictor variables
#' @param y A vector of response variable
#' @param R The number of bootstrap replicates to perform 
#' @returns This function returns a list of:
#'            1. final.coef: Coefficients of final bagged model 
#'            2. var.score: Variance importance score 
#' @export
baggingLasso <- function(X,y,R,p=0.8) {
  result <- check_data(X,y)
  models <- list()
  models.coef <- list()
  if (R<=1 || is.na(R)==TRUE)
    stop("Invalid input of R. R must be an integer greater than 1.")
  for(i in 1:R) {
    sample_indicies <- sample(1:nrow(X),nrow(X),replace=TRUE)
    X_samples <- X[sample_indicies,]
    y_samples <- y[sample_indicies]
    models[[i]] <- fitLassoModel(X_samples,y_samples)
    models.coef[[i]] <- as.numeric(coef(models[[i]]))
  }
  coef.avg <- rowMeans(do.call(cbind,models.coef))
  coef.score <- abs(coef.avg[-1])
  return(list(coef.avg=coef.avg,coef.score=coef.score))
}

#' This function performs bootstrap bagging of elastic model R times. R must be given by users.
#' @param X A matrix of predictor variables
#' @param y A vector of response variable
#' @param R The number of bootstrap replicates to perform 
#' @returns This function returns a list of:
#'            1. final.coef: Coefficients of final bagged model 
#'            2. var.score: Variance importance score 
#' @export 
baggingElasticNet <- function(X,y,R,p=0.8) {
  result <- check_data(X,y)
  models <- list()
  models.coef <- list()
  if (R<=1 || is.na(R)==TRUE)
    stop("Invalid input of R. R must be an integer greater than 1.")
  for(i in 1:R) {
    sample_indicies <- sample(1:nrow(X),nrow(X),replace=TRUE)
    X_samples <- X[sample_indicies,]
    y_samples <- y[sample_indicies]
    models[[i]] <- fitElasticNetModel(X_samples,y_samples)
    models.coef[[i]] <- as.numeric(coef(models[[i]]))
  }
  coef.avg <- rowMeans(do.call(cbind,models.coef))
  coef.score <- abs(coef.avg[-1])
  return(list(coef.avg,coef.score=coef.score))
}


#' This function predict outcomes of test data of bagged logistic model.
#' @param X A matrix of predictor variable
#' @param y A vector of response variable
#' @param R The number of bootstrap replicates to perform with default of 50
#' @param p A proportion of train data with default of 0.8
#' @return If y is binary, it returns a list of: 
#'        1. confusion.matrix: A confusion matrix between predicted data and test data 
#'        2. accuracy: An accuracy of prediction 
#'        If y is continuous, it returns 
#'        RMSE: RMSE(Root Mean Square Error) between predicted data and test data. 
#' @export
baggingPrediction <- function(X,y,R=50,p=0.8) {
  result <- check_data(X,y)
  testtrain <- train_test(X,y,p)
  train_X <- testtrain$train_X
  train_y <- testtrain$train_y
  test_X <- testtrain$test_X
  test_y <-testtrain$test_y
  if (R<=1 || is.na(R)==TRUE)
    stop("Invalid input of R. R must be an integer greater than 1.")
  if (p>=1 || p<=0 || is.na(p)==TRUE)
    stop("Invalid input of p. p must be a decimal greater than 0 than less than 1.")
  modelSelection <- readline(prompt="Choose model for bagging method: \n 1 for logistic \n 2 for linaer \n 3 for ridge \n 4 for lasso \n 5 for elastic net.")
  if(modelSelection==1) {
    if(result$response_type=="continuous")
      stop("Invalid model type for continuous data")
    baggedModel <- baggingLogistic(train_X,train_y,R)
  }
  if(modelSelection==2) {
    if(result$response_type=="binary")
      stop("Invalid model type for binary data")
    baggedModel <- baggingLinear(train_X,train_y,R)
  }
  if(modelSelection==3)
    baggedModel <- baggingRidge(train_X,train_y,R)
  if(modelSelection==4)
    baggedModel <- baggingLasso(train_X,train_y,R)
  if(modelSelection==5)
    baggedModel <- baggingElasticNet(train_X,train_y,R)
  if(modelSelection<1 || modelSelection>5 || is.na(modelSelection)==TRUE)
    stop("Invalid model selection.")
  if(modelSelection==1 || modelSelection==2)
    baggedCoef <-coef(baggedModel$final.model)
  if(modelSelection==3 || modelSelection==4 || modelSelection==5)
    baggedCoef <- baggedModel$coef.avg
  finalPrediction <- cbind(1,test_X) %*% baggedCoef
  if(result$response_type=="binary") {
    finalPrediction <- ifelse(finalPrediction>0.5,1,0)
    confusionMatrix <- table(finalPrediction,test_y)
    accuracy <- (confusionMatrix[1,1]+confusionMatrix[2,2])/(confusionMatrix[1,1]+confusionMatrix[1,2]+confusionMatrix[2,1]+confusionMatrix[2,2])
    return(list(confusionMatrix=confusionMatrix,accuracy=accuracy))
  }
  else if(result$response_type=="continuous") {
    RMSE <- sqrt(mean(test_y-finalPrediction)^2)
    return(list(RMSE=RMSE))
  }
return(finalPrediction) 
}

