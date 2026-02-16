#' This function predicts the outcome of test data in ensemble learning of models including linear, logistic, ridge, lasso, elastic net, and random forest.
#' On every iteration, this function provides the selection of models to perform. Users can stop fitting models by input 7. 
#' @param X A matrix of predictor variables
#' @param y A vector of response variable
#' @param p A proportion of train data. Default is 0.8
#' @return If y is binary, it returns a list of
#'          1. confusion.matrix: confusion matrix between final predictions and actual value of test data.
#'          2. accuracy: overall accuracy based on confusion matrix
#'         If y is continuous, it returns a RMSE value between final predictions and actual value of test data.
#' @export
ensembleLearning <- function(X,y,p=0.8) {
  if (p<=0 || p>=1 || is.na(p)==TRUE)
    stop("Invalid input for p. p must be decimal between 0 and 1")
  result <- check_data(X,y)
  traintest <- train_test(X,y,p)
  train_X <- traintest$train_X
  train_y <- traintest$train_y
  test_X <- traintest$test_X
  test_y <- traintest$test_y
  model <- list()
  model_coef <- list()
  predictions <- list()
  i <- 1
  repeat {
    input <- readline(prompt="Choose model.\n 1 for logistic,\n 2 for linear,\n 3 or ridge,\n 4 for lasso,\n 5 for elastic net,\n 6 for random forest, \n 7 to exit. ")
    if (input == 1) {
      if(result$response_type =="binary") {
        model[[i]] <- fitLogisticModel(X,y)
        model_coef[[i]] <- coef(model[[i]])
        predictions[[i]] <- cbind(1,test_X) %*% model_coef[[i]]
        i <- i+1
      }
      else if(result$response_type =="continuous")
        stop("Response variable must be binary, not continuous")
    }
    if (input == 2) {
      if(result$response_type =="binary")
        stop("Response variable must be continuous, not binary")
      else if(result$response_type =="continuous") {
        model[[i]] <- fitLinearModel(X,y)
        model_coef[[i]] <- coef(model[[i]])
        predictions[[i]] <- cbind(1,test_X) %*% model_coef[[i]]
        i <- i+1
      }
    }
    if (input == 3) {
      model[[i]] <- fitRidgeModel(X,y)
      predictions[[i]] <- predict(model[[i]],newx=test_X)
      i <- i+1
    }
    if(input == 4) {
      model[[i]] <- fitLassoModel(X,y)
      model_coef[[i]] <- coef(model[[i]])
      predictions[[i]] <- predict(model[[i]],newx=test_X)
      i <- i+1
    }
    if(input == 5) {
      model[[i]] <- fitElasticNetModel(X,y)
      model_coef[[i]] <- coef(model[[i]])
      predictions[[i]] <- predict(model[[i]],newx=test_X)
      i <- i+1
    }
    if(input == 6) {
      model[[i]] <- fitRandomForestModel(X,y)
      predictions[[i]] <- predict(model[[i]],newx=test_X)
      i <- i+1
    }
    if(input==7)
      break
    if(input<=1 || input>=8 || is.na(input)==TRUE)
      stop("Invalid model selection")
  }
  final_predictions <- rowMeans(do.call(cbind,predictions))
  if(result$response_type=="binary") {
     final_predictions <- ifelse(final_predictions>0.5,1,0)
     confusionMatrix <- table(final_predictions,test_y)
     accuracy <- (confusionMatrix[1,1]+confusionMatrix[2,2])/(confusionMatrix[1,1]+confusionMatrix[1,2]+confusionMatrix[2,1]+confusionMatrix[2,2])
     return(list(confusion.matrix=confusionMatrix, accuracy=accuracy))
  }
  if(result$response_type=="continuous") {
    RMSE <- sqrt(mean(test_y-final_predictions)^2)
    return(RMSE)
  }
}

