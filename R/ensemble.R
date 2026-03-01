#' This function is to evaluate performance of ensemble learning models including standard regression(linear, logistic) and regularized regression(ridge, LASSO, elastic net) processes. 
#' On every iteration, this function provides the selection of models to perform. Users can stop fitting models by input 6. 
#' @param X A matrix of predictor variables
#' @param y A vector of response variable
#' @param p A proportion of train data. Default is 0.8
#' @return If y is binary, it returns a list of
#'          1. model_summary: Count of model used in ensemble learning process 
#'          2. confusion.matrix: confusion matrix between final predictions and actual value of test data.\
#'          3. accuracy: overall accuracy based on confusion matrix
#'         If y is continuous, it returns a list of
#'          1. model_summary: Count of model used in ensemble learning process
#'          2. RMSE: RMSE(Root Mean Square Error) between predicted data and test data
#'          3. r_squared: R-squared, also known as coefficient of determination
#' @export
ensembleLearning <- function(X,y,p=0.8) {
  p <- as.numeric(p)
  if (p<=0 || p>=1 || is.na(p)==TRUE)
    stop("Invalid input for p. p must be decimal between 0 and 1")
  result <- check_data(X,y)
  traintest <- train_test(X,y,p)
  train_X <- traintest$train_X
  train_y <- traintest$train_y
  test_X <- traintest$test_X
  test_y <- traintest$test_y
  model_names <- character()
  model <- list()
  model_coef <- list()
  predictions <- list()
  i <- 1
  repeat {
    input <- readline(prompt="Choose model.
                      1 for standard regression,
                      2 for ridge, 
                      3 or LASSO, 
                      4 for elastic net, 
                      5 to exit.\n")
    input <- as.numeric(input)
    if (is.na(input) || input < 1 || input > 5)
      stop("Invalid model selection. Input must be an integer between 1 and 5.")
    if (input == 1) {
      model[[i]] <- fitStandard(train_X, train_y)
      predictions[[i]] <- predict(model[[i]], newx = test_X)
      i <- i+1
      if (result$response_type == "binary") 
        model_names <- c(model_names, "Logistic")
      else
        model_names <- c(model_names, "Linear")
    }
    if (input == 2) {
      model[[i]] <- fitRidge(train_X, train_y)
      predictions[[i]] <- predict(model[[i]], newx = test_X)
      i <- i+1
      model_names <- c(model_names, "Ridge")
    }
    if(input == 3) {
      model[[i]] <- fitLasso(train_X, train_y)
      predictions[[i]] <- predict(model[[i]],newx=test_X)
      i <- i+1
      model_names <- c(model_names, "LASSO")
    }
    if(input == 4) {
      model[[i]] <- fitElasticNet(train_X, train_y)
      predictions[[i]] <- predict(model[[i]],newx=test_X)
      i <- i+1
      model_names <- c(model_names, "Elastic Net")
    }
    if(input==5)
      break
  }
  predicted_y <- rowMeans(do.call(cbind,predictions))
  if(result$response_type=="binary") {
     predicted_y <- ifelse(predicted_y>0.5,1,0)
     confusionMatrix <- table(predicted_y,test_y)
     accuracy <- (confusionMatrix[1,1]+confusionMatrix[2,2])/(confusionMatrix[1,1]+confusionMatrix[1,2]+confusionMatrix[2,1]+confusionMatrix[2,2])
     return(list(model_summary = table(model_names),
                 confusion.matrix=confusionMatrix,
                 accuracy=accuracy))
  }
  if(result$response_type=="continuous") {
    sse <- sum((predicted_y - test_y)^2)
    sst <- sum((test_y - mean(test_y))^2)
    r_squared <- 1 - (sse/sst)
    RMSE <- sqrt(mean(test_y - predicted_y)^2)
    return(list(model_summary = table(model_names),
                RMSE=RMSE,
                r_squared = r_squared))
  }
}

