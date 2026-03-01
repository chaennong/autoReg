#' This function performs bootstrap bagging of standard regression for R times.
#' @param X A matrix of predictor variables
#' @param y A vector of response variable
#' @param R The number of bootstrap replicates to perform with default of 50
#' @return This function returns a list of:
#'         1. coef: Coefficients of final bagged model
#'         2. sd.coef: Standard deviation of coefficients
#'         3. pos.rate: Sign consistency rate of each coefficient
#' @export 
baggingStandard <- function(X, y, R = 50) {
  R <- as.numeric(R)
  if (is.na(R) == TRUE || R <= 1) 
    stop("Invalid input of R. R has to be positive integer greater than 1")
  models <- list()
  models.coef <- list()
  for(i in 1:R) {
    sample_indicies <- sample(1:nrow(X), nrow(X), replace = TRUE)
    X_samples <- X[sample_indicies,]
    y_samples <- y[sample_indicies]
    models[[i]] <- fitStandard(X_samples, y_samples)
    models.coef[[i]] <- coef(models[[i]])
  }
  coef_matrix <- do.call(cbind,models.coef)
  mean.coef <- rowMeans(coef_matrix)
  sd.coef <- apply(coef_matrix, 1, sd)
  pos.rate <- apply(coef_matrix, 1, function(x) mean(x>0, na.rm=TRUE))
  return(list(coef = mean.coef, 
              sd.coef = sd.coef,
              pos.rate = pos.rate))
}


#' This function performs bootstrap bagging of ridge regression for R times.
#' @param X A matrix of predictor variables
#' @param y A vector of response variable
#' @param R The number of bootstrap replicates to perform with default of 50
#' @returns This function returns a list of:
#'            1. coef: Coefficients of final bagged model 
#'            2. sd.coef: Standard deviation of each coefficients
#' @export
baggingRidge <- function(X, y, R = 50) {
  R <- as.numeric(R)
  if (is.na(R) == TRUE || R <= 1)
    stop("Invalid input of R. R must be an integer greater than 1.")
  models <- list()
  models.coef <- list()
  for(i in 1:R) {
    sample_indicies <- sample(1:nrow(X), nrow(X), replace=TRUE)
    X_samples <- X[sample_indicies,]
    y_samples <- y[sample_indicies]
    models[[i]] <- fitRidge(X_samples, y_samples)
    models.coef[[i]] <- as.numeric(coef(models[[i]]))
  }
  coef_matrix <- do.call(cbind, models.coef)
  mean.coef <- rowMeans(coef_matrix)
  sd.coef <- apply(coef_matrix, 1, sd)
  selection.rate <- apply(coef_matrix, 1, function(x) mean(x != 0))
  return(list(coef = mean.coef,
              sd.coef = sd.coef))
}

#' This function performs bootstrap bagging of LASSO regression for R times. 
#' @param X A matrix of predictor variables
#' @param y A vector of response variable
#' @param R The number of bootstrap replicates to perform with default of 50
#' @returns This function returns a list of:
#'            1. coef: Coefficients of final bagged model 
#'            2. selection.rate: Selection frequency for each bag
#' @export
baggingLasso <- function(X, y, R = 50) {
  R <- as.numeric(R)
  if (is.na(R) == TRUE || R <= 1)
    stop("Invalid input of R. R must be an integer greater than 1.")
  models <- list()
  models.coef <- list()
  for(i in 1:R) {
    sample_indicies <- sample(1:nrow(X), nrow(X), replace=TRUE)
    X_samples <- X[sample_indicies,]
    y_samples <- y[sample_indicies]
    models[[i]] <- fitLasso(X_samples, y_samples)
    models.coef[[i]] <- as.numeric(coef(models[[i]]))
  }
  coef_matrix <- do.call(cbind, models.coef)
  mean.coef <- rowMeans(coef_matrix)
  sd.coef <- apply(coef_matrix, 1, sd)
  selection.rate <- apply(coef_matrix, 1, function(x) mean(x != 0))
  return(list(coef = mean.coef,
              selection.rate = selection.rate))
}

#' This function performs bootstrap bagging of elastic net for R times. 
#' @param X A matrix of predictor variables
#' @param y A vector of response variable
#' @param R The number of bootstrap replicates to perform with default of 50
#' @returns This function returns a list of:
#'            1. coef: Coefficients of final bagged model 
#'            2. sd.coef: Standard deviation of each coefficients
#'            3. selection.rate: Selection frequency for each bag
#' @export 
baggingElasticNet <- function(X, y, R = 50) {
  R <- as.numeric(R)
  if (is.na(R) == TRUE || R <= 1)
    stop("Invalid input of R. R must be an integer greater than 1.")
  models <- list()
  models.coef <- list()
  for(i in 1:R) {
    sample_indicies <- sample(1:nrow(X), nrow(X), replace=TRUE)
    X_samples <- X[sample_indicies,]
    y_samples <- y[sample_indicies]
    models[[i]] <- fitElasticNet(X_samples, y_samples)
    models.coef[[i]] <- as.numeric(coef(models[[i]]))
  }
  coef_matrix <- do.call(cbind, models.coef)
  mean.coef <- rowMeans(coef_matrix)
  sd.coef <- apply(coef_matrix, 1, sd)
  selection.rate <- apply(coef_matrix, 1, function(x) mean(x != 0))
  return(list(coef = mean.coef,
              sd.coef = sd.coef,
              selection.rate = selection.rate))
}


#' This function is to evaluate performance of model obtained by bootstrap bagging process. 
#' @param X A matrix of predictor variable
#' @param y A vector of response variable
#' @param model Regression model specification ("standard, "ridge", "lasso", "elastic.net") with default of "standard"
#' @param R The number of bootstrap replicates to perform with default of 50
#' @param p A proportion of train data with default of 0.8
#' @return If y is binary, it returns a list of: 
#'        1. confusion.matrix: A confusion matrix between predicted data and test data 
#'        2. accuracy: An accuracy of prediction 
#'        If y is continuous, it returns a list of
#'        1. r_squared: R-squared(Coefficient of Determination)
#'        2. RMSE: RMSE(Root Mean Square Error) between predicted data and test data
#' @export
evaluateBagging <- function(X, y, model = "standard", R = 50, p = 0.8) {
  result <- check_data(X, y)
  R <- as.numeric(R)
  if (is.na(R) == TRUE || R <= 1)
    stop("Invalid input of R. R must be an integer greater than 1.")
  p <- as.numeric(p)
  if (is.na(p) == TRUE || p >= 1 || p <= 0)
    stop("Invalid input of p. p must be a decimal greater than 0 than less than 1.")
  else {
    testtrain <- train_test(X,y,p)
    train_X <- testtrain$train_X
    train_y <- testtrain$train_y
    test_X <- testtrain$test_X
    test_y <-testtrain$test_y
  }
  valid_model <- c("standard", "ridge", "lasso", "elastic.net")
  if (!(model %in% valid_model) || is.na(model)) {
    stop(paste("Invalid model. Expected one of:", paste(valid_model, collapse = ", ")))
  }
  baggedModel <- switch(model,
                        "standard" = baggingStandard(train_X, train_y, R),
                        "ridge" = baggingRidge(train_X, train_y, R),
                        "lasso" = baggingLasso(train_X, train_y, R),
                        "elastic.net" = baggingElasticNet(train_X, train_y, R),
                        stop("Model name is not recognized."))
  coef <- baggedModel$coef
  predicted_y <- cbind(1,test_X) %*% coef
  if(result$response_type=="binary") {
    finalPrediction <- ifelse(predicted_y>0.5,1,0)
    cm <- table(finalPrediction,test_y)
    accuracy <- (cm[1,1]+cm[2,2])/(cm[1,1]+cm[1,2]+cm[2,1]+cm[2,2])
    return(list(confusion.matrix=cm,
                accuracy=accuracy))
  }
  else if(result$response_type=="continuous") {
    sse <- sum((predicted_y - test_y)^2)
    sst <- sum((test_y - mean(test_y))^2)
    r_squared <- 1 - (sse/sst)
    RMSE <- sqrt(mean(test_y - predicted_y)^2)
    return(list(r_squared = r_squared,
                RMSE=RMSE))
  }
}

