#' Check data
#' This function checks if input X and y are valid type of data to perform further machine learning process
#' @param X A matrix of predictor variables
#' @param y A vector of response variable
#' @return This function returns a list consisting:
#'          response_type: type of response variable y(binary or continuous)
#'          y: finalized response value of y 
#' @export
check_data <- function(X, y) {
  # Convert y to binary if it is categorical (factor with two levels)
  if (is.factor(y) && length(levels(y)) == 2) {
    y <- as.numeric(y) - 1
    response_type <- "binary"
  } else if (is.numeric(y)) {
    unique_y <- sort(unique(y))
    if (all(unique_y %in% c(0, 1)) && length(unique_y) == 2) {
      response_type <- "binary"
    } else {
      response_type <- "continuous"
    }
  } else if (is.character(y) && length(unique(y)) == 2) {
    # Convert character to factor and then to numeric
    y <- as.numeric(factor(y)) - 1
    response_type <- "binary"
  } else {
    stop("Not valid type of response variable. y should be numeric, factor, or binary character.")
  }
  #check if X is a matrix
  if(!is.matrix(X)){
    stop("X is not a matrix")
  }
  return(list(response_type=response_type, y = y))
}

#' Generate test and train data
#' @param X A matrix of predictor variables
#' @param y A vector of response variable
#' @param p A proportion of train data
#' @export
train_test <- function(X,y,p) {
  n <- nrow(X)
  train_index <- sample(1:n,p*n)
  train_X <- X[train_index,]
  train_y <- y[train_index]
  test_X <- X[-train_index,]
  test_y <- y[-train_index]
  return(list(train_X = train_X, train_y = train_y, test_X = test_X, test_y = test_y))
}

#' Select K most informative predictors
#' @param X A matrix of predictor variables
#' @param y A vector of response variable
#' @param K The number of informative variables
#' @return The names of K most informative variables
#' @export
prescreening <- function(X,y,K=1) {
  if(K>ncol(X)) 
    stop("K cannot be larger than the number of predictors")
  if(K==0)
    stop("K cannot be 0")
  if(K<0 || is.na(K))
    stop("Invalid input of K.")
  if(ncol(X)<length(y)) {
    option <- readline(prompt = "The number of predictor is less than the number of observation. Do you want to continue? 1 for yes, 2 for no. ")
   if(option==1) {
     p <- ncol(X)
   cor <- rep(NA,times=p)
   names(cor) <- colnames(X)
   for(i in 1:p) {
     cor[i] <- abs(cor(X[,i],y))
   }
   sort_cor <- sort(cor,decreasing=TRUE)
   most_informative <- sort_cor[1:K]
   }
    if(option==2)
      stop("Function has been stopped")
    p <- ncol(X)
    cor <- rep(NA,times=p)
    names(cor) <- colnames(X)
    for(i in 1:p) {
      cor[i] <- abs(cor(X[,i],y))
    }
    sort_cor <- sort(cor,decreasing=TRUE)
    most_informative <- sort_cor[1:K]
    return(names(most_informative))
  }
 
}
