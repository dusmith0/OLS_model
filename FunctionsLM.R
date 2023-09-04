# Generate n-dimensional response Y that follows linear regression model Y = Xbeta + epsilon, 
# where epsilon is normal zero with variance sigma^2 independent across samples. 
# Seed should be set at the beginning of the function


####Function One
# X - design matrix, rows are n samples
# beta - given parameter vector (could be a vector or a matrix with 1 column)
# sigma - standard deviation of the noise, scalar
# seed  - starting seed value, integer

generateY <- function(X, beta, sigma, seed = 5832652){
  #Set seed and generate Y following linear model, note to self: default seed is 5832562
  set.seed(seed)
  if(length(beta) != ncol(X)){
    stop(paste("The parameters Beta must match the number of columns (or x values) in your model"))
  } 
  Y <- X %*% beta + rnorm(nrow(X), mean = 0, sd = sigma)
  # Return Y
  return(Y)
}


####Function Two
# Calculate beta_LS - least-squares solution, do not use lm function
# You can assume that X has full rank, so X'X inverse exists
# X - design matrix, rows are n samples
# Y - response vector (could be a vector or a matrix with 1 column)

calculateBeta <- function(X, Y){
  # Calculate beta_LS
  beta_LS <- solve(t(X) %*% X) %*% t(X) %*% (Y) 
  # Return beta
  return(beta_LS)
}


####Function Three
# Calculate estimation error, defined as ||beta - beta_LS||_2^2
# beta - true coefficient vector (could be a vector or a matrix with 1 column)
# beta_LS - vector estimated by LS (could be a vector or a matrix with 1 column)

calculateEstimationError <- function(beta, beta_LS){
  # Calculate and return error
  Estimate_Error <- sqrt(sum((beta - beta_LS) ^ 2))
  return(Estimate_Error)
}


####Function Four
# Calculate prediction error, defined as ||Y - X beta_LS||_2^2
# Y - response vector (could be a vector or a matrix with 1 column)
# X - design matrix, rows are n samples
# beta_LS - vector estimated by LS (could be a vector or a matrix with 1 column)

calculatePredictionError <- function(Y, X, beta_LS){
  # Calculate and return error
  Estimate_Prediction <- sqrt(sum((Y - X %*% beta_LS) ^ 2))
  return(Estimate_Prediction)
}




