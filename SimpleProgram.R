# Generate data from linear regression model and calculate the least squares vector of coefficients
#####################################################################################################

# Source your functions
source("FunctionsLM.R")

# Model parameters

p = 10 # number of covariates
sigma = 2 # noise standard deviation
beta = c(2, rep(0, p-1)) # true vector of coefficients

# Training data generator

n = 100 # sample size for training data
X = matrix(rnorm(n * p), n, p) # n by p matrix of predictors
# [ToDo] Use generateY function to generate Y for training data with default seed
Y <- generateY(X, beta, sigma)

# [ToDo] Use calculateBeta function to calculate beta_LS
beta_LS <- calculateBeta(X,Y)

# [ToDo] Use calculateEstimationError to assess the estimation error measured by squared eucledian distance - ||beta - beta_LS||_2^2. Report the error in the comments.
calculateEstimationError(beta,beta_LS)

# Testing data generator
n = 200 # sample size for testing data
Xtest = matrix(rnorm(n * p), n, p) # n by p matrix of covariates
# [ToDo] Use generateY function to generate Ytest for testing data with seed = 678910
Y <- generateY(Xtest, beta, sigma, seed=678910)
beta_LS <- calculateBeta(Xtest,Y)

# [ToDo] Use calculatePredictionError to asses the prediction error on Ytest. 
  #Report the error in the comments.
prediction_error <- calculatePredictionError(Y,Xtest,beta_LS)
prediction_error #this produced an error of 26.71015

# [ToDo] Use calculatePredictionError to asses the prediction error on Ytest based only on the first covariate.
# Report the error in the comments.
# Hint: to avoid error of non-conformable arguments, use Xtest[, 1, drop = FALSE]
single_X <- Xtest[ , 1, drop = FALSE]
Y <- generateY(single_X, beta[1], sigma, seed=678910)
beta_LS <- calculateBeta(single_X,Y)
prediction_error <- calculatePredictionError(Y,single_X,beta_LS[1])
prediction_error #this produced an error of 27.28538

