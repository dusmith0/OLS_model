# Ordinary Least Squares Model on Randomly Generated Data

**Description** This program was written to play with play with the Least Squares Model with randomly generated data. The idea is to attempt to recreate parameters given some noise being added to the model. First we generate a large amount of data from a normal curve, choose some parameters for beta, and generate response value with some added noise. Then we fit a model and attempt to recreate the betas, and reduce the MSE as much as possible.  

## Functions

#### generateY
  This function is used to generate y values to test on. 
  Parameters:
  
  1. X: a Matrix of data points to be used for x. 
  2. beta: Initial beta values
  3. sigma: an assigned number to control the variation of the added noise.  
  4. seed: set at 5832652
    
    ```{r, eval = TRUE, echo = TRUE}
    p = 10 # number of covariates
    sigma = 2 # noise standard deviation
    beta = c(2, rep(0, p-1)) # true vector of coefficients

    n = 100 # sample size for training data
    X = matrix(rnorm(n * p), n, p) # n by p matrix of predictors
    Y <- generateY(X, beta, sigma)
    head(Y)
    dim(Y)

    ```
    
#### calculateBeta 
    This function generates estimated betas using the typical LSRL matrix calculation.
    $(X^t*X)^-1*X^t*Y$
    Parameters:
    1.  X: The data set of explanatory variables (Matrix)
    2.  Y: The data set of generated response variables (Vector)

      ```{r, echo = TRUE, eval = TRUE}
      beta_est <- calculateBeta(X,Y)
      head(beta_est)
      beta
      ```
    
#### calculateEstimationError 
    This function calculates the total squared error in Beta estimates. I am using the given formula below:
    $sum((beta - beta_est) ^2)$
    
    ```{r, echo = TRUE, eval = TRUE}
    calculateEstimationError(beta,beta_LS)
    ```
    
#### calculatePredictionError
    This function calculates the MSE in Y-value predictions and actuality using the formula below:
    $sqrt(sum((Y - X %*% beta_LS) ^ 2))$
    
    
#### Example
  ```{r, echo = TRUE, eval = TRUE}
  # Testing data generator
  n = 200 # sample size for testing data
  Xtest = matrix(rnorm(n * p), n, p) # n by p matrix of covariates
  Y <- generateY(Xtest, beta, sigma, seed=678910)
  beta_LS <- calculateBeta(Xtest,Y)
  beta_LS
  head(X)
  head(Y)

  (calculatePredictionError(Y,Xtest,beta_LS))
  ```

  
    
    
    
    
    
    
    
    
    
    
    
    
    
    
