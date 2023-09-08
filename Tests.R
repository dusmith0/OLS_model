# Use this file to create tests/debug your functions

# Source the functions
source("FunctionsLM.R")
#install.packages("testthat")
library("testthat")


# A random set of small data for testing the function created in FunctionsLM.R
# to allow for testing if the functions work. 

x <- matrix(c(rep(1,5),1:5),nrow=5)
beta <- c(2,3) 
sigma <- 3


#First test of Generate Y
y_values <- generateY(x,beta,sigma)
View(y_values)
plot(x[ ,2],y_values)
summary(lm(y_values~x[ ,2]))

str(y_values)


#First Test of Calculate Beta
(beta_LS <- calculateBeta(x,y_values))

#First Test of calculateEstimationError
calculateEstimationError(beta,beta_LS)

#Hand calculation based on the above values
(est <- sqrt((2-2.875692)^2+(3-3.500952)^2))

#Comparing the two with TestThat
expect_equal(calculateEstimationError(beta,beta_LS),
            (est <- sqrt((2-2.875692)^2+(3-3.500952)^2)))

#First test of the CalculatePredictionError
calculatePredictionError(y_values,x,beta_LS)

#Hand calculation
(diffs <- (y_values-c(2.875692 + 3.500952 * c(1,2,3,4,5))))
(squared_diffs <- diffs ^ 2)
(hand <- sqrt(sum(squared_diffs)))

expect_equal(calculatePredictionError(y_values,x,beta_LS),hand)


###I am going to try to invent as weird linear regression models in an attempt
### to break my functions. 

### no intercept
x <- matrix(c(1:5),nrow=5)
beta <- c(3) 
sigma <- 1

(y_values <- generateY(x,beta,sigma))
beta_LS <- calculateBeta(x,y_values)
summary(lm(y_values~x))
calculateEstimationError(beta,beta_LS)
calculatePredictionError(y_values,x,beta_LS)


### Multiple regression 
X <- matrix(c(rep(1,5),2:6,c(13,10,1,1,4),c(12,3,4,4,4),c(1,3,4,3,9)),nrow=5)
beta <- c(1:5) 
sigma <- 1

y_values <- generateY(X,beta,sigma)
beta_LS <- calculateBeta(X,y_values)
summary(lm(y_values~X))
calculateEstimationError(beta,beta_LS)
calculatePredictionError(y_values,X,beta_LS)


## unequal matrix sizes. 
x <- matrix(c(rep(1,5),1:5,6:10,11:15,16:20),nrow=5)
beta <- c(1:4) 
sigma <- 1

y_values <- generateY(x,beta,sigma)

##Single Value matrix
x <- 10
beta <- c(1)
sigma <- 1


(y_values <- generateY(x,beta,sigma))
beta_LS <- calculateBeta(x,y_values)
summary(lm(y_values~x))
calculateEstimationError(beta,beta_LS)
calculatePredictionError(y_values,x,beta_LS)


##imputing values without specifying the variables
(y_values <- generateY(c(1,2,3),3,3))
(beta_LS <- calculateBeta(c(1,2,3),c(1,2)))
(beta_LS <- calculateBeta(c(1,2,3),c(1,2,3)))

#attempting a string input.
(y_values <- generateY((c("1",2,3)),3,3))


#Testing to see if the seed will change when it is added to the parameter. 

#This is the bare bones of the function. With seed 5832652
x <- matrix(c(rep(1,5),1:5),nrow=5)
beta <- c(2,3) 
sigma <- 3
seed = 5832652
set.seed(seed)
  
Y <- x %*% beta + rnorm(nrow(X), mean = 0, sd = sigma)
noise_first_seed <- rnorm(nrow(X), mean = 0, sd = sigma)

#Checking it against my function.
expect_equal(generateY(x,beta,sigma),Y)
#This returned no value, so I assume they are good!

#I am changing the seed to see if it will only change within the function correctly
x <- matrix(c(rep(1,5),1:5),nrow=5)
beta <- c(2,3) 
sigma <- 3
seed = 444666
set.seed(seed)

Y <- x %*% beta + rnorm(nrow(X), mean = 0, sd = sigma)
noise_second_seed <- rnorm(nrow(X), mean = 0, sd = sigma)

#Checking it against my function.
expect_equal(generateY(x,beta,sigma, seed=444666),Y)
#This was again the same. 

expect_equal(noise_first_seed,noise_second_seed)
#These are difference as expected.

#Now I want to check that the seed was not permanently set.
generateY(x,beta,sigma) #This will have the seed of 5832652
noise_third_seed <- rnorm(nrow(X), mean = 0, sd = sigma) #This seed should be something random

expect_equal(noise_first_seed,noise_third_seed)
#They are not equal. This implies that my function does not permenantly set my seed. 
#Which is what was expected. 









