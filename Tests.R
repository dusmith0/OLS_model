# Use this file to create tests/debug your functions

# Source the functions
source("FunctionsLM.R")
install.packages("testthat")
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
beta_LS <- calculateBeta(x,y_values)

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
(diffs <- (y_values-c(2+3,2+6,2+9,2+12,2+15)))
(squared_diffs <- diffs^2)
(hand <- sqrt(sum(squared_diffs)))

expect_equal(calculatePredictionError(y_values,x,beta_LS),hand)



