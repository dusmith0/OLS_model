# Use this file to create tests/debug your functions

# Source the functions
source("FunctionsLM.R")

# A random set of small data for testing the function created in FunctionsLM.R
# to allow for testing if the functions work. 

x <- matrix(c(rep(1,5),1:5),nrow=5)
beta <- c(2,3) 
sigma <- 3


#First test
y_values <- generateY(x,beta,sigma)
View(y_values)
plot(x[ ,2],y_values)
summary(lm(y_values~x[ ,2]))

str(y_values)


#Second Test
calculateBeta(x,y_values)



