# Lab 12
# Calculating Integration Numerically

# function to calculate the value of y at an x value
fof <- function(x){
  y <- 3*x + 2
  return(y)
}

# function to calculate integration using trapezoidal method
trapezoidal <- function(a, b, n){
  # calculate h
  h <- (b-a)/n
  fvals <- 0
  
  # loop to calculate sum of y vals
  if ((n-1) >= 1){
    for (i in 1:(n-1)){
      fvals <- fvals + fof(a+(i*h))
    }
  }
  fvals <- fvals * 2
  
  # caclculate the integration
  integration <- (b-a)/(2*n) * (fof(a) + fvals + fof(b))
  return(integration)
}

# function to calculate integration using simpsons method
simpson <- function(a, b, n){
  # calculate h
  h <- (b-a)/n
  feven <- 0
  fodd <- 0
  
  # caclulate the sum of f(xi) for odd numbers, if there are enough x points
  if((n-1) >= 1){
    # loop for each odd x value
    for(i in seq(from = 1, to = (n-1), by = 2)){
      fodd <- fodd + fof(a+(h*i))
    }
  }
  fodd <- fodd * 4
  
  # caclulate the sum of f(xi) for even numbers, if there are enough x points
  if((n-2) > 1){
    # loop for each even x value
    for(i in seq(from = 2, to = (n-2), by = 2)){
      feven <- feven + fof(a+(h*i))
    }
  }
  feven <- feven * 2
  
  # calculate the integration
  integration <- ((b - a)/(3*n)) * (fof(a) + fodd + feven + fof(b))
  return(integration)
}

# function that calls necessary calculations and prints information
main <- function(a, b, n, trueI){
  trap <- vector(mode = "numeric", length = n)
  simp <- vector(mode = "numeric", length = n)
  absErrT <- vector(mode = "numeric", length = n)
  relErrT <- vector(mode = "numeric", length = n)
  absErrS <- vector(mode = "numeric", length = n)
  relErrS <- vector(mode = "numeric", length = n)
  
  # loop to find integration at each step size
  for(i in 1:n){
    simp[i] <- simpson(a, b, i)
    trap[i] <- trapezoidal(a, b, i)
  }
  
  # replace simpson calculations with NA
  for(i in seq(from = 1, to = n, by = 2)){
    simp[i] <- NA
  }
  
  # caclulate errors
  absErrT <- abs(trap - trueI)
  relErrT <- abs(absErrT / trueI) * 100
  absErrS <- abs(simp - trueI)
  relErrS <- abs(absErrS / trueI) * 100
  
  # create and print matrix of our findings
  nVec <- seq(from = 1, to = n, by = 1)
  table <- cbind(nVec, trap, simp, absErrT, absErrS, relErrT, relErrS)
  colnames(table) <- c("n", "  Trapezoidal", "  Simpson's 1/3", "  Trapezoidal AE", "  Simpson's AE", "  Trapezoidal RE", "  Simpson's RE")
  writeLines(paste("\nThe actual value of this integration is", trueI, "\n"))
  prmatrix(table, rowlab=rep("",length(nVec)))
  
  # plot the graph
  xVec <- seq(from = a, to = b, by = 0.1)
  yVec <- fof(xVec)
  plot(xVec, yVec, type = "l", col = "blue")
}

# call main fucntion
main(0, 2, 3, 4.91)


