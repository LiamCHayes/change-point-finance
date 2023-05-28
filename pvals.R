### Get p-values for hypothesis test



## Load packages, get data, define helper functions
################################################################################


# Load packages
library(tidyverse)
library(ggplot2)
library(rstudioapi)

# helper functions
getReturns <- function(df) {
  # compute returns and add to dataframe
  returns <- rep(0, dim(df)[1])
  for (i in 2:dim(df)[1]) {
    returns[i] <- (df$close[i] - df$close[i-1])/df$close[i-1]
  }
  df$computedRet <- returns
  return(df)
}
getSectorDfList <- function() {
  # return a list of sector data frames 
  sector.df <- list(xlb.r, xle.r, xlf.r, xli.r, xlk.r, xlp.r, xlu.r, xlv.r, xly.r)
  sector.df <- lapply(sector.df, getReturns)
  sector.df <- 
    lapply(sector.df, function(x) {
      mutate(x, Date = as.Date(Date, "%m/%d/%Y")) 
    })
  return(sector.df)
}
kdeVector <- function(dateString, sectorDF, CIDR=T) {
  # return vectorized (discrete) kdes
  df <- sectorDF %>% 
    filter(Date == as.Date(dateString, "%m%d%Y")) %>%
    mutate(Time = as.character(Time)) %>%
    filter(substr(Time, nchar(Time), nchar(Time)) == "0")
  
  if (CIDR) {
    kd <- density(df$CIDR)
    pointsOnSupport <- seq(-0.04, 0.04, length.out=50)
    vectorKDE <- rep(0, 50)
    for (i in 1:50) vectorKDE[i] <- kd$y[which.min(abs(kd$x - pointsOnSupport[i]))]
  }
  else {
    kd <- density(df$computedRet)
    pointsOnSupport <- seq(-0.001, 0.001, length.out=50)
    vectorKDE <- rep(0, 50)
    for (i in 1:50) vectorKDE[i] <- kd$y[which.min(abs(kd$x - pointsOnSupport[i]))]
  }
  return(vectorKDE)
}
concatVectors <- function(startDate, endDate, sector.dfs, cidr=F) {
  # concatenate vectors of kdes for a certain time period
  sector.df.timeFrame <- 
    lapply(sector.dfs, function(x) {
      filter(x, Date > as.Date(startDate, "%m/%d/%Y"),
             Date < as.Date(endDate, "%m/%d/%Y")
      )
    })
  tradingDays <- unique(sector.df.timeFrame[[1]]$Date)
  f_t <- rep(list(c(0,0,0,0,0,0,0,0,0)), length(tradingDays))
  for (i in 1:length(tradingDays)) {
    t <- lapply(sector.df.timeFrame, kdeVector, dateString=tradingDays[i], CIDR=cidr)
    for (j in 1:9) f_t[[i]][j] <- t[j]
  }
  return(f_t)
}
PS <- function(t, A, f_t) {
  # computes partial sum of concatenated vectors
  # A is a list of ints representing a subset of sectors (indexed from 1-9)
  # t is the stopping point of the sum
  partialSum <- vector('list', length=9)
  for (s in A) {
    for (i in 1:t) {
      if (i == 1) partialSum[[s]] <- f_t[[i]][[s]]
      else partialSum[[s]] <- partialSum[[s]] + f_t[[i]][[s]]
    }
  }
  return(partialSum)
}
cusum <- function(A, f_t) {
  cu <- 1/(50*length(A)*length(f_t)**2) * rowSums(sapply(1:length(f_t), function(x) {
    insideSum <- vector('list', length=9)
    result <- rep(0, 9)
    # compute the vector PS(t,A) - (t/T)*PS(T,A)
    for (i in A) {
      insideSum[[i]] <- PS(x, A, f_t)[[i]] - x/length(f_t) * PS(length(f_t), A, f_t)[[i]]
    }
    # compute vector norm
    for (i in A) {
      for (num in insideSum[[i]]) result[i] <- result[i] + num**2
    }
    return(result)
  }))
  return(sum(cu))
}

# Get data
setwd(dirname(getActiveDocumentContext()$path))
load("9ETFs.RData")
sectors <- c('xlb' = 'materials', 'xle' = 'energy', 'xlf' = 'financials',
             'xli' = 'industrials', 'xlk' = 'technology', 'xlp' = 'consumer staples',
             'xlu' = 'utilities', 'xlv' = 'health care', 'xly' = 'consumer discretionary')
sector.df.list <- getSectorDfList()


################################################################################



## Covariance matrix
################################################################################


# get random n day date range
numDays <- 30
startDate <- sample(sector.df.list[[1]]$Date, 1)
endDate <- startDate + numDays
# concatenate vectors 
f_t <- concatVectors(startDate, endDate, sector.df.list)

# Covariance matrix for the first sector
covMatrix <- matrix(rep(0, 50**2), nrow=50, ncol=50)
for (i in 2:length(f_t)) {
  covMatrix <- covMatrix + as.matrix(f_t[[i]][[1]] - f_t[[i-1]][[1]]) %*% t(as.matrix(f_t[[i]][[1]] - f_t[[i-1]][[1]]))
}

# Covariance matrix for all sectors
covMatrices <- lapply(1:9, function(a) {
  covMatrix <- matrix(rep(0, 50**2), nrow=50, ncol=50)
  for (i in 2:length(f_t)) {
    covMatrix <- covMatrix + as.matrix(f_t[[i]][[a]] - f_t[[i-1]][[a]]) %*% t(as.matrix(f_t[[i]][[a]] - f_t[[i-1]][[a]]))
  }
  return(covMatrix)
})

# function for covariance matrix
getCovMatrices <- function(f_t) {
  covMatrices <- lapply(1:9, function(a) {
    covMatrix <- matrix(rep(0, 50**2), nrow=50, ncol=50)
    for (i in 2:length(f_t)) {
      covMatrix <- covMatrix + as.matrix(f_t[[i]][[a]] - f_t[[i-1]][[a]]) %*% t(as.matrix(f_t[[i]][[a]] - f_t[[i-1]][[a]]))
    }
    return(covMatrix)
  })
  return(covMatrices)
}


################################################################################



## Eigenvalues
################################################################################


# function to calculate first B eigenvalues
getEigenvalues <- function(covMatrices, B=10) {
  eigenValues <- lapply(covMatrices, function(x) {
    eigen(x, symmetric=T, only.values=T)
  })
  
  eigenValues <- lapply(eigenValues, function(x) {
    return(x$values[1:B])
  })
  return(eigenValues)
}



################################################################################



## Simulate p-values
################################################################################


brownian_bridge <- function() {
  # Generate a Brownian bridge evaluated on the grid 1:1000/1000
  t <- seq(0, 1, length.out = 1001)  # Grid from 1 to 1000 divided by 1000
  dt <- diff(t)  # Calculate the differences between adjacent points
  # Generate standard Brownian motion
  dW <- sqrt(dt) * rnorm(length(dt))
  W <- c(0, cumsum(dW))
  # Generate Brownian bridge as linear combination of Brownian motion with
  # itself
  B <- W - t * W[length(W)]
  return(B)
}

integrated_brownian_bridge <- function(K) {
  # Generate a square integrated Brownian bridge
  results = 1:K*0
  # result vector
  for(i in 1:K){
    results[i] = sum(brownian_bridge()^2)/length(brownian_bridge())
    # Create independent BB in each run; squared sum over gridpoints
    # provides a Riemann-approximation of the integral
  }
  return(results)
}

B <- 10
covMatrices <- getCovMatrices(f_t)
eigenVals <- getEigenvalues(covMatrices, B)
brwnBridges <- vector('list', length=B)
for (i in 1:length(brwnBridges)) {
  brwnBridges[[i]] <- integrated_brownian_bridge(1000)
}
v_ell <- rep(0, 1000)
i <- 1
for (e in eigenVals) {
  brwnBridges[[i]] <- e * brwnBridges[[i]]
  i <- i+1
}
for (i in 1:1000) {
  s <- 0
  for (j in 1:B) {
    s <- s + brwnBridges[[j]][i]
  }
  v_ell[i] <- s
}
v_ell <- sort(v_ell, decreasing = F)

CUSUM <- cusum(c(1:9), f_t)

which.min(abs(CUSUM[1] - v_ell))

# function to simulate p-values
getPVals <- function(f_t, B = 10, A = c(1:9)) {
  covMatrices <- getCovMatrices(f_t)
  eigenVals <- getEigenvalues(covMatrices, B)
  brwnBridges <- vector('list', length=B)
  for (i in 1:length(brwnBridges)) {
    brwnBridges[[i]] <- integrated_brownian_bridge(1000)
  }
  v_ell <- rep(0, 1000)
  i <- 1
  for (e in eigenVals) {
    brwnBridges[[i]] <- e * brwnBridges[[i]]
    i <- i+1
  }
  for (i in 1:1000) {
    s <- 0
    for (j in 1:B) {
      s <- s + brwnBridges[[j]][i]
    }
    v_ell[i] <- s
  }
  v_ell <- sort(v_ell, decreasing = F)
  
  CUSUM <- cusum(A, f_t)
  
  pval <- 1 - which.min(abs(CUSUM[1] - v_ell))/1000
  
  return(pval)
}

################################################################################



## Test p-values
nsim <- 100
numDays <- 200
A <- 1:9

pvalues <- rep(0, nsim)
for (i in 1:nsim) {
  # get random n day date range within data
  startDate <- sample(sector.df.list[[1]]$Date, 1)
  while (startDate > max(sector.df.list[[1]]$Date)-numDays) { 
    # check if there are enough data points given the start date
    startDate <- sample(sector.df.list[[1]]$Date, 1)
  }
  endDate <- startDate + numDays
  # concatenate vectors 
  f_t <- concatVectors(startDate, endDate, sector.df.list)
  
  pvalues[i] <- getPVals(f_t)
  if (i %% 10 == 0) print(paste(i/nsim*100, "% done"))
}

ggplot() +
  geom_histogram(aes(pvalues)) + 
  labs(title=paste("Histogram of p-values, Time Period =", numDays, "Days"), 
       y="", x="p-values")
  



