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
  # computes cusum statistic
  cu <- 1/(50*length(A)*length(f_t)**2) * rowSums(sapply(1:length(f_t), function(x) {
    insideSum <- vector('list', length=9)
    result <- rep(0, 9)
    # compute the vector PS(t,A) - (t/T)*PS(T,A)
    for (i in A) {
      insideSum[i] <- PS(x, A, f_t)[[i]] - x/length(f_t) * PS(length(f_t), A, f_t)[[i]]
    }
    # compute vector norm
    for (i in A) {
      for (num in insideSum[[i]]) result[i] <- result[i] + num**2
    }
    return(result)
  }))
  return(cu)
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