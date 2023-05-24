### Prepare data and test for changes in a set of sectors




# Load packages
library(tidyverse)
library(ggplot2)
# Get data
setwd('C:/Users/lchco/OneDrive/Documents/School/CSU/Summer_2023/change-point-finance')
load("9ETFs.RData")
sectors <- c('xlb' = 'materials', 'xle' = 'energy', 'xlf' = 'financials',
             'xli' = 'industrials', 'xlk' = 'technology', 'xlp' = 'consumer staples',
             'xlu' = 'utilities', 'xlv' = 'health care', 'xly' = 'consumer discretionary')
# helper functions
getReturns <- function(df) {
  returns <- rep(0, dim(df)[1])
  for (i in 2:dim(df)[1]) {
    returns[i] <- (df$close[i] - df$close[i-1])/df$close[i-1]
  }
  df$computedRet <- returns
  return(df)
}


################################################################################


## Create KDE and vectorize

# determine typical range of KDE (CIDR)
sector <- xlv.r ## Change this to experiment with different sectors
minCIDR <- rep(0,100)
maxCIDR <- rep(0, 100)
for (i in 1:100) {
  df <- sector %>% 
    filter(Date == sample(sector$Date, 1)) %>%
    mutate(Time = as.character(Time)) %>%
    filter(substr(Time, nchar(Time), nchar(Time)) == "0")
  
  kd <- density(df$CIDR)
  minCIDR[i] <- min(kd$x)
  maxCIDR[i] <- max(kd$x)
}
plot(density(minCIDR)) ## This shows most of the minimum CIDR are above -0.04
plot(density(maxCIDR)) ## This shows most of the maximum CIDR are below 0.04

# determine typical range of KDE (Regular returns)
minRet <- rep(0,100)
maxRet <- rep(0, 100)
for (i in 1:100) {
  df <- sector %>% 
    filter(Date == sample(sector$Date, 1)) %>%
    mutate(Time = as.character(Time)) %>%
    filter(substr(Time, nchar(Time), nchar(Time)) == "0")
  
  returns <- rep(0, dim(df)[1])
  for (i in 2:dim(df)[1]) {
    returns[i] <- (df$close[i] - df$close[i-1])/df$close[i-1]
  }
  df$computedRet <- returns
  
  kd <- density(df$computedRet)
  minRet[i] <- min(kd$x)
  maxRet[i] <- max(kd$x)
}
plot(density(minRet, na.rm = T)) ## This shows most of the minimum returns are above -0.001
plot(density(maxRet, na.rm=T)) ## This shows most of the maximum returns are below 0.001

# function to return vectorized kdes
kdeVector <- function(dateString, sectorDF, CIDR=T) {
  df <- sectorDF %>% 
    filter(Date == dateString) %>%
    mutate(Time = as.character(Time)) %>%
    filter(substr(Time, nchar(Time), nchar(Time)) == "0")
  
  if (CIDR) {
    kd <- density(df$CIDR)
    pointsOnSupport <- seq(-0.04, 0.04, length.out=50)
    vectorKDE <- rep(0, 50)
    for (i in 1:50) vectorKDE[i] <- kd$y[which.min(abs(kd$x - pointsOnSupport[i]))]
  }
  else {
    returns <- rep(0, dim(df)[1])
    for (i in 2:dim(df)[1]) {
      returns[i] <- (df$close[i] - df$close[i-1])/df$close[i-1]
    }
    df$computedRet <- returns
    
    kd <- density(df$computedRet)
    pointsOnSupport <- seq(-0.001, 0.001, length.out=50)
    vectorKDE <- rep(0, 50)
    for (i in 1:50) vectorKDE[i] <- kd$y[which.min(abs(kd$x - pointsOnSupport[i]))]
  }
  return(vectorKDE)
}

sector <- xlv.r ## Change this to experiment with different sectors
dateString <- sample(sector$Date, 1)
densityTest <- kdeVector(dateString, sector, CIDR=F)
plot(densityTest, main=dateString)


################################################################################


## Test for no change in a set of sectors

# function to return a list of all sector data frames with returns and formatted dates
getSectorDfList <- function() {
  sector.df <- list(xlb.r, xle.r, xlf.r, xli.r, xlk.r, xlp.r, xlu.r, xlv.r, xly.r)
  sector.df <- lapply(sector.df, getReturns)
  sector.df <- 
    lapply(sector.df, function(x) {
      mutate(x, Date = as.Date(Date, "%m/%d/%Y")) 
    })
  return(sector.df)
}

# function to return vectorized kdes updated
kdeVector <- function(dateString, sectorDF, CIDR=T) {
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

# function for vector concatenation
concatVectors <- function(startDate, endDate, sector.dfs, cidr=F) {
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

# function for partial sum process
PS <- function(t, A, f_t) {
  # A is a list of ints representing a subset of sectors (indexed from 1-9)
  # t is the stopping point of the sum
  partialSum <- rep(0, 9)
  for (s in A) {
    for (i in 1:t) {
      partialSum[s] <- partialSum[s] + sum(f_t[[i]][[s]])
    }
  }
  return(partialSum)
}

# function for cusum
cusum <- function(A, f_t) {
  cu <- 1/(50*length(A)*length(f_t)**2) * sum(sapply(1:length(f_t), function(x) {
      result <- 0
      for (vn in (PS(x, A, f_t) - x/length(f_t) * PS(length(f_t), A, f_t))) result <- result + vn**2
      return(result)
    }))
  return(cu)
}


## Test functions
sector.df.list <- getSectorDfList()
f_t <- concatVectors("07/21/2009", "09/10/2009", sector.df.list)
cusum(c(1:9), f_t)



