### Get p-values for hypothesis test



## Load packages, get data, define helper functions
################################################################################


# Load packages
library(tidyverse)
library(ggplot2)
library(rstudioapi)
library(ssdtools)

# helper functions
getDateRange <- function(numDays, sector.df.list) {
  # returns a random date range
  startDate <- sample(sector.df.list[[1]]$Date, 1)
  while (startDate > max(sector.df.list[[1]]$Date)-numDays) { 
    # check if there are enough data points given the start date
    startDate <- sample(sector.df.list[[1]]$Date, 1)
  }
  endDate <- startDate + numDays
  return(c(startDate, endDate))
}
getReturns <- function(df, logRet=F) {
  # compute returns and add to dataframe
  returns <- rep(0, dim(df)[1])
  if (logRet) {
    for (i in 2:dim(df)[1]) {
      returns[i] <- log(df$close[i]/df$close[i-1])
    }
  }
  else {
    for (i in 2:dim(df)[1]) {
      returns[i] <- (df$close[i] - df$close[i-1])/df$close[i-1]
    }
  }
  df$computedRet <- returns
  return(df)
}
getSectorDfList <- function(logret=F) {
  # return a list of sector data frames 
  sector.df <- list(xlb.r, xle.r, xlf.r, xli.r, xlk.r, xlp.r, xlu.r, xlv.r, xly.r)
  sector.df <- lapply(sector.df, getReturns, logRet=logret)
  sector.df <- 
    lapply(sector.df, function(x) {
      mutate(x, Date = as.Date(Date, "%m/%d/%Y")) 
    })
  return(sector.df)
}
kdeVector <- function(dateString, sectorDF, CIDR=F) {
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
concatDailyVectors <- function(f_t, A=c(1:9)) {
  dailyVectors <- vector('list', length=(length(f_t)))
  i = 1
  for (f in f_t) {
    dailyVector <- vector('numeric')
    for (a in A) {
      if (length(dailyVector) == 0) dailyVector <- f[[a]]
      else dailyVector <- c(dailyVector, f[[a]])
    }
    dailyVectors[[i]] <- dailyVector
    i <- i+1
  }
  return(dailyVectors)
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
dateRange <- getDateRange(30)
# concatenate vectors 
f_t <- concatVectors(dateRange[1], dateRange[2], sector.df.list)

# Covariance matrix for the first sector
covMatrix <- matrix(rep(0, 50**2), nrow=50, ncol=50)
for (i in 2:length(f_t)) {
  covMatrix <- covMatrix + as.matrix(f_t[[i]][[1]] - f_t[[i-1]][[1]]) %*% t(as.matrix(f_t[[i]][[1]] - f_t[[i-1]][[1]]))
}
covMatrix <- 1/(100*length(f_t)) * covMatrix

# Covariance matrix for all sectors
covMatrix <- matrix(rep(0, (50*9)**2), nrow=50*9, ncol=50*9)
dailyVectors <- concatDailyVectors(f_t, 1:9)
for (i in 2:length(dailyVectors)) {
  covMatrix <- covMatrix + as.matrix(dailyVectors[[i]] - dailyVectors[[i-1]]) %*% t(as.matrix(dailyVectors[[i]] - dailyVectors[[i-1]]))
}
covMatrix <- 1/(100*9*length(dailyVectors)) * covMatrix


# function for covariance matrix
getCovMatrix <- function(f_t, A = c(1:9)) {
  covMatrix <- matrix(rep(0, (50*length(A))**2), nrow=50*length(A), ncol=50*length(A))
  dailyVectors <- concatDailyVectors(f_t, A)
  for (i in 2:length(dailyVectors)) {
    covMatrix <- covMatrix + as.matrix(dailyVectors[[i]] - dailyVectors[[i-1]]) %*% t(as.matrix(dailyVectors[[i]] - dailyVectors[[i-1]]))
  }
  covMatrix <- 1/(100*length(A)*length(dailyVectors)) * covMatrix
  return(covMatrix)
}


################################################################################



## Eigenvalues
################################################################################


# function to calculate first B eigenvalues
getEigenvalues <- function(covMatrix, B=10) {
  eigenValues <- eigen(covMatrix, symmetric = T, only.values = T)$values
  return(eigenValues[1:B])
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

# function to simulate p-values
getPVals <- function(f_t, B = 30, A = c(1:9)) {
  covMatrix <- getCovMatrix(f_t, A)
  eigenVals <- getEigenvalues(covMatrix, B)
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



## Simulate p-values
nsim <- 100
numDays <- 30
A <- c(1:9)
sector.df.list <- getSectorDfList(logret = F)

pvalues <- rep(0, nsim)
dates <- rep(0, nsim)
for (i in 1:nsim) {
  # get random n day date range within data
  dateRange <- getDateRange(numDays, sector.df.list)
  # concatenate vectors 
  f_t <- concatVectors(dateRange[1], dateRange[2], sector.df.list)
  
  p <- getPVals(f_t)
  dates[i] <- dateRange[1]
  pvalues[i] <- p
  print(paste("(", i, "/", nsim, ") p-value for", numDays, "day interval (", dateRange[1], "-", dateRange[2], ") :", p))
}
class(dates) <- "Date"



## Plot results of the simulation
ggplot() +
  geom_histogram(aes(pvalues), bins=30) + 
  labs(title="Histogram of p-values", y="", x="p-values")

#ggsave("plots/pvals_500d_3vol.pdf", width = 6, height = 4)

pvals <- data.frame(dates, pvalues)
ggplot(data=pvals) + 
  geom_point(aes(x=dates, y=pvalues)) +
  geom_hline(aes(yintercept=0.05), col="red", size=1) +
  labs(title = "P-values VS Start Dates", y="P-value", x="Year") +
  scale_y_continuous(breaks = scales::pretty_breaks(n=10)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
  
#ggsave("plots/p_vs_date_500d_3vol.pdf", width = 6, height = 4)

ggplot(data=pvals[pvalues<0.05,]) + 
  geom_point(aes(x=dates, y=pvalues)) +
  geom_segment(aes(y=pvalues, x=dates, yend=pvalues, xend=dates+numDays), size=2, alpha=0.75)+
  geom_point(aes(x=dates+numDays, y=pvalues))+ 
  geom_vline(aes(xintercept=max(dates)), col="red", size=1) +
  geom_vline(aes(xintercept=min(dates+numDays)), col="red", size=1) +
  geom_xribbon(aes(xmin=max(dates), xmax=min(dates+numDays), y=pvalues), fill='red', alpha=0.5) +
  labs(title = "Significant Result Time Period Overlap", y="P-value", x="Date") +
  scale_y_continuous(breaks = scales::pretty_breaks(n=5)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y")

#ggsave("plots/sig_result_overlap.pdf", width = 6, height = 4)

data.frame(pvalues) %>%
  summarise(Mean = mean(pvalues), Min = min(pvalues), Max = max(pvalues), 
            SD = sd(pvalues), Rejections = sum((pvalues < 0.05)), 
            Percent_Rejected = sum((pvalues < 0.05))/nsim) %>%
  knitr::kable()

print(paste("Significant time period overlap is from", max(pvals[pvalues<0.05,]$dates), "to", min(pvals[pvalues<0.05,]$dates+numDays)))



## Narrow down results to get a smaller date range
nsim <- 100
A <- c(2,3,5)
sector.df.list <- lapply(getSectorDfList(logret = F), function(x) filter(x, Date >= as.Date("2008-12-26")))
sector.df.list <- lapply(sector.df.list, function(x) filter(x, Date <= as.Date('2009-03-30')))
numDays <- 30

pvalues <- rep(0, nsim)
dates <- rep(0, nsim)
for (i in 1:nsim) {
  # get random n day date range within data
  dateRange <- getDateRange(numDays, sector.df.list)
  # concatenate vectors 
  f_t <- concatVectors(dateRange[1], dateRange[2], sector.df.list)
  
  p <- getPVals(f_t)
  dates[i] <- dateRange[1]
  pvalues[i] <- p
  print(paste("(", i, "/", nsim, ") p-value for", numDays, "day interval (", dateRange[1], "-", dateRange[2], ") :", p))
}
class(dates) <- "Date"

ggplot() +
  geom_histogram(aes(pvalues), bins=30) + 
  labs(title="Histogram of p-values", y="", x="p-values")

pvals <- data.frame(dates, pvalues)
ggplot(data=pvals) + 
  geom_point(aes(x=dates, y=pvalues)) +
  geom_hline(aes(yintercept=0.05), col="red", size=1) +
  labs(title = "P-values VS Start Dates", y="P-value", x="Year") +
  scale_y_continuous(breaks = scales::pretty_breaks(n=10)) +
  scale_x_date(date_breaks = "10 days", date_labels = "%b %d")

ggplot(data=pvals[pvalues<0.05,]) + 
  geom_point(aes(x=dates, y=pvalues)) +
  geom_segment(aes(y=pvalues, x=dates, yend=pvalues, xend=dates+numDays), size=2, alpha=0.75)+
  geom_point(aes(x=dates+numDays, y=pvalues))+ 
  geom_vline(aes(xintercept=max(dates)), col="red", size=1) +
  geom_vline(aes(xintercept=min(dates+numDays)), col="red", size=1) +
  geom_xribbon(aes(xmin=max(dates), xmax=min(dates+numDays), y=pvalues), fill='red', alpha=0.5) +
  labs(title = "Significant Result Time Period Overlap", y="P-value", x="Date") +
  scale_y_continuous(breaks = scales::pretty_breaks(n=5)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y")

print(paste("Significant time period overlap is from", max(pvals[pvalues<0.05,]$dates), "to", min(pvals[pvalues<0.05,]$dates+numDays)))
      