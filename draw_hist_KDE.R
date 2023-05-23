### Exploring the data and drawing distributions



# Load packages
library(tidyverse)
library(ggplot2)
# Get data
setwd('C:/Users/lchco/OneDrive/Documents/School/CSU/Summer 2023/change-point-finance')
load("9ETFs.RData")
sectors <- c('xlb' = 'materials', 'xle' = 'energy', 'xlf' = 'financials',
             'xli' = 'industrials', 'xlk' = 'technology', 'xlp' = 'consumer staples',
             'xlu' = 'utilities', 'xlv' = 'health care', 'xly' = 'consumer discretionary')




## Technology sector explore with 10 minute returns
summary(xlk.r)
tech <- xlk %>% mutate(Time = Time/100) %>% filter(Time %% 10 == 0)

# Whole data set
techPlot <- ggplot(data=tech)
techPlot + geom_boxplot(aes(CIDR))
techPlot + geom_histogram(aes(CIDR), bins=500)
techPlot + stat_qq(aes(sample=CIDR)) + stat_qq_line(aes(sample=CIDR))
techPlot + geom_point(aes(x=1:length(CIDR), y=CIDR))

# Calculate regular returns
techOct52005 <- xlk.r %>% 
  filter(Date == "10/5/2005") %>%
  mutate(Time = as.character(Time)) %>%
  filter(substr(Time, nchar(Time), nchar(Time)) == "0")

returns <- rep(0, dim(techOct52005)[1])
for (i in 2:dim(techOct52005)[1]) {
  returns[i] <- (techOct52005$close[i] - techOct52005$close[i-1])/techOct52005$close[i-1]
}
techOct52005$computedRet <- returns
techOct52005Plot <- ggplot(data = techOct52005)

# Plot returns
techOct52005Plot + 
  geom_histogram(aes(computedRet), bins=40) +
  labs(title = "Histogram of Returns - XLK 10/5/2005", x = "Returns", y="") +
  theme_bw()

techOct52005Plot + 
  geom_density(aes(computedRet)) +
  labs(title = "KDE of Returns - XLK 10/5/2005", x = "Returns", y="") +
  theme_bw()

# CIDR curve plot and replication
techOct52005Plot +
  geom_point(aes(y=CIDR, x=1:length(CIDR))) +
  labs(title="CIDR curve - XLK 10/5/2005", x="k")

CIDR_replication <- c(log(techOct52005$close) - log(techOct52005$close[1]))
ggplot() + 
  geom_point(aes(y=CIDR_replication, x=1:length(CIDR_replication))) +
  labs(title="CIDR curve replication - XLK 10/5/2005", x="k")

# Different day, still tech
techDec302010 <- xlk.r %>% 
  filter(Date == "12/30/2010") %>%
  mutate(Time = as.character(Time)) %>%
  filter(substr(Time, nchar(Time), nchar(Time)) == "0")

returns <- rep(0, dim(techDec302010)[1])
for (i in 2:dim(techDec302010)[1]) {
  returns[i] <- (techDec302010$close[i] - techDec302010$close[i-1])/techDec302010$close[i-1]
}
techDec302010$computedRet <- returns
techDec302010Plot <- ggplot(data = techDec302010)

techDec302010Plot + 
  geom_histogram(aes(computedRet), bins=40) +
  labs(title = "Histogram of Returns - XLK 12/30/2010", x = "Returns", y="") +
  theme_bw()

techDec302010Plot + 
  geom_density(aes(computedRet)) +
  labs(title = "KDE of Returns - XLK 12/30/2010", x = "Returns", y="") +
  theme_bw()




## Create function to draw histogram and KDE given a date and sector dataframe (must be .r)
drawHistogram <- function(dateString, sectorDF, CIDR=T) {
  d <- sectorDF %>% 
    filter(Date == dateString) %>%
    mutate(Time = as.character(Time)) %>%
    filter(substr(Time, nchar(Time), nchar(Time)) == "0")
  
  returns <- rep(0, dim(d)[1])
  for (i in 2:dim(d)[1]) {
    returns[i] <- (d$close[i] - d$close[i-1])/d$close[i-1]
  }
  d$computedRet <- returns
  dPlot <- ggplot(data = d)
  
  if(CIDR) {
    pl <- dPlot + 
      geom_histogram(aes(CIDR), bins=40) +
      labs(title = paste("Histogram of CIDR - ", str_to_upper(substr(deparse(substitute(xlk.r)), 1, 3)), dateString), x = "Returns", y="") +
      theme_bw()
  }
  else {
    pl <- dPlot + 
      geom_histogram(aes(computedRet), bins=40) +
      labs(title = paste("Histogram of Returns - ", str_to_upper(substr(deparse(substitute(xlk.r)), 1, 3)), dateString), x = "Returns", y="") +
      theme_bw()
  }
 
  
  return(pl)
}

drawKDE <- function(dateString, sectorDF, CIDR=T) {
  d <- sectorDF %>% 
    filter(Date == dateString) %>%
    mutate(Time = as.character(Time)) %>%
    filter(substr(Time, nchar(Time), nchar(Time)) == "0")
  
  returns <- rep(0, dim(d)[1])
  for (i in 2:dim(d)[1]) {
    returns[i] <- (d$close[i] - d$close[i-1])/d$close[i-1]
  }
  d$computedRet <- returns
  dPlot <- ggplot(data = d)
  
  if (CIDR) {
    pl <- dPlot + 
      geom_density(aes(CIDR)) +
      labs(title = paste("KDE of CIDR - ", str_to_upper(substr(deparse(substitute(xlk.r)), 1, 3)), dateString), x = "Returns", y="") +
      theme_bw()
  }
  else {
    pl <- dPlot + 
      geom_density(aes(computedRet)) +
      labs(title = paste("KDE of Returns - ", str_to_upper(substr(deparse(substitute(xlk.r)), 1, 3)), dateString), x = "Returns", y="") +
      theme_bw()
  }
  
  return(pl)
}




## Plots
random_date <- sample(xlk.r$Date, 1)
# draw histogram of returns
xlk_hist_returns <- drawHistogram(random_date, xlk.r, CIDR = F)
xlk_hist_returns

# draw kde of returns
xlk_kde_returns <- drawKDE(random_date, xlk.r, CIDR = F)
xlk_kde_returns

# draw histogram of CIDR
xlk_hist_CIDR <- drawHistogram(random_date, xlk.r)
xlk_hist_CIDR

# draw kde of CIDR
xlk_kde_CIDR <- drawKDE(random_date, xlk.r)
xlk_kde_CIDR




## How to save the most recently shown plots
# ggsave("xlk_kde_CIDR.pdf", height = 3, width = 4)




## Explore more
drawKDE("07/24/2006", xlp.r, CIDR=T)
drawKDE("07/24/2006", xlp.r, CIDR=F)

plot(y=subset(xlp.r, Date=="07/24/2006")$ret, x=1:length(subset(xlp.r, Date=="07/24/2006")$ret))

drawHistogram("07/24/2006", xlp.r, CIDR=F)

summary(subset(xlp.r, Date=="07/24/2006")$ret)
sum(subset(xlp.r, Date=="07/24/2006")$ret)

## Pick random days from xlp
set.seed(15)

random_date <- sample(xlp.r$Date, 1)
randDateData <- subset(xlp.r, Date==random_date)

# Returns
drawKDE(random_date, xlp.r, CIDR=F)
drawHistogram(random_date, xlp.r, CIDR=F)

var(randDateData$ret)

ggplot(data=randDateData, aes(sample = ret)) +
  stat_qq() + stat_qq_line() + 
  labs(title=paste("Normal QQ Plot for Returns, ", random_date))

# CIDR
drawKDE(random_date, xlp.r, CIDR=T)
drawHistogram(random_date, xlp.r, CIDR=T)

var(randDateData$CIDR)

ggplot(data=randDateData, aes(sample =CIDR)) +
  stat_qq() + stat_qq_line() + 
  labs(title=paste("Normal QQ Plot for CIDR, ", random_date))



## Pick random days from xly
set.seed(15)

random_date <- sample(xly.r$Date, 1)
randDateData <- subset(xly.r, Date==random_date)

# Returns
drawKDE(random_date, xly.r, CIDR=F)
drawHistogram(random_date, xly.r, CIDR=F)

var(randDateData$ret)

ggplot(data=randDateData, aes(sample = ret)) +
  stat_qq() + stat_qq_line() + 
  labs(title=paste("Normal QQ Plot for Returns, ", random_date))

# CIDR
drawKDE(random_date, xly.r, CIDR=T)
drawHistogram(random_date, xly.r, CIDR=T)

var(randDateData$CIDR)

ggplot(data=randDateData, aes(sample =CIDR)) +
  stat_qq() + stat_qq_line() + 
  labs(title=paste("Normal QQ Plot for CIDR, ", random_date))




## Pick random days from xlk
set.seed(15)

random_date <- sample(xlk.r$Date, 1)
randDateData <- subset(xlk.r, Date==random_date)

# Returns
drawKDE(random_date, xlk.r, CIDR=F)
drawHistogram(random_date, xlk.r, CIDR=F)

var(randDateData$ret)

ggplot(data=randDateData, aes(sample = ret)) +
  stat_qq() + stat_qq_line() + 
  labs(title=paste("Normal QQ Plot for Returns, ", random_date))

# CIDR
drawKDE(random_date, xlk.r, CIDR=T)
drawHistogram(random_date, xlk.r, CIDR=T)

var(randDateData$CIDR)

ggplot(data=randDateData, aes(sample=CIDR)) +
  stat_qq() + stat_qq_line() + 
  labs(title=paste("Normal QQ Plot for CIDR, ", random_date))



## Explore xlp more
staples <- xlp %>% mutate(Time = Time/100) %>% filter(Time %% 10 == 0)

staplesPlot <- ggplot(data=staples)
staplesPlot + geom_histogram(aes(CIDR), bins=500)
staplesPlot + stat_qq(aes(sample=CIDR)) + stat_qq_line(aes(sample=CIDR))
