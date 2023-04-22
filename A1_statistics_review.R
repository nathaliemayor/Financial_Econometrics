#' FINANCIAL ECONOMETRICS - HMW1 
#' @author  Nathalie Mayor      

# loading packages
library(xts)
library(tseries)
library(tidyverse)
library(rio)

# -----------------------------------------
# Question 1
# -----------------------------------------

# import the data
s1_data <- import("data/s1_data.txt")

# transform data as time series object
s1_data <- s1_data %>% 
  mutate(date = 
           as.Date.factor(
             s1_data$date,
             format="%d/%m/%Y"
           )
  )

ts_s1_data< - xts(
  x=s1_data[,-1],
  order.by = s1_data[,"date"]
  )

# -----------------------------------------
# Question 2
# -----------------------------------------
ts_s1_data[,"sp500"] <- 100*diff(log(ts_s1_data[,"SP500"]))
ts_s1_data[,"ukx"] <- 100*diff(log(ts_s1_data[,"UKX"]))
ts_s1_data[,"btcusd"]<-100*diff(log(ts_s1_data[,"BTCUSD"]))
ts_s1_data[,"cl1"]<-100*diff(log(ts_s1_data[,"CL1"]))
ts_s1_data[,"mxef"] <- 100*diff(log(ts_s1_data[,"MXEF"]))

# convert rf to monthly values
ts_s1_data[,"RF"] <- ts_s1_data[,"USRF"]/12

# subsample
ts_s1_data <- ts_s1_data['2011-01/']

# mean log returns for UKX and MXEF
mean(ts_s1_data[,"ukx"], na.rm = TRUE)
mean(ts_s1_data[,"mxef"], na.rm = TRUE)

# -----------------------------------------
# Question 3
# -----------------------------------------
autoplot(
  ts_s1_data$BTCUSD,
  main="BTCUSD spot rate") +
  labs(x="Time (year)", 
       y="Index value")
autoplot(
  ts_s1_data$btcusd,
  main="BTCUSD monthly log returns") +
  labs(x="Time (year)", 
       y="monthly log returns")

# -----------------------------------------
# Question 4
# -----------------------------------------
t.test(
  as.vector(
    ts_s1_data[,"mxef"]), 
       alternative="two.sided",
       mu=0,
  conf.level=0.99
  )
# mean is not different from 0

# -----------------------------------------
# Question 5
# -----------------------------------------
t.test(
  as.vector(ts_s1_data$btcusd), 
  alternative="two.sided",
       mu=0,
  conf.level=0.99)

diffBTCUSDRF<-ts_s1_data[,"btcusd"] - ts_s1_data[,"RF"]

t.test(
  as.vector(diffBTCUSDRF),
  alternative="two.sided",
       conf.level=0.99
  )

# -----------------------------------------
# Question 6
# -----------------------------------------
ReturnMatrix <- cbind(
  ts_s1_data[,"sp500"],
  ts_s1_data[,"ukx"],
  ts_s1_data[,"btcusd"],
  ts_s1_data[,"cl1"],
  ts_s1_data[,"mxef"]
  )

cor_rm <- cor(
  ReturnMatrix %>% 
                na.omit()
  )

# CL1 most correlated to btcusd
# SP most correlated to ukx

# -----------------------------------------
# Question 7
# -----------------------------------------
basicStats(ReturnMatrix)

# highest kurtosis is sp500 17.234
sp<-ts_s1_data[,"sp500"]
m<-mean(sp, na.rm = TRUE)
sdsp<-sd(sp, na.rm = TRUE)

qplot(
  x = sp500, 
  data = ts_s1_data, 
  geom = "blank",
  xlim=c(-20,20),
  main="SP500 returns") +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, 
                args = c(mean = m, sd = sdsp), 
                col = "red")

qqnorm(sp)
qqnormPlot(sp)

# -----------------------------------------
# Question 8
# -----------------------------------------
jarque.bera.test(ts_s1_data$sp500)
jarque.bera.test(ts_s1_data$ukx)
jarque.bera.test(ts_s1_data$btcusd)
jarque.bera.test(ts_s1_data$cl1)
jarque.bera.test(ts_s1_data$mxef)

# sp500 is the farthest from normal with stat 1213

# -----------------------------------------
# Question 9
# -----------------------------------------
mean(rowMeans(ReturnMatrix))

SR <- mean(
  rowMeans(ReturnMatrix)-ts_s1_data$RF)/sd(rowMeans(ReturnMatrix)
                                           )

# -----------------------------------------
# Question 10
# -----------------------------------------
t.test(
  rowMeans(ReturnMatrix), 
  alternative = "two.sided",
       mu = 0,
  conf.level = 0.95)

# -----------------------------------------
# Question 11
# -----------------------------------------
full_ts_s1_data <- xts(x=s1_data[-1],order.by = s1_data$date)
remove(s1_data,date)

# a
s1_potus <- import("potus_by_party.txt")
s1_potus$date<-as.Date.factor(s1_potus$date,format="%Y-%m-%d")
ts_potus<-xts(x=s1_potus[-1],order.by = s1_potus$date)
full_ts_s1_data<-cbind(full_ts_s1_data,ts_potus)

# b
full_ts_s1_data
SP_with_div<-full_ts_s1_data$SP500+na.omit(lag(full_ts_s1_data$SPDIV/12))
full_ts_s1_data$SP_tot_return<-SP_with_div/na.omit(lag(full_ts_s1_data$SP500))-1
sd(na.omit(SP_with_div/lag(full_ts_s1_data$SP500))-1)

# c
mean(na.omit(full_ts_s1_data[full_ts_s1_data$party==1]$SP_tot_return))
quantile(full_ts_s1_data[full_ts_s1_data$party==1]$SP_tot_return,na.rm = TRUE)
IQR(full_ts_s1_data[full_ts_s1_data$party==1]$SP_tot_return,na.rm = TRUE)


# d
mean(na.omit(full_ts_s1_data[full_ts_s1_data$party==0]$SP_tot_return))
quantile(full_ts_s1_data[full_ts_s1_data$party==0]$SP_tot_return,na.rm = TRUE)
IQR(full_ts_s1_data[full_ts_s1_data$party==0]$SP_tot_return,na.rm = TRUE)

# e
republican <- na.omit(full_ts_s1_data[full_ts_s1_data$party==0]$SP_tot_return)
democrat <- na.omit(full_ts_s1_data[full_ts_s1_data$party==1]$SP_tot_return)

t.test(
  as.vector(democrat),
  as.vector(republican), 
  alternative="two.sided",
       mu=0,
  conf.level=0.90
  )









