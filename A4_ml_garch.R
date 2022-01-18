### FINANCIAL ECONOMETRICS - HOMEWORK 4 - CODE
### NATHALIE MAYOR 

# loading the packages
library(ggplot2)        
library(xts)          
library(forecast)       
library(np)         
library(stats4)         
library(rugarch)        
library(L1pack)         
library(rmutil)         

# setting the working directory
current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))

# loading the data
ps4_data <- read.table("s4_data.txt",header = TRUE,sep="\t")
ps4_data$date<-as.Date.factor(ps4_data$date,format="%d/%m/%Y") #Used as.Date.factor because the date column was considered as "character"
ps4_data$date
ts_ps4_data<-xts(x=ps4_data[-1],order.by = ps4_data$date)


#### Q1 ####
negLL <- function (b0, b1, s){
  logdens = suppressWarnings (dlaplace(y= ts_ps4_data$utils, m= b0+b1*ts_ps4_data$mkt, s= s, log=TRUE))
  -sum(logdens)
}

fit.mle <- mle(negLL, start = list(b0=0, b1=0, s=1), lower=c(-Inf,-Inf,0), method ="L-BFGS-B")
summary(fit.mle)

#### Q2 ####
Box.test(ts_ps4_data$utils^2, lag=12, type="Lj")

#### Q3 ####
spec1 <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(4,0)), 
                    mean.model = list(armaOrder=c(0,0)), 
                    distribution.model = "norm")
model1 <- ugarchfit(spec = spec1, data = ts_ps4_data$utils)
model1
plot(model1, which=9) 


#### Q4 ####
newfit <- ugarchfit(spec = spec1, data = ts_ps4_data$utils, out.sample=9) 
fitfor <- ugarchforecast(newfit, n.ahead = 1, n.roll = 9,out.sample = 9)
sigma(fitfor)



#### Q5 ####
coef <- coef(newfit)
mu <- as.numeric(coef["mu"])
VaR_oosample <- 10*mu +sigma(fitfor)*sqrt(10)*qnorm(0.05)
VaR_oosample

#### Q6 ####
spec2 <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(4,0)), 
                    mean.model = list(armaOrder=c(0,0)), 
                    distribution.model = "std")
model2 <-ugarchfit(spec = spec2, data = ts_ps4_data$utils)
model2
plot(model2, which=9) 

#### Q7 ####
spec3 <- ugarchspec(variance.model = list(model="gjrGARCH",garchOrder=c(1,1)),
                    mean.model = list(armaOrder=c(0,0)),
                    distribution.model = "std")

model3 <- ugarchfit(spec = spec3, data = ts_ps4_data$utils)

max_sd <- max (sigma (ugarchfit(spec = spec3, data = ts_ps4_data$utils)))

model3
max_sd

#### Q8 ####
plot(model2, which=11) # ACF of Squared Standardized Residuals - Model 2

plot(model3, which=11) # ACF of Squared Standardized Residuals - Model 3

plot(model2, which=10) # ACF of Standardized Residuals - Model 2

plot(model3, which=10) # ACF of Standardized Residuals - Model 3



#### Q9 ####
model1 # Akaike       2.5105

model2 # Akaike       2.4685

model3 # Akaike       2.4105




