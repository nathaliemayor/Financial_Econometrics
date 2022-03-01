### FINANCIAL ECONOMETRICS - HOMEWORK 5 - CODE
### NATHALIE MAYOR - 17342353

# Loading packages
library(ggplot2)
library(xts)
library(sandwich) 
library(lmtest) 
library(forecast)
library(boot) 
library(plm) 
library(doSNOW)
library(ggthemes)

# setting the working directory
current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))

# loading the data
ps5_data <- read.table("s5_data.txt",header = TRUE,sep="\t")
ps5_data$date<-as.Date.factor(ps5_data$date,format="%d/%m/%Y") #Used as.Date.factor because the date column was considered as "character"
ts<-xts(x=ps5_data[-1],order.by = ps5_data$date)
remove(ps5_data)

#### Q1 ####
ts$ERfund4 <- ts$fund4_qoq - ts$rf_qoq
ts <- ts['1991-03-31/']
fit <- lm(ERfund4 ~ mktrf_qoq+ smb_qoq+hml_qoq, data=ts)
summary(fit)
autoplot(as.xts(fit$res),)+
  theme_economist()

#### Q2 ####
res <- residuals(fit)
set.seed(1234)
y_sim <- fitted(fit) + sample(res,replace=T)
fit.resamp <- lm(y_sim ~ mktrf_qoq+ smb_qoq+hml_qoq, data=ts)
summary(fit.resamp)

autoplot(as.xts(fit.resamp$res),)+
  theme_economist()

#### Q3 ####
ncores <- parallel::detectCores()
ctemp <- makeCluster(ncores) 
registerDoSNOW(ctemp)
B <- 10000 #nb of replicates
get.coef <- function(x,indx){
  # x: the data to be resampled
  # idx: an index created by the boot() function in order to apply random resampling with replacement
  y_sim <- fitted(fit) + x[indx]
  return(lm(y_sim ~ ts$mktrf_qoq + ts$smb_qoq + ts$hml_qoq)$coef)
}

boot.out_p <- boot(res, get.coef, B, parallel = "multicore", ncpus = ncores)
boot.out_p #bias = (average bootstrapped coef) - (original coef)   #std. error: bootstrapped standard error

# two-sided bootstrapped p-value 
pval.alpha <- 2 * sum(boot.out_p$t[,1] < 0) / B
pval.betam <- 2 * sum(boot.out_p$t[,2] < 0) / B
pval.betas <- 2 * sum(boot.out_p$t[,3] < 0) / B
pval.betah <- 2 * sum(boot.out_p$t[,4] < 0) / B

#### Q4 ####
tsboot.out <- tsboot(res, get.coef, R=10000, sim="fixed", l=length(res)^(1/3)) 

pval.ts.alpha <- 2 * sum(tsboot.out$t[,1] < 0) / B
pval.ts.beta <- 2 * sum(tsboot.out$t[,2] < 0) / B
pval.ts.beta2 <- 2 * sum(tsboot.out$t[,3] < 0) / B
pval.ts.beta3 <- 2 * sum(tsboot.out$t[,4] < 0) / B

#### Q5 ####
data <- read.table("s5_data_panel_hw.txt",header = TRUE,sep="\t")
pdata <- pdata.frame(data,c("main_strategy","date"))
remove(data)

pdata$rx <- pdata$performance - pdata$rf
windows()
ggplot(data=as.data.frame(pdata), aes(x = main_strategy, y = rx)) + geom_boxplot()



#### Q6 ####
fit.pool <- plm(rx~mktrf+smb+hml, model="pooling", data=pdata)
summary(fit.pool)

#### Q7 ####
pdwtest(fit.pool)

#### Q8 ####
T <- length(unique(pdata$date))
m <- floor(0.75*T^(1/3)) 
coeftest(fit.pool, vcov = vcovSCC(fit.pool, maxlag = m)) #robust (Driscoll-Kraay) errors

#### Q9 ####
fit.fe<-plm(rx~mktrf+smb+hml, model="within", data=pdata)
summary(fit.fe)
coeftest(fit.fe, vcov = vcovSCC(fit.fe, maxlag = m))


#### Q10 ####
summary(fixef(fit.fe), vcov = vcovSCC(fit.fe, maxlag = m))


