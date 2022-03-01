### FINANCIAL ECONOMETRICS - HOMEWORK 3 - CODE
### NATHALIE MAYOR 

# Loading packages
library(ggplot2)        
library(xts)            
library(forecast)       
library(urca) 

# setting working directory
current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))

#### Q0 ####
ps3_data <- read.table("s3_data.txt",header = TRUE,sep="\t")
ps3_data$date<-as.Date.factor(ps3_data$date,format="%d/%m/%Y") #Used as.Date.factor because the date column was considered as "character"
ps3_data$date
ts_ps3_data<-xts(x=ps3_data[-1],order.by = ps3_data$date)
remove(ps3_data,date)
ts_ps3_data$return_SP500 <- 100*diff(log(ts_ps3_data$SP500))-ts_ps3_data$rf          
ts_ps3_data$return_ENER <- 100*diff(log(ts_ps3_data$ENER))-ts_ps3_data$rf  
ts <- ts_ps3_data['2000-01/',] 


plot(ts$return_ENER)
plot(ts$ENER)


#### Q1 ####

ts_sub <- ts
ts_sub <- ts_sub['/2008-12',]
correlogram_acf_ENER <- ggAcf(ts_sub$return_ENER,lag.max=15,calc.ci = TRUE, level = 95)+    
  ggtitle("Correlogram of ENER - ACF")+
  labs(x="Lag", y= 'Autocorrelation')+
  theme_bw()
ggsave("1.correlogram_acf_ERER.png")                                

correlogram_pacf_ENER <- ggPacf(ts_sub$return_ENER,lag.max=15,calc.ci = TRUE, level = 95)+    
  ggtitle("Correlogram of ENER - PACF")+
  labs(x="Lag", y= 'Partial Autocorrelation')+
  theme_bw()
ggsave("2.correlogram_pacf_ENER.png")

ggAcf(ts_sub$return_ENER,lag.max = 15)


#### Q2 ####
fit <- ar.mle(as.ts(ts_sub$return_ENER),aic = TRUE)  
fit_order <- fit$order  

#### Q3 ####
fit_ar_0 <- Arima(ts_sub$return_ENER,order = c(0,0,0))         
checkresiduals(fit_ar_0)      

sd(ts_sub$return_ENER)



#### Q4 ####
forecast(fit_ar_0, h=1,level = 90)                           

as.numeric( forecast(fit_ar_0, h=1,level = 90)$mean)           
as.numeric( forecast(fit_ar_0, h=1,level = 90)$lower)          
as.numeric( forecast(fit_ar_0, h=1,level = 90)$upper) 


#### Q5 ####
ts$f_ar0 <- NA                                                

for (i in (nrow(ts['/2009-01'])) : nrow(ts))                  
{
  fit_ar_0 <- Arima (ts$return_ENER[1:(i-1)], order = c(0,0,0))
  ts$f_ar0[i] <- as.numeric( forecast(fit_ar_0, h=1,level = 90)$mean)           
}

forecast_error_ar0 <- ts$return_ENER-ts$f_ar0               
RMSE_ar0 <- sqrt(mean((forecast_error_ar0)^2,na.rm=TRUE))

#### Q6 ####
ts$PDR<-log(ts$SP500/ts$SPDIV)


adf_aic <- ur.df(ts$PDR['/2018-12'], type = "drift", selectlags = "AIC")   
summary(adf_aic)

adf_bic <- ur.df(ts$PDR['/2018-12'], type = "drift", selectlags = "BIC")   
summary(adf_bic)

adf_fix <- ur.df(ts$PDR['/2018-12'], type = "drift", selectlags = "Fixed") 
summary(adf_fix)

kpss <- ur.kpss(ts$PDR['/2018-12'] ,type="mu")                             
summary(kpss)

#### Q7 ####
X <- cbind(ts$return_SP500, ts$eurusd, ts$VIX,ts$PDR, ts$return_ENER, lag(ts$return_ENER))

fit_ols <- lm(ts$return_ENER['/2008-12'] ~ lag(X['/2008-12']))

summary(fit_ols)

forecast(fit_ols, as.data.frame(X['2008-12']), level = 95)


#### Q8 ####
ts$f_ols <- NA                                                  

for (i in (nrow(ts['/2009-01'])) : nrow(ts))                    
{
  fit_ols <- lm(ts$return_ENER[1:i-1] ~ lag( X[1:(i-1)]))    
  ts$f_ols[i] <- as.numeric(forecast(fit_ols,                 
                                     as.data.frame(X[i-1]),  
                                     h =1)$mean)           
}

series <- cbind(ts$f_ols,ts$return_ENER)                    
autoplot.zoo(series,facets=NULL)+
  ggtitle("ENER: forecasted VS actual returns")+
  scale_x_date(limits = c(as.Date("2009-01-31 "), NA))+
  ylim(-15,15)+
  theme_dark()  

forecast_error_ols <- ts$return_ENER-ts$f_ols                 
RMSE_ols <- sqrt(mean((forecast_error_ols)^2,na.rm=TRUE))

#### Q9 ####
dm.test(forecast_error_ar0, forecast_error_ols,                
        alternative = "two.sided", power = 2)   

