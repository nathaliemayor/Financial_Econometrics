### FINANCIAL ECONOMETRICS - HMW2
### Nathalie Mayor

# installing packages
install.packages("dygraphs")
install.packages("systemfit")
# loading packages
library(xts)
library(tseries)
library(ggplot2)
library(dygraphs) 
library(forecast) 
library(lmtest) 
library(sandwich) 
library(systemfit)



## set the working directory
current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))

#### Q0 ####

## import the data
s2_data <- read.table("s2_data.txt",header = TRUE,sep= "\t")
class(s2_data$date)


## transform s1_data as time series object
s2_data$date<-as.Date.factor(s2_data$date,format="%d/%m/%Y") #Used as.Date.factor because the date column was considered as "character"
s2_data$date
ts_s2_data<-xts(x=s2_data[-1],order.by = s2_data$date)
remove(s2_data,date)
start(ts_s2_data)
end(ts_s2_data)

## excess returns of HF indices
ts_s2_data$fi<-100*(ts_s2_data$FI/lag(ts_s2_data$FI)-1)-ts_s2_data$rf
ts_s2_data$dist<-100*(ts_s2_data$DIST/lag(ts_s2_data$DIST)-1)-ts_s2_data$rf
ts_s2_data$mac<-100*(ts_s2_data$MAC/lag(ts_s2_data$MAC)-1)-ts_s2_data$rf
ts_s2_data$mult<-100*(ts_s2_data$MULT/lag(ts_s2_data$MULT)-1)-ts_s2_data$rf
ts_s2_data$els<-100*(ts_s2_data$ELS/lag(ts_s2_data$ELS)-1)-ts_s2_data$rf

# removing the 1st row
ts_s2_data<-ts_s2_data[-1,]

#### Q1 ####
# alpha: 0.28047, beta: 0.19285, R^2: 0.3798, alpha statistically significant at 5%
fit_mult <- lm(mult ~ mktrf, data=ts_s2_data)
summary(fit_mult)

#### Q2 ####
alpha_mult<-fit_mult$coefficients[1]
beta_mult<-fit_mult$coefficients[2]
residuals_fit_mult<-as.xts(resid(fit_mult))
nobs<-length(residuals_fit_mult)
ncoef<-length(coef(fit_mult))
dv<-ts_s2_data$mult
y_hat<-fitted(fit_mult)
y_bar<-mean(dv)
rss<-sum(residuals_fit_mult^2)
tss<-sum((dv-y_bar)^2)

scatter_multmkt<-ggplot(data=ts_s2_data,                               
                     mapping = aes(x = mktrf, y = mult) )+
  ggtitle("Regression of MULT on Market")+
  geom_point(col= "dodgerblue2", size = 0.3)+
  geom_vline(xintercept = 0, col="gray2")+
  geom_hline(yintercept = 0, col="gray2")+
  geom_smooth(method="lm", col='dimgray', size =0.5)+
  labs(x="Market return", y= 'Fixed Income Arbitrage return')+
  theme_bw() 
ggsave("q2.png")  

residuals_mult_timeline1 <- autoplot.zoo(residuals_fit_mult)+                                          
  ggtitle("Residuals")+
  labs(x="Years", y= 'Residuals')+
  theme_bw()
ggsave("q2.2.png")                                 

residuals_mult_timeline2 <- autoplot.zoo(residuals_fit_mult^2)+                                          
  ggtitle("Residuals squared")+
  labs(x="Years", y= 'Residuals')+
  theme_bw()
ggsave("q2.3.png")                                

bptest(fit_mult)                                                 

bptest(fit_mult, ~ mktrf + I(mktrf^2), data = ts_s2_data)   


#### Q3 ####
correlogram_residuals_mult <- ggAcf (residuals_fit_mult, lag.max = 10)+                   
  ggtitle("Correlogram of Residuals")+           
  theme_bw()
ggsave("q3.pdf")                                

dwtest(fit_mult)      


#### Q5 ####
lag1 <- floor(0.75*nobs^(1/3)) 

nobs^(1/4)

fit1 <- coeftest(fit_mult, 
                 vcov = NeweyWest(fit_mult, lag=lag1, prewhite = FALSE))    

#### Q6 ####
r_mac <- ts_s2_data$mac ~ ts_s2_data$mktrf                               
r_mult <- ts_s2_data$mult ~ ts_s2_data$mktrf                             
r_els <- ts_s2_data$els ~ ts_s2_data$mktrf                             

system <- list(MACreg = r_mac,                                
                 MULTreg = r_mult, ELSreg= r_els)

fitsure <- systemfit(system, method = "SUR")               


summary(fitsure)  

#### Q7 ####
restriction <- c("MACreg_(Intercept)","MULTreg_(Intercept)", 
                 "ELSreg_(Intercept)") 
linearHypothesis(fitsure, restriction, test = "Chisq")      

qchisq(0.95, df = 3)                                        



#### Q8 ####
ts_s2_new <- ts_s2_data                                                 
ts_s2_new <- ts_s2_new['2009-07/',]                               

fitnew <- lm(data=ts_s2_new, mult ~ mktrf)   

bptest(fitnew, ~ mktrf + I(mktrf^2), data = ts_s2_new)           

dwtest(fitnew)                                  

# Result: DW = 2.12, p-value = 0.76                

res_new <- as.xts(resid(fitnew))                           
T_sub <- length(res_new)                                   
m_sub <- floor(0.75*T_sub^(1/3))                          
fit_rob <- coeftest(fitnew,                                   
                    vcov = NeweyWest(fitnew, lag= m_sub, 
                                     prewhite = FALSE))                               

# Std. Error  (beta) = 0.024822
# coefficient (beta)=  0.196804 

coefbeta<-fit_rob[2]
sebeta<-fit_rob[4]

(coefbeta-0.21)/sebeta

# critical value for 5% level: 1.96

#### Q9 ####

# CAPM

fit_fi_capm <- lm(data=ts_s2_data, fi ~ mktrf)             
bptest(fit_fi_capm, ~ mktrf + I(mktrf^2), data = ts_s2_data)      
dwtest(fit_fi_capm)                                       

res_fi_capm <- as.xts(resid(fit_fi_capm))               
T_fi        <- length(res_fi_capm)                      
m_fi_capm <- 4     

0.75*nobs^0.3333-1

fit.rob_fi_capm <- coeftest(fit_fi_capm,                  
                             vcov = NeweyWest(fit_fi_capm, 
                                              lag=m_fi_capm,prewhite = FALSE))                               

# FAMA-FRENCH:

fit_fi_ff <- lm (data=ts_s2_data, fi ~ mktrf+smb+hml+rmw+cma)    

bptest(fit_fi_ff, ~   mktrf + smb+hml+rmw+cma+                     
         I(mktrf^2)+I(smb^2)+I(hml^2)+I(rmw^2)+I(cma^2)+
         mktrf*smb+mktrf*hml+mktrf*rmw+mktrf*cma+
         smb* hml+smb* rmw+smb* cma+
         hml*rmw+ hml*cma+
         rmw*cma
       ,data = ts_s2_data)       

dwtest(fit_fi_ff)                                         

m_fi_ff <- 4                                                       

fit.rob_fi_ff <- coeftest(fit_fi_ff, 
                           vcov = NeweyWest(fit_fi_ff,
                                            lag=m_fi_ff,prewhite = FALSE))

#### Q10 ####
r1_ff <- ts_s2_data$fi   ~ ts_s2_data$mktrf +ts_s2_data$smb +ts_s2_data$hml +ts_s2_data$rmw +ts_s2_data$cma  
r2_ff <- ts_s2_data$mult ~ ts_s2_data$mktrf +ts_s2_data$smb +ts_s2_data$hml +ts_s2_data$rmw +ts_s2_data$cma  
r3_ff <- ts_s2_data$els  ~ ts_s2_data$mktrf +ts_s2_data$smb +ts_s2_data$hml +ts_s2_data$rmw +ts_s2_data$cma  

eqSystem <- list(FIregff = r1_ff, MULTregff = r2_ff, ELSregff = r3_ff)

fitsur_ff <- systemfit(eqSystem, "SUR")
summary(fitsur_ff)

summary(lm(r1_ff))                                                
summary(lm(r2_ff))                                                  
summary(lm(r3_ff))                                                  


#### Q11 ####

restrictionff <- c("FIregff_(Intercept)","MULTregff_(Intercept)",    
                   "ELSregff_(Intercept)") 

linearHypothesis (fitsur_ff, restrictionff, test = "Chisq")          

qchisq(0.95, df = 3)                                                 
















































