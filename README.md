# Introduction 

This repository contains R code for a Financial Econometrics (M.A.) course at UNISG.

# Specifications on Files

## [A1_statistics_review.R](https://github.com/nathaliemayor/Financial_Econometrics/blob/main/A1_statistics_review.R)

For given time series (asset prices), computation of confidence intervals and correlation matrices, Jarque-Bera test statistic and Sharpe ratio.

## [A2_regressions.R](https://github.com/nathaliemayor/Financial_Econometrics/blob/main/A2_regressions.R)

For given time series (asset prices), estimation of the market model, analysis of the residuals, testing for heteroskedasticity, testing for autocorrelation (Durbin-Watson test), computation of HAC standard errors, estimation of the CAPM in a SURE fashion and using non-HAC covariance estimator, test for joint significance, estimation of the CAPM with robust standard errors, testing the joint significance through a Chi-square test.

## [A3_predicting_asset_returns.R](https://github.com/nathaliemayor/Financial_Econometrics/blob/main/A3_predicting_asset_returns.R)

For given time series (asset prices), construction of correlograms (ACF, PACF), selection of an AR(p) process, analysis of the residuals, stationarity tests (ADF, KPSS) and Diebold-Mariano test.

## [A4_mle_garch.R](https://github.com/nathaliemayor/Financial_Econometrics/blob/main/A4_mle_garch.R)

For given time series (asset prices), estimating parameters (for the zero-mean Laplace distribution) with MLE, using the log-liklihood, testing for ARCH effects (Ljung-Box test on the squared returns). Estimation of an ARCH(4) model on asset returns, analysis of the residuals and estimation of a GJR(1,1) model (with t-distribution for the residuals). 

## [A5_bootstrapping_panel_data.R](https://github.com/nathaliemayor/Financial_Econometrics/blob/main/A5_bootstrapping_panel_data.R)

For given time series (asset prices), constructing an artificial batch of asset returns using resampled error terms drawn (with replacement) from the
residuals, (bootstrap, 10'000 times). Estimating the Fama-French model with pooled OLS (without and with robust standard errors), testing for the autocorrelation of residuals (Durbin-Watson test).


















































