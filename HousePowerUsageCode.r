---
title: "Houston Power Usage EDA and Forecasting Part of Texas Feb 2021 Snowstorm and Power Outage Investigation"
author: "Balaubramaniam Dashinamoorthi"
date: "3/28/2021"
output:
pdf_document: default
html_document: default
---
# Introduction
# Data preparation
library(tswge)
library(smooth)
library(timeSeries)
library(dplyr)
library(ggplot2)
library(forecast)
installed.packages("orcutt")
library(orcutt)
library(tseries)
library(tidyverse)
library(GGally)
library(astsa)
library(vars)
library(nnfor)
library(ts)
#dev.off()
#Read the consolidated Houston PowerUsage per day and Weather data which has 1499 records including header
df=read.csv('C:/Timeseries/HoustonPowerWeather.csv')
head(df)
dim(df)#1498 22
#tail(df)#1498
plotts.sample.wge(df$PowerKWH)
aic5.wge(df$PowerKWH,p=1:10,q=1:3,type="aic")#getting p=10,q=1
#dev.off()
fit = lm(df$PowerKWH~df$Dew_max+df$Temp_max+df$Hum_max+df$Wind_max+df$Press_max+df$Precipit, data=df)
estFull = est.arma.wge(fit$residuals, p = 10, q = 1)
AIC(fit) #sliced data-from 01-06-2016 to 12-31-2019-4 years
aic.wge(fit$residuals,p=1:10,q=1:3)  # AIC picks full data p=5,q=1
#Forecasting Last data was 7-7-2020, from 7-7-20 until Feb 21 2020 229 days
# Using AIC EST2 To Forecast ASE which is p=10 and q=1-from 01-06-2016 to 12-31-2019-4 years
forecastFull=fore.aruma.wge(df3$df.PowerKWH,phi=estFull$phi,theta=estFull$theta,n.ahead=229,limits=FALSE,lastn=FALSE,plot=TRUE)
ASE = mean((df$PowerKWH - forecastFull$f )^2)
ASE

diff=artrans.wge(df$PowerKWH, c(rep(0,364),1))
plotts.sample.wge(diff)
aic5.wge(diff)#both favors p=2,q=1
aic5.wge(diff,type="bic")#both favors p=2,q=1
#est=est.arma.wge(diff,p=1,q=1)
est=est.arma.wge(diff,p=2,q=1)
#diff2=artrans.wge(diff,phi.tr=est$theta)
plotts.sample.wge(est$res)
forecastD=fore.aruma.wge(df$PowerKWH,s=365,phi=est$phi,theta=est$theta,n.ahead=229,limits=FALSE,lastn=TRUE,plot=TRUE)

ASE = mean((df$PowerKWH[1270:1498] - forecastD$f )^2)
ASE