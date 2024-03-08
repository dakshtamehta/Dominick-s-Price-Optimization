#####################################################################################################
# Script: rfj_Script.R
# Copyright (c) 2023 by Alan Montgomery. Distributed using license CC BY-NC 4.0
# To view this license see https://creativecommons.org/licenses/by-nc/4.0/
#
# R script to compute linear regression example using refrigated juice data (this includes OJ)
#
# Requires the following files:
#   kvrfjd.txt       weekly sales and price for each product and store
#   rfjstore.csv     store information
#   rfjdemo.csv      demographic information about stores
#
# The data included for this exercise is for internal use only and
# may not be posted or distributed further.
#####################################################################################################


# setup library
if (!require(psych)) {install.packages("psych"); library(psych)}
if (!require(hglm)) {install.packages("hglm"); library(hglm)}


#####################################################################################################
# import and transform the data
#####################################################################################################

# import data
#setwd("~/Documents/class/marketing analytics/cases/dominicks/data")

# read in the data
rfjdata = read.table(file="/Users/mehta/Documents/CMU/Mini 3/AM/Dominicks/kvrfjd.txt",header=TRUE)
rfjdemo=read.csv(file="/Users/mehta/Documents/CMU/Mini 3/AM/Dominicks/rfjdemo.csv")    # demographic information about stores
rfjdate=read.csv(file="/Users/mehta/Documents/CMU/Mini 3/AM/Dominicks/rfjdate.csv")    # dates for each week
rfjstore=read.csv(file="/Users/mehta/Documents/CMU/Mini 3/AM/Dominicks/rfjstore.csv")  # store information
summary(rfjdata)

# create new data
storeidx = rfjdata[["store"]]

# create a list of store indices
stores = unique(storeidx)  # returns the unique store numbers (eliminates duplicates)
nstores = length(stores)

# remove blank stores
rfjdemo=rfjdemo[!is.na(rfjdemo$store),]

# recode store and upc as factors
rfjdata$store=as.factor(rfjdata$store)
rfjdemo$store=as.factor(rfjdemo$store)
rfjstore$store=as.factor(rfjstore$store)

# set the random number seed to the samples will be the same when re-run
nobs=nrow(rfjdata)
set.seed(1234)  # save the seed so we can recreate sample
sample = sample.int(2, size=nobs, prob=c(.6,.4), replace=TRUE)  # randomly split data into 2 groups
trainsample=(sample==1)     # put 60% of the observations in the training sample
validsample=(sample==2)     # put the other 40% in the validation sample (compare models)



#####################################################################################################
# simple model
#####################################################################################################

## log linear model (pooled model)
( mdl0=lm(lmv1~lpr1,data=rfjdata[trainsample,]) )

# plot all data
plot(lmv1~lpr1,data=rfjdata)
# overlay regression line (adjust the intercept by the average effect of the non-price variables)
abline(mdl0)

# just plot data for one store
plot(lmv1~lpr1,data=rfjdata[which(rfjdata$store==2),])
# overlay regression line (adjust the intercept by the average effect of the non-price variables)
abline(mdl0)

# predict the validation sample using the model
pred0=predict(mdl0,newdata=rfjdata,type='response')      # compute the predictions using the previous estaimtes
err0=(rfjdata$lmv1-pred0)      # compute the error = actual - predicted
sqerr0=(err0^2)            # compute the square of the error = error*error
abserr0=abs(err0)          # compute the absolute error = abs(error)
describeBy(cbind(err0,sqerr0,abserr0),group=sample,fast=TRUE)   # summarize the various measures of errors for the training and validation samples



#####################################################################################################
# pooled model
#####################################################################################################

## log linear model (pooled model)
( mdl1=lm(lmv1~lpr1+lpr4+lpr5+lpr11+afeat1+adisp1,data=rfjdata[trainsample,]) )

# plot all data
plot(lmv1~lpr1,data=rfjdata)
# overlay regression line (adjust the intercept by the average effect of the non-price variables)
abline(a=mdl1$coefficients["(Intercept)"]
        +mean(rfjdata$lpr4)*mdl1$coefficients["lpr4"]
        +mean(rfjdata$lpr5)*mdl1$coefficients["lpr5"]
        +mean(rfjdata$lpr11)*mdl1$coefficients["lpr11"]
        +mean(rfjdata$afeat1)*mdl1$coefficients["afeat1"]
        +mean(rfjdata$adisp1)*mdl1$coefficients["adisp1"],
       b=mdl1$coefficients["lpr1"])

# just plot data for one store
plot(lmv1~lpr1,data=rfjdata[which(rfjdata$store==2),])
# overlay regression line (adjust the intercept by the average effect of the non-price variables)
abline(a=mdl1$coefficients["(Intercept)"]
       +mean(rfjdata$lpr4)*mdl1$coefficients["lpr4"]
       +mean(rfjdata$lpr5)*mdl1$coefficients["lpr5"]
       +mean(rfjdata$lpr11)*mdl1$coefficients["lpr11"]
       +mean(rfjdata$afeat1)*mdl1$coefficients["afeat1"]
       +mean(rfjdata$adisp1)*mdl1$coefficients["adisp1"],
       b=mdl1$coefficients["lpr1"])

# predict the validation sample using the model
pred1=predict(mdl1,newdata=rfjdata,type='response')      # compute the predictions using the previous estaimtes
err1=(rfjdata$lmv1-pred1)      # compute the error = actual - predicted
sqerr1=(err1^2)            # compute the square of the error = error*error
abserr1=abs(err1)          # compute the absolute error = abs(error)
describeBy(cbind(err1,sqerr1,abserr1),group=sample,fast=TRUE)   # summarize the various measures of errors for the training and validation samples



#####################################################################################################
# store model
#####################################################################################################

## log linear model (store model)
# notice since store is a factor it will include store specific intercepts
#        store*lprice means to include a separate lprice coefficient for each store
( mdl2=lm(lmv1~store+store*lpr1+store*lpr4+store*lpr5+store*lpr11+store*afeat1+store*adisp1,data=rfjdata[trainsample,]) )

# plot the results
plot(lmv1~lpr1,data=rfjdata[which(rfjdata$store==5),])
# overlay regression line (adjust the intercept by the average effect of the non-price variables)
abline(a=mdl2$coefficients["(Intercept)"]
       +mdl2$coefficients["store5"]
       +mean(rfjdata$lpr4)*(mdl2$coefficients["lpr4"]+mdl2$coefficients["store5:lpr4"])
       +mean(rfjdata$lpr5)*(mdl2$coefficients["lpr5"]+mdl2$coefficients["store5:lpr5"])
       +mean(rfjdata$lpr11)*(mdl2$coefficients["lpr11"]+mdl2$coefficients["store5:lpr11"])
       +mean(rfjdata$afeat1)*(mdl2$coefficients["afeat1"]+mdl2$coefficients["store5:afeat1"])
       +mean(rfjdata$adisp1)*(mdl2$coefficients["adisp1"]+mdl2$coefficients["store5:adisp1"]),
       b=mdl2$coefficients["lpr1"]+mdl2$coefficients["store5:lpr1"])
# overlay regression line from model 1 (adjust the intercept by the average effect of the non-price variables)
abline(a=mdl1$coefficients["(Intercept)"]
       +mean(rfjdata$lpr4)*mdl1$coefficients["lpr4"]
       +mean(rfjdata$lpr5)*mdl1$coefficients["lpr5"]
       +mean(rfjdata$lpr11)*mdl1$coefficients["lpr11"]
       +mean(rfjdata$afeat1)*mdl1$coefficients["afeat1"]
       +mean(rfjdata$adisp1)*mdl1$coefficients["adisp1"],
       b=mdl1$coefficients["lpr1"],col="blue")

# predict the validation sample using the model
pred2=predict(mdl2,newdata=rfjdata,type='response')      # compute the predictions using the previous estaimtes
err2=(rfjdata$lmv1-pred2)    # compute the error = actual - predicted
sqerr2=(err2^2)            # compute the square of the error = error*error
abserr2=abs(err2)          # compute the absolute error = abs(error)
describeBy(cbind(err2,sqerr2,abserr2),group=sample,fast=TRUE)   # summarize the various measures of errors for the training and validation samples

# save the output of the coefficients as a matrix
# we have to extract the store and store*lprice effects and organize them as a matrix so it is easier to deal with
cstoren=as.character(unique(rfjdata$store))   # vector of store numbers
cstore=paste0("store",cstoren)  # create character vector with all store names and store numbers (notice store2 is base level and is dropped)
# first extract the common chain wide effect
mdl2.int=mdl2$coefficients["(Intercept)"]
mdl2.lprice=mdl2$coefficients["lpr1"]
mdl2.feat=mdl2$coefficients["afeat1"]
mdl2.disp=mdl2$coefficients["adisp1"]
# next extract the store effects
mdl2.store.int=mdl2$coefficients[cstore]  # looks for all stores
mdl2.store.lprice=mdl2$coefficients[paste0(cstore,":lpr1")]
# combine them together
mdl2.parm=cbind(mdl2.int,mdl2.lprice,mdl2.feat,mdl2.disp,mdl2.store.int,mdl2.store.lprice)
write.csv("mdl2.parm")  # you can import this into excel



#####################################################################################################
# mixed model
#####################################################################################################

## hierarchical model in which the intercept and lprice variable is allowed to change
( mdl3=hglm2(meanmodel = lmv1~lpr1+(1|store)+(lpr1|store)+lpr4+lpr5+lpr11+afeat1+adisp1,data=rfjdata[trainsample,]) )
# extract the coefficients for store #5
fcoef=mdl3$fixef
rcoef=mdl3$ranef[c("(Intercept)| store:5","lprice| store:5")]



#####################################################################################################
# extract the wholesale costs from week #93
#####################################################################################################

# find the wholesale costs for a week
iweek=93
profm1=rfjdata$profm1[rfjdata$store==2 & rfjdata$week==iweek]

# find the retail prices actually charged for a week
price1=exp(rfjdata$lpr1[rfjdata$store==2 & rfjdata$week==iweek])*64

# impute the cost from the wholesale cost
cost1=price1*(1-profm1)


