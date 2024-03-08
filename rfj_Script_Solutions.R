###############################################################################
### Script: rfj_Script_Solutions.R
### Version: 1.0
### Copyright (c) 2024 by Alan Montgomery. Distributed using license CC BY-NC 4.0
###   To view this license see https://creativecommons.org/licenses/by-nc/4.0/
### Notes:
###   R script to compute linear regression example using refrigated juice data (this includes OJ)
### Input Files:
###   kvrfjd.txt                   txt dataset
###   rfjdemo.xlsx                 Excel dataset
###   rfjdate.xlsx                 Excel dataset
###   rfjstore.xlsx                Excel dataset
### Output Files:
###   mdl2_parm.csv                csv dataset
### Variables:
###   df_rfjdata                   Main dataset (dataframe)
###   df_rfjdemo                   Demographic dataset (dataframe)
###   df_rfjdate                   Date dataset (dataframe)
###   df_rfjstore                  Information dataset (dataframe)
###   lvec_trainsample             Training set (logical vector)
###   lvec_validsample             Validtaion set (logical vector)
###############################################################################



###############################################################################
### setup the environment
###############################################################################

# setup library
if (!require(psych)) {install.packages("psych"); library(psych)}
if (!require(hglm)) {install.packages("hglm"); library(hglm)}
if (!require(lme4)) {install.packages("lme4"); library(lme4)}
if (!require(openxlsx)) {install.packages("openxlsx"); library(openxlsx)}
if (!require(vioplot)) {install.packages("vioplot"); library(vioplot)}



###############################################################################
### import and transform the data
###############################################################################

# setup your working directory !! replace with correct location !!
setwd("~/Documents/class/analytical marketing/cases/dominicks/data/")

# read in the data
df_rfjdata <- read.xlsx("dom_rfj_data.xlsx",sheet="kvrfjd") # transaction data
df_rfjdemo <- read.xlsx("dom_rfj_data.xlsx", sheet="rfjdemo") # demographics for stores
df_rfjdate <- read.xlsx("dom_rfj_data.xlsx", sheet="rfjdate") # dates for each week
df_rfjstore <- read.xlsx("dom_rfj_data.xlsx", sheet="rfjstore") # store information
summary(df_rfjdata)   

# create new data
storeidx <- df_rfjdata[["store"]]

# create a list of store indices
stores <- unique(storeidx) # returns the unique store numbers (eliminates duplicates)
nstores <- length(stores)

# remove blank stores
df_rfjdemo <- df_rfjdemo[!is.na(df_rfjdemo$store), ]

# recode store and upc as factors
df_rfjdata$store <- as.factor(df_rfjdata$store)
df_rfjdemo$store <- as.factor(df_rfjdemo$store)
df_rfjstore$store <- as.factor(df_rfjstore$store)

# set the random number seed to the samples will be the same when re-run
nobs <- nrow(df_rfjdata)
set.seed(1234) # save the seed so we can recreate sample
sample <- sample.int(2, size = nobs, prob = c(.6, .4), replace = TRUE) # randomly split data into 2 groups
lvec_trainsample <- (sample == 1) # put 60% of the observations in the training sample
lvec_validsample <- (sample == 2) # put the other 40% in the validation sample (compare models)



###############################################################################
### estimate many different models
###############################################################################

# log model (pooled model not feat+disp)
(mdl0 <- lm(lmv1 ~ lpr1, data = df_rfjdata[lvec_trainsample, ]))

# log linear model (pooled model)
(mdl1 <- lm(lmv1 ~ lpr1 + afeat1 + adisp1, data = df_rfjdata[lvec_trainsample, ]))

# log linear model (store model)
(mdl2 <- lm(lmv1 ~ store + store * lpr1 + afeat1 + adisp1, data = df_rfjdata[lvec_trainsample, ]))

# log linear model (store model)
(mdl3 <- lm(lmv1 ~ store + store * lpr1 + store * afeat1 + store * adisp1, data = df_rfjdata[lvec_trainsample, ]))

# log linear model (store model)
(mdl4 <- lm(lmv1 ~ store + store * lpr1 + store * afeat1 + store * adisp1
  + lpr1 + lpr2 + lpr3 + lpr4 + lpr5 + lpr6 + lpr7 + lpr8 + lpr9 + lpr10 + lpr11 + lpr12, data = df_rfjdata[lvec_trainsample, ]))

# log linear model (store model)
(mdl5 <- lm(
  lmv1 ~ store + store * lpr1 + store * afeat1 + store * adisp1
    + store * lpr1 + store * lpr2 + store * lpr3 + store * lpr4 + store * lpr5 + store * lpr6
    + store * lpr7 + store * lpr8 + store * lpr9 + store * lpr10 + store * lpr11 + store * lpr12,
  data = df_rfjdata[lvec_trainsample, ]
))

# log linear model (store model)
(mdl6 <- lm(
  lmv1 ~ store + store * lpr1 + store * afeat1 + store * adisp1
    + store * lpr1 + store * lpr2 + store * lpr3 + store * lpr4 + store * lpr5 + store * lpr6
    + store * lpr7 + store * lpr8 + store * lpr9 + store * lpr10 + store * lpr11 + store * lpr12
    + store * afeat1 + store * afeat2 + store * afeat3 + store * afeat4 + store * afeat5 + store * afeat6
    + store * afeat7 + store * afeat8 + store * afeat9 + store * afeat10 + store * afeat11 + store * afeat12
    + store * adisp1 + store * adisp2 + store * adisp3 + store * adisp4 + store * adisp5 + store * adisp6
    + store * adisp7 + store * adisp8 + store * adisp9 + store * adisp10 + store * adisp11 + store * adisp12,
  data = df_rfjdata[lvec_trainsample, ]
))

# hierarchical model
(hmdl4 <- hglm2(meanmodel = lmv1 ~ lpr1 + lpr2 + lpr3 + lpr4 + lpr5 + lpr6 + lpr7 + lpr8 + lpr9 + lpr10 + lpr11 + lpr12
  + (1 | store) + (lpr1 | store) + (afeat1 | store) + (adisp1 | store), data = df_rfjdata[lvec_trainsample, ]))

(hmdl4 <- lmer(lmv1 ~ lpr1 + lpr2 + lpr3 + lpr4 + lpr5 + lpr6 + lpr7 + lpr8 + lpr9 + lpr10 + lpr11 + lpr12
  + (1 | store) + (lpr1 | store) + (afeat1 | store) + (adisp1 | store), data = df_rfjdata[lvec_trainsample, ]))

# predict the validation sample using the model
summarymdl <- function(mdl, df_rfjdata, sample) {
  pred <- predict(mdl, newdata = df_rfjdata, type = "response") # compute the predictions using the previous estaimtes
  err <- (df_rfjdata$lmv1 - pred) # compute the error = actual - predicted
  sqerr <- (err^2) # compute the square of the error = error*error
  abserr <- abs(err) # compute the absolute error = abs(error)
  # compute Mean(Error), MAE, MSE by each group
  meanerror <- aggregate(err, by = list(sample = sample), mean)
  mae <- aggregate(sqerr, by = list(sample = sample), mean)
  mse <- aggregate(abserr, by = list(sample = sample), mean)
  rmse <- sqrt(mse)
  mst <- aggregate(df_rfjdata$lmv1, by = list(sample = sample), var)
  rsquare <- 1 - mse / mst
  # return as a matrix
  return(cbind(meanerr = meanerror[, 2], mae = mae[, 2], mse = mse[, 2], rmse = rmse[, 2], rsquare = rsquare[, 2]))
}

# summarize the models
(sum0 <- summarymdl(mdl0, df_rfjdata, sample))
(sum1 <- summarymdl(mdl1, df_rfjdata, sample))
(sum2 <- summarymdl(mdl2, df_rfjdata, sample))
(sum3 <- summarymdl(mdl3, df_rfjdata, sample))
(sum4 <- summarymdl(mdl4, df_rfjdata, sample))
(sum5 <- summarymdl(mdl5, df_rfjdata, sample))
(sum6 <- summarymdl(mdl6, df_rfjdata, sample))
(hsum4 <- summarymdl(hmdl4, df_rfjdata, sample))

# extract the statistics for the holdout sample
results <- rbind(sum0[2, ], sum1[2, ], sum2[2, ], sum3[2, ], sum4[2, ], sum5[2, ], sum6[2, ])

# extract the R^2 statistics for the training data
fitr <- c(sum0[1, 5], sum1[1, 5], sum2[1, 5], sum3[1, 5], sum4[1, 5], sum5[1, 5], sum6[1, 5])



###############################################################################
### summarize price elasticities
###############################################################################

# we have to extract the store and store*lprice effects and organize them as a matrix so it is easier to deal with
cstoren <- as.character(unique(df_rfjdata$store)) # vector of store numbers
cstore <- paste0("store", cstoren) # create character vector with all store names and store numbers (notice store2 is base level and is dropped)

# extract common effect
pelas0 <- coefficients(mdl0)["lpr1"]
pelas1 <- coefficients(mdl1)["lpr1"]
pelas2 <- coefficients(mdl2)["lpr1"]
pelas3 <- coefficients(mdl3)["lpr1"]
pelas4 <- coefficients(mdl4)["lpr1"]
pelas5 <- coefficients(mdl5)["lpr1"]
pelas6 <- coefficients(mdl6)["lpr1"]

# extract store effects
pelas2.store <- mdl2$coefficients[paste0(cstore, ":lpr1")]
names(pelas2.store)[1] <- "store2:lpr1"
pelas2.store[1] <- 0
pelas2.store <- pelas2.store + pelas2
pelas3.store <- mdl3$coefficients[paste0(cstore, ":lpr1")]
names(pelas3.store)[1] <- "store2:lpr1"
pelas3.store[1] <- 0
pelas3.store <- pelas3.store + pelas3
pelas4.store <- mdl4$coefficients[paste0(cstore, ":lpr1")]
names(pelas4.store)[1] <- "store2:lpr1"
pelas4.store[1] <- 0
pelas4.store <- pelas4.store + pelas4
pelas5.store <- mdl5$coefficients[paste0(cstore, ":lpr1")]
names(pelas5.store)[1] <- "store2:lpr1"
pelas5.store[1] <- 0
pelas5.store <- pelas5.store + pelas5
pelas6.store <- mdl6$coefficients[paste0(cstore, ":lpr1")]
names(pelas6.store)[1] <- "store2:lpr1"
pelas6.store[1] <- 0
pelas6.store <- pelas6.store + pelas6

# plot the histogram of elasticities
par(mfrow = c(1, 7), oma = c(0, 0, 5, 0))
boxplot(rep(pelas0, 86), xlab = "mdl0", ylim = c(0, -6))
boxplot(rep(pelas1, 86), xlab = "mdl1", ylim = c(0, -6))
boxplot(pelas2.store, xlab = "mdl2", ylim = c(0, -6))
boxplot(pelas3.store, xlab = "mdl3", ylim = c(0, -6))
boxplot(pelas4.store, xlab = "mdl4", ylim = c(0, -6))
boxplot(pelas5.store, xlab = "mdl5", ylim = c(0, -6))
boxplot(pelas6.store, xlab = "mdl6", ylim = c(0, -6))



###############################################################################
### pooled model
###############################################################################

# plot all data
plot(lmv1 ~ lpr1, data = df_rfjdata)

# overlay regression line (adjust the intercept by the average effect of the non-price variables)
abline(
  a = mdl1$coefficients["(Intercept)"] + mean(df_rfjdata$adisp1) * mdl1$coefficients["adisp1"] + mean(df_rfjdata$afeat1) * mdl1$coefficients["afeat1"],
  b = mdl1$coefficients["lpr1"]
)

# just plot data for one store
plot(lmv1 ~ lpr1, data = df_rfjdata[which(df_rfjdata$store == 5), ])

# overlay regression line (adjust the intercept by the average effect of the non-price variables)
abline(
  a = mdl1$coefficients["(Intercept)"] + mean(df_rfjdata$adisp1) * mdl1$coefficients["adisp1"] + mean(df_rfjdata$afeat1) * mdl1$coefficients["afeat1"],
  b = mdl1$coefficients["lpr1"]
)

# predict the validation sample using the model
pred1 <- predict(mdl1, newdata = df_rfjdata[df_rfjdata$store == 2, ], type = "response") # compute the predictions using the previous estaimtes
err1 <- (df_rfjdata$lmv1[df_rfjdata$store == 2] - pred1) # compute the error = actual - predicted

# plot the histogram for one individual store
hist(err1)
ts.plot(err1)


###############################################################################
### store model
###############################################################################

# plot the results
plot(lmv1 ~ lpr1, data = df_rfjdata[which(df_rfjdata$store == 5), ])

# overlay regression line
coef <- mdl2$coefficients[c("(Intercept)", "adisp1", "afeat1", "store5", "lpr1", "store5:lpr1")] # extract the coefficients
abline(
  a = coef[1] + mean(df_rfjdata$adisp1) * coef[2] + mean(df_rfjdata$afeat1) * coef[3] + coef[4],
  b = coef[5] + coef[6], col = "blue"
)

# overlay regression line (adjust the intercept by the average effect of the non-price variables)
abline(
  a = mdl1$coefficients["(Intercept)"] + mean(df_rfjdata$adisp1) * mdl1$coefficients["adisp1"] + mean(df_rfjdata$afeat1) * mdl1$coefficients["afeat1"],
  b = mdl1$coefficients["lpr1"]
)

# add legend
legend("topright", c("Pooled", "Store"), lty = 1, col = c("black", "blue"), bty = "n")

# save the output of the coefficients as a matrix
# we have to extract the store and store*lprice effects and organize them as a matrix so it is easier to deal with
cstoren <- as.character(unique(df_rfjdata$store)) # vector of store numbers
cstore <- paste0("store", cstoren) # create character vector with all store names and store numbers (notice store2 is base level and is dropped)

# first extract the common chain wide effect
mdl2.int <- mdl2$coefficients["(Intercept)"]
mdl2.lprice <- mdl2$coefficients["lpr1"]
mdl2.feat <- mdl2$coefficients["afeat1"]
mdl2.disp <- mdl2$coefficients["adisp1"]

# next extract the store effects
mdl2.store.int <- mdl2$coefficients[cstore] # looks for all stores
names(mdl2.store.int)[1] <- "store2"
mdl2.store.int[1] <- 0 # replace base value
mdl2.store.lprice <- mdl2$coefficients[paste0(cstore, ":lpr1")]
names(mdl2.store.lprice)[1] <- "store2"
mdl2.store.lprice[1] <- 0 # replace base value

# combine them together
mdl2.parm <- cbind(mdl2.int, mdl2.lprice, mdl2.feat, mdl2.disp, mdl2.store.int, mdl2.store.lprice)
write.csv("mdl2_parm.csv") # you can import this into excel

# first extract the common chain wide effect
mdl5.int <- mdl5$coefficients["(Intercept)"]
mdl5.lprice <- mdl5$coefficients["lpr1"]
mdl5.feat <- mdl5$coefficients["afeat1"]
mdl5.disp <- mdl5$coefficients["adisp1"]

# next extract the store effects
mdl5.store.int <- mdl5$coefficients[cstore] # looks for all stores
names(mdl5.store.int)[1] <- "store2"
mdl5.store.int[1] <- 0 # replace base value
mdl5.store.lprice <- mdl5$coefficients[paste0(cstore, ":lpr1")]
names(mdl5.store.lprice)[1] <- "store2"
mdl5.store.lprice[1] <- 0 # replace base value

# combine them together
mdl5.parm <- cbind(mdl5.int, mdl5.lprice, mdl5.feat, mdl5.disp, mdl5.store.int, mdl5.store.lprice)
write.csv("mdl5.parm") # you can import this into excel

# compare distribution of price elasticities
par(mfrow = c(2, 1))
hist(mdl2.parm[, 2] + mdl2.parm[, 6], xlab = "Price Elasticity", xlim = c(0, -5), main = "Store Price Coefficients")
abline(v = mean(mdl2.parm[, 2] + mdl2.parm[, 6]), col = "blue", lwd = 3) # add mean as vertical line
hist(mdl5.parm[, 2] + mdl5.parm[, 6], xlab = "Price Elasticity", xlim = c(0, -5), main = "Store Price Coefficients with Store Competing")
abline(v = mean(mdl5.parm[, 2] + mdl5.parm[, 6]), col = "blue", lwd = 3) # add mean as vertical line



###############################################################################
### extract the wholesale costs from week #93
###############################################################################

# find the wholesale costs for week #93
iweek <- 93

# extract vector of profit margin, price and costs for given week
week.store <- df_rfjdata$store[df_rfjdata$week == iweek]
week.profm1 <- df_rfjdata$profm1[df_rfjdata$week == iweek]
week.price1 <- exp(df_rfjdata$lpr1[df_rfjdata$week == iweek]) * 64
week.cost1 <- week.price1 * (1 - week.profm1)

# create matrix with store #'s in rows
scost <- data.frame(store = week.store, cost = week.cost1, price = week.price1)

# compute elasticities
pelas <- data.frame(
  store = cstoren,
  pelas2 = mdl2.parm[, 2] + mdl2.parm[, 6], pelas5 = mdl5.parm[, 2] + mdl5.parm[, 6]
)

# compute optimal prices
all <- merge(scost, pelas, by = "store")
all$oprice2 <- all$cost * all$pelas2 / (1 + all$pelas2)
all$oprice2[all$oprice2 < 0] <- NA
all$oprice5 <- all$cost * all$pelas5 / (1 + all$pelas5)
cor(all$oprice2, all$price, use = "complete")
cor(all$oprice5, all$price)
par(mfrow = c(1, 3), oma = c(0, 0, 5, 0))
boxplot(all$price, data = all, xlab = "Actual", ylab = "Price", ylim = c(2, 16))
boxplot(all$oprice2, data = all, xlab = "Model#2", ylim = c(2, 16))
boxplot(all$oprice5, data = all, xlab = "Model#5", ylim = c(2, 16))
title(main = "Predicted Optimal Prices Across Stores", outer = TRUE)

# redo with violin plots
if (!require(vioplot)) {install.packages("vioplot"); library(vioplot)}

par(mfrow = c(1, 3), oma = c(0, 0, 5, 0))
vioplot(all$price, data = all, xlab = "Actual", ylab = "Price", ylim = c(2, 16))
vioplot(all$oprice2, data = all, xlab = "Model#2", ylim = c(2, 16))
vioplot(all$oprice5, data = all, xlab = "Model#5", ylim = c(2, 16))
title(main = "Predicted Optimal Prices Across Stores", outer = TRUE)



###############################################################################
### illustrate hierarchical models
###############################################################################

# reset graphics
par(mfrow = c(1, 1)) # 1 graph per panel

# plot all data
plot(lmv1 ~ lpr1, data = df_rfjdata)
abline(mdl1)

# just plot data for one store
plot(lmv1 ~ lpr1, data = df_rfjdata[which(df_rfjdata$store == 5), ])
abline(mdl1)



###############################################################################
### log linear model (store model)
###############################################################################

# extract the coefficients
coef <- mdl2$coefficients[c("(Intercept)", "store5", "lpr1", "store5:lpr1")]

# plot the results
plot(lmv1 ~ lpr1, data = df_rfjdata[which(df_rfjdata$store == 5), ])
abline(a = coef[1] + coef[2], b = coef[3] + coef[4], col = "blue")
abline(mdl1) # overlay pooled model

# hierarchical model in which the intercept and lprice variable is allowed to change
(hmdl3 <- hglm2(meanmodel = lmv1 ~ lpr1 + (1 | store) + (lpr1 | store), data = df_rfjdata[lvec_trainsample, ]))

# extract the coefficients
fcoef <- hmdl3$fixef
rcoef <- hmdl3$ranef[c("(Intercept)| store:5", "lpr1| store:5")]

# plot the results
plot(lmv1 ~ lpr1, data = df_rfjdata[which(df_rfjdata$store == 5), ])
abline(a = fcoef[1] + rcoef[1], b = fcoef[2] + rcoef[2], col = "red") # results from hierarchical model
abline(a = coef[1] + coef[2], b = coef[3] + coef[4], col = "blue") # results from store model
abline(
  a = mdl1$coefficients["(Intercept)"] + mean(df_rfjdata$afeat1) * mdl1$coefficients["afeat1"] + mean(df_rfjdata$afeat1) * mdl1$coefficients["adisp1"],
  b = mdl1$coefficients["lpr1"]
) # results from pooled model
legend("topright", c("Mixed", "Store", "Pool"), lty = 1, col = c("red", "blue", "black"), bty = "n")

