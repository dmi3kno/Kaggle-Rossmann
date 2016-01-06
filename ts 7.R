# This file contains main time series models for
# Kaggle Rossmann Store Sales Challenge
# (c) Dmytro Perepolkin, 2015

if (!require("devtools"))
  install.packages("devtools")
if (!require("TStools")) 
  devtools::install_github("trnnick/TStools")

library(readr)
library(data.table)
library(caret)
#library(e1071)
#library(tsoutliers)
library(forecast)
library(TStools)
#library(zoo)
#library(CES)
#library(rminer)
require(MASS)

WIN <- TRUE

if (WIN) {
  source('~//R//Rossmann//dp7.R', echo=TRUE)
  source('~//R//Rossmann//fn7.R', echo=TRUE)
} else {
  source('~/Documents/Kaggle/Rossmann/dp7.R', echo=TRUE)
  source('~/Documents/Kaggle/Rossmann/fn7.R', echo=TRUE)
}
# runs Arima model for a store
runArima <- function(ss, holdout, silent=TRUE) { 
#  tryCatch({
  cat("Starting Arima for store", ss)
  sss <- paste0("S",ss,collapse = "")
  t_S <- ts(tsSales[Date>=startd&Date<=startf+horizon-1,(sss), with=FALSE], f=7)
  if (holdout) labsaa_S <- t_S[(length(t_S)-horizon+1):length(t_S)]
  taa_S <- ts(tsSales[Date>=startd&Date<=startf-1,(sss), with=FALSE], f=7)

  
#  if (holdout) {taa_S <- t_S[1:(length(t_S)-horizon)] }else{taa_S <- t_S[1:(length(t_S))]}
  
  xreg <- MakeXreg(xreg.dt, ss, startd, startf+horizon-1)
  
  xregaa <- xreg[1:(nrow(xreg)-horizon),]
  xregaaf <- xreg[(nrow(xreg)-horizon+1):nrow(xreg),]
  
  lambda_S <- tryCatch({BoxCox.lambda(taa_S, method='guerrero', lower=-5, upper=5)}, error = function(e) e) #guerrero
  if (any(class(lambda_S) == "error")) lambda_S <- BoxCox.lambda(taa_S, method='loglik', lower=-5, upper=5)
  
  zaa_S <- auto.arima(taa_S, xreg=xregaa, stepwise=T,parallel=F, approximation=T, trace=F, lambda = lambda_S)
  #zaa_S <- tryCatch(auto.arima(taa_S, xreg=xregaa, approximation=F,trace=F, stepwise=F, parallel=T, lambda = lambda_S), error = function(e) e)
  zaaf_S <- forecast(zaa_S, xreg=xregaaf)
  if (!silent) plot(zaaf_S)
  
  predsaa_S <- as.integer(expm1(zaaf_S$mean))-intec
  isx_S <- as.integer(expm1(zaaf_S$x))-intec
  fitx.aa_S <- as.integer(expm1(zaaf_S$fitted))-intec
  if (holdout)  labsaa_S <- as.integer(expm1(labsaa_S))-intec
  model <- paste(zaa_S$arma, collapse = " ")
  
  
  err.aa0_S <- rmpse(fitx.aa_S, isx_S)
#  err.aa0_S100 <- rmpse(tail(fitx.aa_S, 100), tail(isx_S,100))
  
  if (holdout) { 
    err.aa_S <- rmpse(predsaa_S, labsaa_S)
    #cat("Sales:", err.aa_S$value, " in sample ", err.aa0_S$value, "\n")
  } else {
    err.aa_S <- NULL
    #cat("Sales in sample ", err.aa0_S$value,"last 100 days", err.aa0_S100$value, "\n")
  }
  dts <- seq(startd, startf+horizon-1, by="days")
  cat("Store=", ss, "Lambda=", lambda_S, "Model=", model, "RMPSEin=", err.aa0_S$value, "RMPSEout=", err.aa_S$value,"\n")
  res <- data.frame(Store=ss, Date=dts, Sales=c(fitx.aa_S,predsaa_S))
  cat("Done with Arima for store", ss)
  return(res)
#  }, error = function(e) e)
} #end of ARIMA function
# runs exponential smoothing model for a store
runES <- function(ss, holdout, silent=TRUE){
  cat("Starting ES for store", ss)
  sss <- paste0("S",ss,collapse = "")

  t_R <- ts(tsReceipts[Date>=startd&Date<=startf-1,(sss), with=FALSE], f=7, start=2013)
  if (holdout) {
    t_S <- ts(tsSales[Date>=startd&Date<=startf+horizon-1,(sss), with=FALSE], f=7, start=2013)
  } else {
    t_S <- ts(tsSales[Date>=startd&Date<=startf-1,(sss), with=FALSE], f=7, start=2013)
    }
  xreg <- MakeXreg(xreg.dt, ss, startd, startf+horizon-1)
  hwgam <- ets(ts(t_R,f=7), model="ZZZ"); method <- gsub(",", "", substr(hwgam$method, 5, nchar(hwgam$method)-1))
  hwgamR <- es(ts(t_R,f=7),  xreg=xreg, model=method, h=horizon, silent = T)
  xreg$Receipts <- c(as.vector(hwgamR$fitted), as.vector(hwgamR$forecast))
  
  hwgam <- ets(ts(t_S,f=7), model="ZZZ"); method <- gsub(",", "", substr(hwgam$method, 5, nchar(hwgam$method)-1))
  
  z <- es(t_S
          , model=method#"ZZZ"
          , bounds="admissible"
          , h=horizon
          , holdout = holdout
          , xreg=xreg
          #, IC="AICc"
          , trace=FALSE 
          #, CF.type="MAE"
          , silent = silent
  )
  predsaa_S <- as.integer(expm1(z$forecast))-intec
  model <- z$model
  fitx.aa_S <- as.integer(expm1(z$fitted)) - intec
#  err.aa0_S100 <- rmpse(tail(fitx.aa_S, 100), tail(isx_S,100))
  
  if (holdout) {
    labsaa_S <- as.integer(expm1(z$x.holdout))-intec
    isx_S <- as.integer(expm1(z$x[1:(length(z$x)-horizon)])) - intec
    err.aa_S <- rmpse(predsaa_S, labsaa_S)
    #cat("Sales:", err.aa_S$value, " in sample ", err.aa0_S$value, "\n")
  } else {
    err.aa_S <- NULL
    isx_S <- as.integer(expm1(z$x)) - intec
    #cat("Sales in sample ", err.aa0_S$value,"last 100 days", err.aa0_S100$value, "\n")
  }
  err.aa0_S <- rmpse(fitx.aa_S, isx_S)
  lambda_S <- 0
  dts <- seq(startd, startf+horizon-1, by="days")
  cat("Store=", ss, "Lambda=", lambda_S, "Model=", model, "RMPSEin=", err.aa0_S$value, "RMPSEout=", err.aa_S$value,"\n")
  res <- data.frame(Store=ss, Date=dts, Sales=c(fitx.aa_S,predsaa_S))
  cat("Done with ES for store", ss)
  return(res)
} # end of ES model
# loops through stores in parallel mode. Can run both Arima and ES - see RunModelA argument
runTSmodels <- function(stores, holdout, RunModelA, silent){
  pred.df <- NULL
  #RunModelA <- FALSE
  #strt<-Sys.time()
  
    library(foreach)
    library(doParallel)
    cl <- makeCluster(8)
    registerDoParallel(cl)

    if (RunModelA){
        pred.df <- foreach(ss = stores, 
                       .packages=c("forecast","TStools", "data.table", "caret"), 
                       .export=ls(envir=globalenv()),
                       .combine = rbind) %dopar% {
        tryCatch(
          runArima(ss, holdout = holdout, silent=silent), 
          error = function(e) runES(ss, holdout = holdout, silent=silent)
            )}
      } else {
        pred.df <- foreach(ss = stores, 
                           .packages=c("forecast","TStools", "data.table", "caret"), 
                           .export=ls(envir=globalenv()),
                           .combine = rbind) %dopar% {
          runES(ss, holdout = holdout, silent=silent)
                           }
        }

    stopCluster(cl)
return(pred.df)
    }
# saves results
saveresults <- function(pred.df, modname){
  sub.df <- subset(test, select=c("Id", "Store", "Date"))
  sub.df <- merge(sub.df, pred.df, by=c("Store","Date"))
  fname.df <- paste(modname,as.character(format(Sys.time(), "%Y-%m-%d-%H-%M-%S")), "df.csv",sep="")
  fname.res <- paste(modname,as.character(format(Sys.time(), "%Y-%m-%d-%H-%M-%S")), "res.csv",sep="")
  write_csv(pred.df, fname.df)
  write_csv(sub.df, fname.res)
  return()
}

# Initialize the run
train.stores <- unique(test$Store)
horizon <- 48
startd <- as.Date("2013-01-01")
startf <- as.Date("2015-06-10")
td <- as.numeric(startf-startd)
problem.stores <- c(25,764,897,30,738,445,532,623,927,589,348,831,426,557,886,809,126,570,137,175)
single.stores <- c(137, 25)

# perform cross-validation
aa.pred.df <- runTSmodels(problem.stores, holdout = T, RunModelA = T, silent = F) # replace problem.stores with train.stores to run all stores
saveresults(aa.pred.df, "arima")
es.pred.df <- runTSmodels(problem.stores, holdout = T, RunModelA = F, silent = F) # replace problem.stores with train.stores to run all stores
saveresults(es.pred.df, "es")

# make prediction on the test set and save submission files
startf <- as.Date("2015-08-01")
aa.pred.df <- runTSmodels(problem.stores, holdout = F, RunModelA = T, silent = F) # replace problem.stores with train.stores to run all stores
saveresults(aa.pred.df, "arima")
es.pred.df <- runTSmodels(problem.stores, holdout = F, RunModelA = F, silent = F) # replace problem.stores with train.stores to run all stores
saveresults(es.pred.df, "es")

#stopCluster(cl)
#registerDoSEQ() -> unregister
