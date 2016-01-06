# This file contains ML model
# Kaggle Rossmann Store Sales Challenge
# (c) Dmytro Perepolkin, 2015

library(xgboost)
library(caret)
library(zoo)
library(forecast)
library(TStools)
library(randomForest)

WIN <- TRUE

if (WIN) {
  source('~//R//Rossmann//dp7.R', echo=TRUE)
  source('~//R//Rossmann//fn7.R', echo=TRUE)
} else {
  source('~/Documents/Kaggle/Rossmann/dp7.R', echo=TRUE)
  source('~/Documents/Kaggle/Rossmann/fn7.R', echo=TRUE)
}

runTSrf <- function(ss, holdout, silent=TRUE){
#ss <- 25; holdout=T;silent=F

sss <- paste0("S",ss,collapse = "")
t_S <- ts(tsSales[1:(td+horizon),(sss), with=F], f=7, start=2013)
#t_C <- ts(tsCustomers[1:(td+horizon),(sss), with=F], f=7, start=2013)

taa_S <- ts(tsSales[1:td,(sss), with=F], f=7, start=2013)
#taa_C <- ts(tsCustomers[1:td,(sss), with=F], f=7, start=2013)

if (holdout) {
  labsaa_S <- ts(tsSales[(td+1):(td+horizon),(sss), with=F], f=7, start=2013)
  } else{
  labsaa_S <- rep(0,48)
  }

xregML <- MakeXregML(xreg.dt, ss, startd, startf+horizon-1)
#xreg <- MakeXreg(xreg.dt, ss, startd, startf+horizon-1)

#hwgam <- ets(taa_S, model="ZZZ"); method <- gsub(",", "", substr(hwgam$method, 5, nchar(hwgam$method)-1))
#hwgamS <- es(taa_S,  xreg=xreg, model=method, h=horizon, silent = T)
#hwgam <- ets(taa_C, model="ZZZ"); method <- gsub(",", "", substr(hwgam$method, 5, nchar(hwgam$method)-1))
#hwgamC <- es(taa_C,  xreg=xreg, model=method, h=horizon, silent = T)

xregaa <- xregML[1:td,]
xregaaf <- xregML[(td+1):(td+horizon),]

#cat("inplace accuracy /n")
#rmpse(as.integer(expm1(gamdata$Sales))-intec, as.integer(expm1(hwgamS$fitted))-intec)$value
#rmpse(as.integer(expm1(gamdataf$Sales))-intec, as.integer(expm1(hwgamS$forecast))-intec)$value

#readyPred <- read_csv("arima2015-12-13-10-40-09df.csv") #es2015-12-13-10-41-22df.csv ##arima2015-12-13-10-40-09df.csv
#readyPred$Sales <- log1p(readyPred$Sales+intec)
#hwgamS$fitted <- readyPred[readyPred$Store==25 & readyPred$Date<as.Date(startf), 3]
#hwgamS$forecast <- readyPred[readyPred$Store==25 & readyPred$Date>=as.Date(startf), 3]


gamdata <- cbind(Sales=as.vector(taa_S), xregaa)#, ESmod=as.vector(hwgamS$fitted), ESmodC=as.vector(hwgamC$fitted))
gamdataf <- cbind(Sales=as.vector(labsaa_S), xregaaf)#, ESmod=as.vector(hwgamS$forecast), ESmodC=as.vector(hwgamC$forecast))

#cat("ES/ARIMA accuracy /n")
#rmpse(as.integer(expm1(gamdata$Sales))-intec, as.integer(expm1(hwgamS$fitted))-intec)$value
#rmpse(as.integer(expm1(gamdataf$Sales))-intec, as.integer(expm1(hwgamS$forecast))-intec)$value

form.lin <- as.formula(paste("Sales", paste(colnames(gamdata[,-1]), collapse ="+"), sep = "~"))

myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = nrow(gamdata)-horizon*2,
                              horizon = horizon,
                              fixedWindow = TRUE)

#ksvmm1 <- train(form.lin,data=gamdata, method="gam", trControl = myTimeControl, tuneLength=5)
#ksvmm <- train(form.lin,data=gamdata, method="gbm", trControl = myTimeControl, tuneLength=5)
#ksvmm <- earth(form.lin, data=gamdata)
set.seed(111)
ksvmm1 <- randomForest(y=gamdata$Sales, x=gamdata[,-1], ntree=1000, importance = F)
#ksvmm2 <- knnreg(gamdata[,-1], gamdata[,1], k=3, trControl = myTimeControl, tuneLength=5)

ksvmfit <- predict(ksvmm1, gamdata[,-1])
ksmvpred <- predict(ksvmm1, gamdataf[,-1])

#ksvmfit2 <- predict(ksvmm2, gamdata[,-1])
#ksmvpred2 <- predict(ksvmm2, gamdataf[,-1])

#ksvmfit <- (ksvmfit1 + ksvmfit2)/2
#ksmvpred <- (ksmvpred1+ksmvpred2)/2


par(mfrow=c(2,1))
plot(as.integer(expm1(gamdata$Sales))-intec, type="l")
lines(as.integer(expm1(ksvmfit))-intec, col="blue")

plot(as.integer(expm1(gamdataf$Sales))-intec, type="l")
lines(as.integer(expm1(ksmvpred))-intec, col="red")
#lines(as.integer(expm1(oldksmvpred))-intec, col="green")

err.aa0_S <- rmpse(as.integer(expm1(ksvmfit))-intec,as.integer(expm1(gamdata$Sales))-intec)
err.aa_S <- 0#rmpse(as.integer(expm1(ksmvpred))-intec,as.integer(expm1(gamdataf$Sales))-intec)

#varImpPlot(ksvmm1)
pred <-  c(as.integer(expm1(ksvmfit))-intec, as.integer(expm1(ksmvpred))-intec)

dts <- seq(startd, startf+horizon-1, by="days")
cat("Store=", ss, "RMPSEin=", err.aa0_S$value, "\n") #"RMPSEout=", err.aa_S$value,
res <- data.frame(Store=ss, Date=dts, Sales=pred)
return(res)
}

# Initialize the run
train.stores <- unique(test$Store)
horizon <- 48
#holdout <- TRUE
#goParallel <- TRUE
startd <- as.Date("2013-01-01")
startf <- as.Date("2015-08-01")
td <- as.numeric(startf-startd)
problem.stores <- c(25,764,897,30,738,445,532,623,927,589,348,831,426,557,886,809,126,570,137,175)
single.stores <- c(137, 25)

cat("Adding results from ARIMAL1")
if (WIN) fpath <- c("~//R//Rossmann//L1-MODS//") else fpath <- c("~/Documents/Kaggle/Rossmann/L1-MODS/") 
arimafiles <- c("AL1S2015-12-14-14-27-54df.csv", 
                "AL1S2015-12-14-15-31-25df.csv",
                "AL1S2015-12-14-16-36-08df.csv",
                "AL1S2015-12-14-18-48-03df.csv")
ArimaL1dt <- LoadRes(fpath, arimafiles, "ArimaL1S") 
xreg.dt <- merge(xreg.dt, ArimaL1dt, all.x = T, by=c("Store", "Date"))
rm(ArimaL1dt)
cat("Adding results from ESL1S")
esfiles <- c("ESL1S2015-12-14-20-09-37df.csv")
ESL1dt <- LoadRes(fpath, esfiles, "ESL1S")
xreg.dt <- merge(xreg.dt, ESL1dt, all.x = T, by=c("Store", "Date"))

cat("Adding results from ESL1C")
esfiles <- c("ESL1C2015-12-14-21-12-30df.csv")
ESL1dt <- LoadRes(fpath, esfiles, "ESL1C")
xreg.dt <- merge(xreg.dt, ESL1dt, all.x = T, by=c("Store", "Date"))
rm(ESL1dt)

predrfdf <- NULL
for (ss in train.stores[576:856]){ 
p <- runTSrf(ss, holdout = F, silent = F)
predrfdf <- rbind(predrfdf, p)
}
predrfdf1 <- predrfdf[!duplicated(predrfdf), ]

savefiles(predrfdf1, "rf")


par(mfrow=c(1,1))