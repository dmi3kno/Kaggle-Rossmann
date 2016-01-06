# This file contains data preparation for the model 
# Kaggle Rossmann Store Sales Challenge
# (c) Dmytro Perepolkin, 2015

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

Sys.setlocale("LC_TIME", "C")

rm(list=ls())

WIN <- TRUE

if (WIN) {setwd("~//R//Rossmann")} else{setwd("~/Documents/Kaggle/Rossmann")}

# Load Google Trends and weather
states <- read_csv2("DE-states.csv")
weather <- NULL
gt.ra <- NULL

if (WIN){path <-"~//R//Rossmann//Weather//"}else{path <-"../Rossmann/Weather/"}
# import weather and Google Trends data
for (j in states$Code[1:16]){
  cat("Reading state", j, "\n")
  stateweather <- read_csv2(paste0(path,j,"_weather.csv", collapse = ""))
  cat(nrow(stateweather), "lines \n")
  stateweather$State <- j
  weather <- rbind(weather, stateweather)
}

if (WIN){path <-"~//R//Rossmann//GT_RD//"}else{path <-"../Rossmann/GT_RD/"}

for (j in states$Code){
  cat("Reading state", j, ", ")
  ra <- read_csv(paste0(path,j,"_gt_rd.csv", collapse = ""))
  cat(nrow(ra), "lines \n")
  ra$State <- j
  names(ra) <- c("Week","Rossmann","Dm", "State")
  gt.ra <- rbind(gt.ra, ra[1:146,])
}
rm(stateweather, ra)
gt.ra$Date <- as.Date(sapply(gt.ra$Week, function(x) substr(x, 1, 10)))

gtadj <- read_csv("gtdaily.csv")
gtadj <- as.data.table(gtadj)
gtadj[,WeekYear:=format(Date, "%W-%Y")]
gtadj[,RossmannWAdj :=mean(RossmannAdj), by=WeekYear]

dmadj <- read_csv("dmdaily.csv")
dmadj <- as.data.table(dmadj)
dmadj[,WeekYear:=format(Date, "%W-%Y")]
dmadj[,dmWAdj :=mean(dmAdj), by=WeekYear]
dmadj$WeekYear <- NULL

# Load train and test sets
train <- read_csv("train.csv", col_types = list(StateHoliday=col_factor(c("0","a","b","c"))))
test <- read_csv("test.csv", col_types = list(StateHoliday=col_factor(c("0","a","b","c"))))
stores <- read_csv("store.csv")
storestates <- read_csv("store_states.csv")
holidays <- read_csv("holidays.csv")

storestates$State[storestates$State=="HB,NI"] <- "NI"

stores <- merge(stores, storestates, by="Store")
train <- merge(train, storestates, by="Store")
test <- merge(test, storestates, by="Store")
rm(storestates)

#fix some NAs in stores

stores$CompetitionOpenSinceYear[is.na(stores$CompetitionOpenSinceYear)] <- 1990 # Dealing with NA and outlayers
stores$CompetitionOpenSinceMonth[is.na(stores$CompetitionOpenSinceMonth)] <- 1 # Dealing with NA
stores$CompetitionDistance[is.na(stores$CompetitionDistance)] <- 75000 # Dealing with NA

stores$CompetitionOpenSinceDate <- as.Date(paste(stores$CompetitionOpenSinceYear, stores$CompetitionOpenSinceMonth, 1, sep=" "), format = "%Y %m %d")
stores$CompetitionOpenSinceDate[is.na(stores$CompetitionOpenSinceDate)] <- as.Date("2016-01-01")
stores$Promo2SinceDate <- as.Date(paste(stores$Promo2SinceYear, stores$Promo2SinceWeek, 1, sep=" "), format = "%Y %U %u")
stores$Promo2SinceDate[is.na(stores$Promo2SinceDate)] <- as.Date("2016-01-01")
#Convert to data.tables for easier handling
train <- as.data.table(train)
test <- as.data.table(test)
stores <- as.data.table(stores)
holidays <- as.data.table(holidays)
weather <-  as.data.table(weather)

#treat missing data in #988
missingNY <- train[Store==988 & Date==as.Date("2013-01-02")]
missingNY$Date <-as.Date("2013-01-01")
train <- rbind(train, missingNY)
rm(missingNY)
# Rename States, treat missing data in test set

test[is.na(Open), Open:=0]

# Verity that missing data is in fact all in BY state
#goodstores  <- train[Date==as.Date("2014-07-01"), Store]
#stores$NoGaps <- as.numeric(stores$Store %in% goodstores)
#table(stores$State, stores$NoGaps)

#Copy and paste 2013 data
missingdays <- as.Date("2015-01-01") - as.Date("2014-07-01")
trainBY2013 <- train[State=="BY" & Date>=as.Date("2013-07-02") & Date <= as.Date("2013-07-02")+missingdays-1]
trainBY2013 <- trainBY2013[, Date:= Date+364]
train <- rbindlist(list(train, trainBY2013), use.names = TRUE)
rm(trainBY2013)

# zero sales should mean closed store
train[Sales==0, Open:=0]

#fix the Google Trends data
gt.ra <- as.data.table(gt.ra)
gt.ra[Date==as.Date("2013-01-06"), Date:=as.Date("2013-01-01")]
gt.ra[,Week:=NULL]

#Sort everything by date
train <- train[order(Date),]
test <- test[order(Date),]
holidays <- holidays[order(Date),]
gt.ra <- gt.ra[order(Date),]
weather <- weather[order(Date),]

# see if there's any more missing data
cat("no more missing data in train:", train[,.N] == length(unique(train$Store)) * (as.Date("2015-08-01")-as.Date("2013-01-01")))
#table(is.na(train)) #should be 1050330 observations
cat("no more missing data in test:", test[,.N]  == length(unique(test$Store)) * (as.Date("2015-09-18")-as.Date("2015-08-01")))
#table(is.na(test)) #should be 41088 observations

MarkStoreRefurbishment <- function(storeopen, minlength){
  SCrle <- rle(storeopen)
  SCrle$values[SCrle$length > minlength] <- replace(SCrle$values[SCrle$length > minlength],  SCrle$values[SCrle$length > minlength] ==0, 999) 
  StoreClosures <- as.numeric(inverse.rle(SCrle)==999)
  return(StoreClosures)
}
intec <- 5000L

# prepare y matrices for ARIMA models
tsReceipts <- copy(train[,.(Date,Store, Sales, Customers)])
tsReceipts[, Receipts:=Sales/Customers]
tsReceipts[, Receipts:= Receipts+intec/1000]
tsReceipts[is.na(Receipts), Receipts:=0]
tsReceipts[, c("Sales", "Customers"):=NULL]
tsReceipts <- dcast.data.table(tsReceipts, Date ~ Store, value.var = "Receipts")
colnames(tsReceipts) <- c("Date", sapply(1:1115, function(x) paste0("S", x, collapse = "")))

tsSales <- copy(train[,.(Date,Store,Sales)])
#tsSales[, Sales:=Sales+intec]
tsSales[, Sales:= log1p(Sales+intec)]
tsSales <- dcast.data.table(tsSales, Date ~ Store, value.var = "Sales")
colnames(tsSales) <- c("Date", sapply(1:1115, function(x) paste0("S", x, collapse = "")))

tsCustomers <- copy(train[,.(Date,Store,Customers)])
#tsCustomers[, Customers:=Customers+intec]
tsCustomers[, Customers:=log1p(Customers+intec)]
tsCustomers <- dcast.data.table(tsCustomers, Date ~ Store, value.var = "Customers")
colnames(tsCustomers) <- c("Date", sapply(1:1115, function(x) paste0("S", x, collapse = "")))

#train[, c("Sales", "Customers") := NULL]
train[, Sales:=Sales+intec]
train[, Customers:=Customers+intec]
test[, c("Sales", "Customers") := 0]
#test[, Id := NULL]

# Bind train and test (w/o ID field) to use as regressor database
xreg.dt <- rbindlist(list(train, test[,!"Id", with=FALSE]), use.names = TRUE)
xreg.dt <- merge(xreg.dt, gtadj, all.x =T, by="Date")
xreg.dt <- merge(xreg.dt, dmadj, all.x =T, by="Date")
rm(gtadj);rm(dmadj)

holidaysObservance <- holidays[HolidayName %in% c("May Day", "Valentine's Day", "Mother's Day"), Date]
holidayStNicholas <- holidays[HolidayName %in% c("Saint Nicholas Day"), Date]
holidayNational <- holidays[HolidayName %in% c("New Year's Day", "Easter Monday","Ascension Day", "Whit Monday", "Day of German Unity"), Date]
holidayLocal.dt <- holidays[HolidayType %in% c("Common Local holidays", "Local holiday"), .(Date, ObservedIn)]
holidayChristmas <- holidays[HolidayName %in% c("Christmas Eve", "Christmas Day", "Boxing Day"), Date]
holidayEaster <- holidays[HolidayName %in% c("Easter Day"), Date]
holidayGoodFriday<- holidays[HolidayName %in% c("Good Friday"), Date]

xreg.dt <- merge(xreg.dt, holidayLocal.dt, all.x = T, by="Date")
rm(holidayLocal.dt)

setkey(xreg.dt, Store, Date)
xreg.dt[, RossmannAdjm1:= shift(RossmannAdj, 1, type="lag", fill=mean(RossmannAdj)),by=.(Store)]
xreg.dt[, RossmannAdjd1:= (RossmannAdj-RossmannAdjm1), by=.(Store)]
xreg.dt[, RossmannAdjm7:= shift(RossmannAdj, 7, type="lag", fill=mean(RossmannAdj)),by=.(Store)]
xreg.dt[, RossmannAdjd7:= (RossmannAdj-RossmannAdjm7), by=.(Store)]
xreg.dt[, RossmannAdjm14:= shift(RossmannAdj, 14, type="lag", fill=mean(RossmannAdj)),by=.(Store)]
xreg.dt[, RossmannAdjd14:= (RossmannAdj-RossmannAdjm14), by=.(Store)]


xreg.dt[, dmAdjm1:= shift(dmAdj, 1, type="lag", fill=mean(dmAdj)),by=.(Store)]
xreg.dt[, dmAdjd1:= (dmAdj-dmAdjm1), by=.(Store)]
xreg.dt[, dmAdjm7:= shift(dmAdj, 7, type="lag", fill=mean(dmAdj)),by=.(Store)]
xreg.dt[, dmAdjd7:= (dmAdj-dmAdjm7), by=.(Store)]
xreg.dt[, dmAdjm14:= shift(dmAdj, 14, type="lag", fill=mean(dmAdj)),by=.(Store)]
xreg.dt[, dmAdjd14:= (dmAdj-dmAdjm14), by=.(Store)]

xreg.dt[, OpenYTD:= shift(Open, 1, type="lag", fill=0), by=.(Store)]

xreg.dt[,ObservedHoliday:=as.numeric(Date %in% holidaysObservance)]
xreg.dt[,ObservedHoliday:=shift(ObservedHoliday, 1, type = "lead", fill = 0), by=.(Store)]

xreg.dt[,NationalHoliday:=as.numeric(Date %in% holidayNational)]
xreg.dt[,ChristmasHoliday:=as.numeric(Date %in% holidayChristmas)]
xreg.dt[,StNicholasHoliday:=as.numeric(Date %in% holidayStNicholas)]
xreg.dt[,StNicholasHolidayp1:=shift(StNicholasHoliday, 1, type = "lead", fill = 0), by=.(Store)]
xreg.dt[,StNicholasHolidaym1:=shift(StNicholasHoliday, 1, type = "lag", fill = 0), by=.(Store)]

xreg.dt[,EasterHoliday:=as.numeric(Date %in% holidayEaster)]
xreg.dt[,EasterHoliday:=shift(EasterHoliday, 1, type = "lead", fill = 0), by=.(Store)]
xreg.dt[,GoodFridayHoliday:=as.numeric(Date %in% holidayGoodFriday)]
xreg.dt[,GoodFridayHoliday:=shift(GoodFridayHoliday, 1, type = "lead", fill = 0), by=.(Store)]
xreg.dt[,LocalHoliday:=as.numeric(State %in% unlist(strsplit(ObservedIn, split=", ")))]
xreg.dt[,ObservedIn := NULL]
#xreg.dt[,StateHoliday := NULL]

xreg.dt[,DayOfWeek := as.factor(format(Date, "%a"))] # Dont need it as ts is set at f= 7
xreg.dt[,DayOfMonth := as.numeric(format(Date, "%d"))] #dont need it as factor
xreg.dt[,MonthOfYear := as.factor(format(Date, "%b"))]
xreg.dt[,Year := as.factor(format(Date, "%Y"))]
#xreg.dt[,OpenSunday :=as.numeric(DayOfWeek==7 & Open==1)]

xreg.dt[, Payday:= as.numeric(DayOfMonth %in% c(1, 2, 15, 16))]
xreg.dt[, EOM:=as.numeric(DayOfMonth %in% c(29,30,31))]
xreg.dt[,Nov22:=as.numeric(DayOfMonth==22 & MonthOfYear=="Nov")]
xreg.dt[,FoolDay:=as.numeric(DayOfMonth==01 & MonthOfYear=="Apr")]
xreg.dt[,FoolDay:=as.numeric(DayOfMonth==01 & MonthOfYear=="Apr")]

xreg.dt[, MOD:=as.numeric(DayOfMonth %in% c(17, 18, 19, 20) & MonthOfYear %in% c("Dec"))]
xreg.dt[, ChristmasWeek:=as.numeric(DayOfMonth %in% c(21, 22, 23) & MonthOfYear %in% c("Dec"))]
xreg.dt[, OrtXms:=as.numeric(DayOfMonth %in% c(05, 06, 07) & MonthOfYear %in% c("Jan"))]

weather[, Rain:=as.numeric(grepl("Rain",weather$Events))]
weather[, Snow:=as.numeric(grepl("Snow",weather$Events))]
weather[, Fog:=as.numeric(grepl("Fog",weather$Events))]
weather[, Hail:=as.numeric(grepl("Hail",weather$Events))]
weather[, Thunderstorm:=as.numeric(grepl("Thunderstorm",weather$Events))]

weather <- weather[,.(State, Date, Mean_TemperatureC, Mean_Wind_SpeedKm_h, Precipitationmm, Rain, Snow, Fog, Hail, Thunderstorm)]

xreg.dt <- merge(xreg.dt, weather, all.x = T, by=c("State", "Date"))
rm(weather)

# NB watch out for refurb period in test set for stores 622 (closed from 5-sep) and 879 (closed from 12-sep)
xreg.dt[, RefurbPeriod := MarkStoreRefurbishment(Open, 5), by=Store]
RefurbStartDates <- xreg.dt[RefurbPeriod==1, .(RefStartDate=min(Date)), by=Store]
xreg.dt <- merge(xreg.dt, RefurbStartDates, all.x=T, by="Store")
rm(RefurbStartDates)

xreg.dt[, SaleStarts:=as.numeric((RefStartDate-Date) %in% c(8:14) & DayOfWeek=="Mon")]
SaleStartDates <- xreg.dt[SaleStarts==1, .(SaleStartDate=Date), by=Store]
xreg.dt <- merge(xreg.dt, SaleStartDates, all.x=T, by="Store")
rm(SaleStartDates)

xreg.dt[Date>=SaleStartDate, FireSale:=as.numeric((RefStartDate-Date) %in% c(7:14))]
xreg.dt[, c("RefStartDate", "SaleStartDate"):=NULL]
xreg.dt[is.na(FireSale), FireSale:=0]

#xreg.dt[, PreRefurbm1:=shift(RefurbPeriod, n=1, type="lead", fill=0) - RefurbPeriod, by=Store]
#xreg.dt[PreRefurbm1==-1, PreRefurbm1:=0]


xreg.dt[, PostRefurb:=shift(RefurbPeriod, n=1, type="lag", fill=0) - RefurbPeriod, by=Store]
xreg.dt[PostRefurb==-1, PostRefurb:=0]

xreg.dt[, PostRefurbp1:=shift(PostRefurb, n=1, type="lag", fill=0), by=Store]
xreg.dt[, PostRefurbp2:=shift(PostRefurb, n=2, type="lag", fill=0), by=Store]
xreg.dt[, PostRefurbp3:=shift(PostRefurb, n=3, type="lag", fill=0), by=Store]
xreg.dt[, PostRefurbp4:=shift(PostRefurb, n=4, type="lag", fill=0), by=Store]
xreg.dt[, PostRefurbp5:=shift(PostRefurb, n=5, type="lag", fill=0), by=Store]

#cat("adding competition effect flag\n")
CompetitionEffect = 30

stores[is.na(Promo2SinceWeek), Promo2SinceWeek:= 1]
stores[is.na(Promo2SinceYear), Promo2SinceYear:= 2016]
stores[, State:=NULL]
xreg.dt <- merge(xreg.dt, stores, all.x = T, by="Store")
xreg.dt[,Promo2Month:=as.numeric(format(Date, "%b") %in% unlist(strsplit(as.character(PromoInterval), split=",")))]
xreg.dt[is.na(Promo2Month), Promo2Month:= 0]

cat("xreg.dt is ready!/n")#; summary(xreg.dt)


