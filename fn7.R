# This file contains various service functions
# Kaggle Rossmann Store Sales Challenge
# (c) Dmytro Perepolkin, 2015

# wrapper function for writing to file during the parallel Arima
catf <- function(..., file="log.txt", append=TRUE){
  cat(..., file=file, append=append)
}

#step-wise feature selection
vif_func<-function(in_frame,thresh=10,trace=T,...){
  
  require(fmsb)
  
  if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]))
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      var_names <- names(in_dat)
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2])))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
      
    }
    
    return(names(in_dat))
    
  }
  
}

# main error function. takes plain predictions and labels (not logs)
rmpse <- function(preds, labels) {
  preds <- preds[labels!=0]
  labels <- labels[labels!=0]
  err <- sqrt(mean((preds/labels-1)^2))
  return(list(metric = "RMPSE", value = err))
}

# Create matrix of numeric predictors. Receives dataframe, store#, start and end date
MakeXreg <- function(df, store, start.date, end.date){
  # to test the function open these and run in terminal
  #start.date <- startd
  #end.date <-  startf+horizon-1
  #store <- 909
  #df <- xreg.dt

  df <- df[Store==store & Date >= start.date & Date <= end.date]
  #sstate <- df[Store==store, State]
  #gt.ra <- gt.ra[State==sstate]
  #df <- merge(df, gt.ra, all.x =T, by="Date")
  #df$Rossmann <- na.locf(df$Rossmann)
  #df$Dm <- na.locf(df$Dm)
  #weather.df <- weather[State==sstate & Date >= start.date & Date <= end.date]
  #weather.df[is.na(weather.df)] <-0
  #df <- merge(df, weather.df, all.x =T, by="Date")
  
  xreg <- data.frame(ChristmasHoliday=(df$ChristmasHoliday)*df$Open
                     #,  Open          = df$Open
                     , OpenYTD       = df$OpenYTD
                     #, DayOfWeek     = model.matrix(~as.factor(DayOfWeek)+0, data=df)*df$Open
                     #, Promo         = df$Promo*df$Open
                     #, PromoYTD      = df$PromoYTD
                     #, Sm28 = log1p(df$Sm28)*df$Open
                     #, Sm21 = log1p(df$Sm21)*df$Open
                     #, Sm14 = log1p(df$Sm14)*df$Open
                     #, Sm7 = log1p(df$Sm7)*df$Open
                     #, Rec28 = df$Rec28*df$Open
                     #, Rec21 = df$Rec21*df$Open
                     #, Rec14 = df$Rec14*df$Open
                     #, Cus28 = log1p(df$Cus28)*df$Open
                     #, Cus21 = log1p(df$Cus21)*df$Open
                     #, Cus14 = log1p(df$Cus14)*df$Open
                     #, EOQ1 = df$EOQ1*df$Open
                     #, EOQ2 = df$EOQ2*df$Open
                     #, EOQ3 = df$EOQ3*df$Open
                     #, EOJ = df$EOJ*df$Open
                     #, EOA = df$EOA*df$Open
                     #, EON = df$EON*df$Open
                     #, OrtXms = df$OrtXms * df$Open
                     #, Nov22 = df$Nov22*df$Open
                     #, FoolDay=df$FoolDay*df$Open
                     , EOM = model.matrix(~EOM:as.factor(MonthOfYear)+0, data = df)*df$Open
                     , MOD = df$MOD*df$Open
                     , GoodFridayHoliday = df$GoodFridayHoliday*df$Open
                     , StNicholasHoliday = (df$StNicholasHoliday+df$StNicholasHolidayp1+df$StNicholasHolidaym1)*df$Open
                     #, Payday        = (df$Payday+df$Paydaym1+df$Paydayp1+df$Paydayp2)*df$Open
                     , Paydaym1        = (df$Payday)*df$DayOfWeek %in% c(1:5)
                     , PromoDayOfWeek = model.matrix(~as.factor(Promo):as.factor(DayOfWeek)+0, data = df)*df$Open
                     , ObservedHoliday= (df$ObservedHoliday)* df$Open
                     , NationalHoliday= df$NationalHoliday*df$Open
                     , ChristmasWeek = df$ChristmasWeek*df$Open
                     , EasterHoliday = df$EasterHoliday*df$Open
                     #, StateHoliday  = model.matrix(~df$StateHoliday+0)[,-1]
                     , SchoolHoliday = df$SchoolHoliday *df$Open
                     #, DayOfMonth    = model.matrix(~as.factor(df$DayOfMonth)+0)[,-1]
                     #, MonthOfYear   = model.matrix(~df$MonthOfYear+0)[,-1]
                     #, WeekOfYear    = model.matrix(~df$WeekofYear+0)[,-1]
                     #, Year          = model.matrix(~df$Year+0)[,-1]
                     , Temp          = na.locf(df$Mean_TemperatureC)
                     , Wind          = na.locf(df$Mean_Wind_SpeedKm_h)
                     #, Precipit      = as.numeric(na.locf(df$Precipitationmm))
                     , Rain          = df$Rain
                     #, Snow          = df$Snow
                     #, Fog           = df$Fog
                     #, Hail          = df$Hail
                     #, Thunderstorm  = df$Thunderstorm
                     #, GT_rossmann   = log1p(df$Rossmann)*df$Open
                     #, GT_dm         = df$Dm
                     , RossmannAdj = (df$RossmannAdj)*df$Open
                     , RossmannAdjd1 = (df$RossmannAdjd1)*df$Open
                     , RossmannAdjd7 = (df$RossmannAdjd7)*df$Open
                     , RossmannAdjd14 = (df$RossmannAdjd14)*df$Open
                     #, DmAdj = (df$dmAdj)*df$Open
                     #, DmAdjd1 = (df$dmAdjd1)*df$Open
                     #, DmAdjd7 = (df$dmAdjd7)*df$Open
                     #, DmAdjd14 = (df$dmAdjd14)*df$Open
                     #, RossmannWAdj   = (df$RossmannWAdj)*df$Open
                     #, RossmannAdjDOWMM = model.matrix(~as.factor(DayOfWeek):log1p(df$RossmannAdj)+0, data = df)*df$Open
                     #, RossmannAdjPromoMM = model.matrix(~as.factor(Promo):log1p(df$RossmannAdj)+0, data = df)*df$Open
  )
  
  
  #  nzv_cols <- nearZeroVar(xreg, freqCut = 999.9/0.1)
  #  #cat("removing", nzv_cols, "from xreg \n")
  #  if(length(nzv_cols) > 0) xreg <- xreg[, -nzv_cols]
  trans <- preProcess(xreg, method=c("zv","center", "scale"))
  xreg.pca <- predict(trans, xreg)
  #cat("xreg struc", str(xreg))
  #vars2keep <-vif_func(xreg.pca, thresh=10, trace=T)
  #xreg <- subset(xreg.pca, select=vars2keep)
  #  xreg  <- data.frame(df$Date, xreg)

  return(xreg.pca)
  #return(xreg)
}

# Create matrix of numeric predictors for single-store ML methods. Receives dataframe, store#, start and end date
MakeXregML <- function(df, store, start.date, end.date){
  # to test the function open these and run in terminal
  #start.date <- startd
  #end.date <-  startf+horizon-1
  #store <- 909
  #df <- xreg.dt

  df <- df[Store==store & Date >= start.date & Date <= end.date]
  
  xreg <- data.frame( ChristmasHoliday = df$ChristmasHoliday#*df$Open
                      #,  Open          = df$Open
                      , Promo2    = as.numeric(df$Date>=df$Promo2SinceDate)
                      , Promo2Refresh = as.numeric((df$Date >= df$Promo2SinceDate) & df$Promo2Month)
                      , OpenYTD       = df$OpenYTD
                      , CompetitionEntrance <- as.numeric((df$Date >= df$CompetitionOpenSinceDate) & (df$Date < (df$CompetitionOpenSinceDate + CompetitionEffect)))
                      #, DayOfWeek     = model.matrix(~as.factor(DayOfWeek)+0, data=df)*df$Open
                      #, Promo         = df$Promo*df$Open
                      #, OrtXms = df$OrtXms * df$Open
                      #, Nov22 = df$Nov22*df$Open
                      #, FoolDay=df$FoolDay*df$Open
                      , EOM = model.matrix(~EOM:as.factor(MonthOfYear)+0, data = df)*df$Open
                      , MOD = df$MOD*df$Open
                      , GoodFridayHoliday = df$GoodFridayHoliday*df$Open
                      , StNicholasHoliday = (df$StNicholasHoliday+df$StNicholasHolidayp1+df$StNicholasHolidaym1)*df$Open
                      #, Payday        = (df$Payday+df$Paydaym1+df$Paydayp1+df$Paydayp2)*df$Open
                      , Paydaym1        = (df$Payday)*df$DayOfWeek %in% c(1:5)
                      , PromoDayOfWeek = model.matrix(~as.factor(Promo):as.factor(DayOfWeek)+0, data = df)*df$Open
                      , ObservedHoliday= (df$ObservedHoliday)* df$Open
                      , NationalHoliday= df$NationalHoliday*df$Open
                      , ChristmasWeek = df$ChristmasWeek*df$Open
                      , EasterHoliday = df$EasterHoliday*df$Open
                      #, StateHoliday  = model.matrix(~df$StateHoliday+0)[,-1]
                      , SchoolHoliday = df$SchoolHoliday *df$Open
                      #, DayOfMonth    = model.matrix(~as.factor(df$DayOfMonth)+0)[,-1]
                      #, MonthOfYear   = model.matrix(~df$MonthOfYear+0)[,-1]
                      #, WeekOfYear    = model.matrix(~df$WeekofYear+0)[,-1]
                      #, Year          = model.matrix(~df$Year+0)[,-1]
                      , Temp          = na.locf(df$Mean_TemperatureC)
                      , Wind          = na.locf(df$Mean_Wind_SpeedKm_h)
                      , Precipit      = as.numeric(na.locf(df$Precipitationmm))
                      , Rain          = df$Rain
                      #, Snow          = df$Snow
                      #, Fog           = df$Fog
                      #, Hail          = df$Hail
                      #, Thunderstorm  = df$Thunderstorm
                      , RefurbPeriod = df$RefurbPeriod
                      , SaleStarts  = df$SaleStarts
                      , FireSale  = df$FireSale
                      , PostRefurbp02 = df$PostRefurb+df$PostRefurbp1+df$PostRefurbp2
                      , PostRefurbp35 = df$PostRefurbp3+df$PostRefurbp4+df$PostRefurbp5 
                      , RossmannAdj = (df$RossmannAdj)*df$Open
                      , RossmannAdjd1 = (df$RossmannAdjd1)*df$Open
                      , RossmannAdjd7 = (df$RossmannAdjd7)*df$Open
                      , RossmannAdjd14 = (df$RossmannAdjd14)*df$Open
                      , DmAdj = (df$dmAdj)*df$Open
                      , DmAdjd1 = (df$dmAdjd1)*df$Open
                      , DmAdjd7 = (df$dmAdjd7)*df$Open
                      , DmAdjd14 = (df$dmAdjd14)*df$Open
                      , ESL1S = na.locf(df$ESL1S)
                      , ESL1C = na.locf(df$ESL1C)
  )
  
  
  #  nzv_cols <- nearZeroVar(xreg, freqCut = 999.9/0.1)
  #  #cat("removing", nzv_cols, "from xreg \n")
  #  if(length(nzv_cols) > 0) xreg <- xreg[, -nzv_cols]
  trans <- preProcess(xreg, method=c("zv","center", "scale"))
  xreg.pca <- predict(trans, xreg)
  #cat("xreg struc", str(xreg))
  #vars2keep <-vif_func(xreg.pca, thresh=10, trace=T)
  #xreg <- subset(xreg.pca, select=vars2keep)
  #  xreg  <- data.frame(df$Date, xreg)
  return(xreg.pca)
  #return(xreg)
}

# Create xreg matrix for ML algorithms
MakeXregIID <- function(df, store, start.date, end.date){
  # to test the function open these and run in terminal
  #start.date <- startd
  #end.date <-  startf+horizon-1
  #store <- 909
  #df <- xreg.dt
  cat("Start Making xreg for", store)
  df <- df[Store==store & Date >= start.date & Date <= end.date]
  
  xreg <- data.frame( ChristmasHoliday = df$ChristmasHoliday#*df$Open
                      #,  Open          = df$Open
                      , Promo2    = as.numeric(df$Date>=df$Promo2SinceDate)
                      , Promo2Refresh = as.numeric((df$Date >= df$Promo2SinceDate) & df$Promo2Month)
                      , OpenYTD       = df$OpenYTD
                      , CompetitionEntrance <- as.numeric((df$Date >= df$CompetitionOpenSinceDate) & (df$Date < (df$CompetitionOpenSinceDate + CompetitionEffect)))
                      #, DayOfWeek     = model.matrix(~as.factor(DayOfWeek)+0, data=df)*df$Open
                      #, Promo         = df$Promo*df$Open
                      #, OrtXms = df$OrtXms * df$Open
                      #, Nov22 = df$Nov22*df$Open
                      #, FoolDay=df$FoolDay*df$Open
                      , EOM = model.matrix(~EOM:as.factor(MonthOfYear)+0, data = df)*df$Open
                      , MOD = df$MOD*df$Open
                      , GoodFridayHoliday = df$GoodFridayHoliday*df$Open
                      , StNicholasHoliday = (df$StNicholasHoliday+df$StNicholasHolidayp1+df$StNicholasHolidaym1)*df$Open
                      #, Payday        = (df$Payday+df$Paydaym1+df$Paydayp1+df$Paydayp2)*df$Open
                      , Paydaym1        = (df$Payday)*df$DayOfWeek %in% c(1:5)
                      , PromoDayOfWeek = model.matrix(~as.factor(Promo):as.factor(DayOfWeek)+0, data = df)*df$Open
                      , ObservedHoliday= (df$ObservedHoliday)* df$Open
                      , NationalHoliday= df$NationalHoliday*df$Open
                      , ChristmasWeek = df$ChristmasWeek*df$Open
                      , EasterHoliday = df$EasterHoliday*df$Open
                      #, StateHoliday  = model.matrix(~df$StateHoliday+0)[,-1]
                      , SchoolHoliday = df$SchoolHoliday *df$Open
                      #, DayOfMonth    = model.matrix(~as.factor(df$DayOfMonth)+0)[,-1]
                      #, MonthOfYear   = model.matrix(~df$MonthOfYear+0)[,-1]
                      #, WeekOfYear    = model.matrix(~df$WeekofYear+0)[,-1]
                      #, Year          = model.matrix(~df$Year+0)[,-1]
                      , Temp          = na.locf(df$Mean_TemperatureC)
                      , Wind          = na.locf(df$Mean_Wind_SpeedKm_h)
                      , Precipit      = as.numeric(na.locf(df$Precipitationmm))
                      , Rain          = df$Rain
                      #, Snow          = df$Snow
                      #, Fog           = df$Fog
                      #, Hail          = df$Hail
                      #, Thunderstorm  = df$Thunderstorm
                      , RefurbPeriod = df$RefurbPeriod
                      , SaleStarts  = df$SaleStarts
                      , FireSale  = df$FireSale
                      , PostRefurbp02 = df$PostRefurb+df$PostRefurbp1+df$PostRefurbp2
                      , PostRefurbp35 = df$PostRefurbp3+df$PostRefurbp4+df$PostRefurbp5 
                      , RossmannAdj = (df$RossmannAdj)*df$Open
                      , RossmannAdjd1 = (df$RossmannAdjd1)*df$Open
                      , RossmannAdjd7 = (df$RossmannAdjd7)*df$Open
                      , RossmannAdjd14 = (df$RossmannAdjd14)*df$Open
                      , DmAdj = (df$dmAdj)*df$Open
                      , DmAdjd1 = (df$dmAdjd1)*df$Open
                      , DmAdjd7 = (df$dmAdjd7)*df$Open
                      , DmAdjd14 = (df$dmAdjd14)*df$Open
                      
  )
  
  
  #  nzv_cols <- nearZeroVar(xreg, freqCut = 999.9/0.1)
  #  #cat("removing", nzv_cols, "from xreg \n")
  #  if(length(nzv_cols) > 0) xreg <- xreg[, -nzv_cols]
  trans <- preProcess(xreg, method=c("zv","center", "scale"))
  xreg.pca <- predict(trans, xreg)
  #cat("xreg struc", str(xreg))
  #vars2keep <-vif_func(xreg.pca, thresh=10, trace=T)
  #xreg <- subset(xreg.pca, select=vars2keep)
  #  xreg  <- data.frame(df$Date, xreg)
  return(xreg.pca)
  #return(xreg)
}

# function for loading the result files
LoadRes <- function(fpath, files, colnam){
  resAll <- NULL
  for (i in 1:length(files)){
    res <- read_csv(paste0(fpath, files[i], collapse = ""))
    resAll <- rbind(resAll, res)
  }
  names(resAll)[3] <- colnam
  return(resAll)
}

# function for saving result files, generates timestamped filenames
savefiles <- function(preddf, modname){
  fname <- paste0(modname,format(Sys.timeDate(), "%Y%m%d-%H%M%S"), collapse = "-")
  write_csv(preddf, paste0(fname, "df.csv", collapse = ""))
  
  tst <- read_csv("test.csv", col_types = list(StateHoliday=col_factor(c("0","a","b","c"))))
  preddf_sub <- merge(tst, preddf, by=c("Store", "Date"))
  preddf_sub <- subset(preddf_sub, select=c("Id", "Sales"))
  write_csv(preddf_sub, paste0(fname, "res.csv", collapse = ""))
}