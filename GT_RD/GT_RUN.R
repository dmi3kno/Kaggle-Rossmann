#Start by copying these functions in R. 
#Then run the following code:
#NB! In order for the code to run properly, you will have to specify the download directory of your default browser (downloadDir)

downloadDir="C://Users//dmyp//Downloads"

url=vector()
filePath=vector()
adjustedWeekly=data.frame()
keyword="rossmann"


#Create URLs to daily data
for(i in 1:12){
  url[i]=URL_GT(keyword, year=2013, month=i, length=1)
}

#Download 
for(i in 1:length(url)){
  filePath[i]=downloadGT(url[i], downloadDir)
}

dailyData=readGT(filePath)
dailyData=dailyData[order(dailyData$Date),]

#Get weekly data
url=URL_GT(keyword, year=2013, month=1, length=12)
filePath=downloadGT(url, downloadDir)
weeklyData=readGT(filePath)

adjustedDaily=dailyData[1:2]
adjustedDaily=merge(adjustedDaily, weeklyData[1:2], by="Date", all=T)
adjustedDaily[4:5]=NA
names(adjustedDaily)=c("Date", "Daily", "Weekly", "Adjustment_factor", "Adjusted_daily")

#Adjust for date missmatch
for(i in 1:nrow(adjustedDaily)){
  if(is.na(adjustedDaily$Daily[i])) adjustedDaily$Daily[i]=adjustedDaily$Daily[i-1]
}

#Create adjustment factor
adjustedDaily$Adjustment_factor=adjustedDaily$Weekly/adjustedDaily$Daily

#Remove data before first available adjustment factor
start=which(is.finite(adjustedDaily$Adjustment_factor))[1]
stop=nrow(adjustedDaily)
adjustedDaily=adjustedDaily[start:stop,]

#Fill in missing adjustment factors
for(i in 1:nrow(adjustedDaily)){
  if(is.na(adjustedDaily$Adjustment_factor[i])) adjustedDaily$Adjustment_factor[i]=adjustedDaily$Adjustment_factor[i-1]
}

#Calculated adjusted daily values
adjustedDaily$Adjusted_daily=adjustedDaily$Daily*adjustedDaily$Adjustment_factor

#Plot the results
library(ggplot2)
ggplot(adjustedDaily, aes(x=Date, y=Adjusted_daily))+geom_line(col="blue")+ggtitle("SVI for Google Trends")