require(xlsx)
require(timeDate)
raw<-read.xlsx("Sleep Experiment.xlsx", sheetIndex=1, header=T)

raw<-raw[!is.na(raw$Status),]

raw$Time<-timeDate(raw$Time)
n<-sum(raw$Status=="Initiated")-sum(raw$Status=="Void")

data<-data.frame(Date=rep(raw$Time[1], n), Delay = numeric(n), awake=rep(F, n))
names(data)=c("Date", "Delay", "Awake")

indices<-which(raw$Status=="Initiated")
counter=0

#This is all a bit cryptic but basically it translates the raw excel file to usable data by
#setting the initialization date as the date, calculating the date as the difference between alert and initialize,
#looking for a response to see if I was awake or not, and throwing out any voided entries.
for (i in 1:length(indices)){
  if (!any(raw$Status[indices[i]:ifelse(i==length(indices), nrow(raw),indices[i+1])]=="Void")){
    counter=1+counter
print(i)
    data$Date[counter]<-raw$Time[indices[i]]
    alert=which(raw$Status[indices[i]:ifelse(i==length(indices), nrow(raw), (indices[i+1]-1))]=="Alerted")+indices[i]-1
    data$Delay[counter]<-as.numeric(difftime(raw$Time[alert],raw$Time[indices[i]], units="mins"))
    if (any(raw$Status[indices[i]:ifelse(i==length(indices), nrow(raw),indices[i+1])]=="Awake")){
      data$Awake[counter]=TRUE
    }else{data$Awake[counter]=FALSE}
    
  }
}
data
data<-data[data$Delay<360,]
plot(data$Awake~data$Delay)
ggplot(data)+geom_point(aes(x=Date, y=Delay, color=Awake))
