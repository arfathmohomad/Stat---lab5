setwd("C:\\Users\\IT18141948\\Desktop\\PS")
getwd()
data<-read.table("Data.txt",header = TRUE,sep = ",")
fix(data)
names(data)<-c("X1","X2")
attach(data)
hist(X2,main="Histrogram for Number Of ShareHolders")
histogram <- hist(X2,main="Histrogram for ShareHolders",seq(130,270,length=8))
breaks<-round(histogram$breaks)
breaks
classes<-c()
for(i in 1:length(breaks)-1){
  classes[i]<-paste0(breaks[i],",",breaks[i+1])
}
classes
feq<-histogram$counts
feq

mid<-histogram$mids
mid

cbind(class= classes,Fequency=feq)
lines(histogram$mids,feq)


plot(histogram$mids,feq,type = '1',main = "Freq.polygon",
     xlab = "shareholders",ylab = "Fequency",
     ylim = c(0,max(feq)))

cum.feq<-cumsum(feq)
cum.feq

new<-c()
for(i in 1:length(breaks)){
  if(i ==1){
    new [i] = 0 
  }else{
    new[i] = cum.feq[i-1]
  }
}
plot(breaks, new , type='o', main = "Frequency Polygon for shareholders",xlab = "Shareholders",ylab = "Cultimative Frequency",ylim = c(0,max(cum.feq)))

cbind(Upper = breaks,cum.feq = new)

abline(h = sum(cum.feq)*0.75,v = 207)


detach(data)

