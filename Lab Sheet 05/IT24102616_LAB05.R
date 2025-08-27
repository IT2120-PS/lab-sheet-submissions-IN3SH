setwd("C:\\Users\\it24102616\\Desktop\\IT24102616")
#pt1
Delivery_Times = read.table("Exercise - Lab 05.txt",header=TRUE)

fix(Delivery_Times)
names(Delivery_Times) = c("Delivery_Time")
attach(Delivery_Times)

#pt2
histogram = hist(Delivery_Time,main="Histogram for deliver times",breaks=seq(20,70,length=9),right=TRUE)

#pt3
install.packages("e1071")
library(e1071)

# Calculate skewness
skewness(Delivery_Times$Delivery_Time)

#pt4
breaks = round(histogram$breaks)
freq = histogram$counts
mids = histogram$mids
classes = c()

for (i in 1:length(breaks)-1) {
  classes[i] <- paste0("[", breaks[i], ",", breaks[i+1], ")")
}

cbind(classes = classes, Frequency = freq)


cum.freq <- cumsum(freq)
new <- c()

for(i in 1:length(breaks)){
  if(i==1){
    new[i] = 0
  }else{
    new[i] = cum.freq[i-1]
  }
}

plot(breaks, new, type = "l", main = "Cumalative Frequancy polygon for deliver times",
     xlab = "deliver times", ylab = "Frequancy", ylim = c(0,max(cum.freq)))                  
cbind(Upper = breaks, CumFreq = new)



