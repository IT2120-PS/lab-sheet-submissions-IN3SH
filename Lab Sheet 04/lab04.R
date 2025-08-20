setwd("C:\\Users\\it24102616\\Desktop\\LAB04")
#01
branch_data<-read.table("Exercise.txt", header=TRUE, sep=",")
fix(branch_data)
attach(branch_data)

#02
sapply(branch_data, typeof)


#03
boxplot(Sales_X1,main="Box plot for Sales",outline = TRUE,outpch=8,horizontal=TRUE)


#04
summary(Advertising_X2)

IQR(Advertising_X2)

#05
get.outliners=function(x){
  q1 = quantile(x)[2]
  q3 = quantile(x)[4]
  iqr = q3-q1
  
  ub = q3 + 1.5*iqr
  lb = q1 - 1.5*iqr
  
  outliers = x[x < lb | x> ub]
  

  print(paste("Upper BOund =", ub))
  print(paste("Lower BOund =", lb))
  print(paste("Outliers:",paste(outliers, collapse = ", ")))
}

get.outliners(Years_X3)

