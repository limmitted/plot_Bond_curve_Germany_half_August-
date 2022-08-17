# 1. German Federal Zero-Coupon Bond Curve/ discount Curve for first August half of 2022
#(P_{t} (??) : ?? > 0) 
#(https://www.bundesbank.de/en/service/federal-securities/prices-and-yields)
library("readxl")
library("pillar")

#(representation of the term structure of how interest rates/bond prices vary with maturity)
# t = 01.08.22-15.08.22 (11 dates)
# ?? = 04.09.22-15.08.2052 (22 dates)

Y=matrix(nrow=11,ncol=35)
X=matrix(nrow=11,ncol=35)

for (i in 1:11){
my_data <- read_excel("2022-08-excel-data.xlsx",range = "A4:K77", sheet = i,col_names = TRUE)
t=subset(my_data, my_data$...4 == 0)
colnames(t) =(c(1,2,3,4,5,6,7,8,9,10,11))
if(i<7){ # account for 1 additional in second half of sheets of .xlsx file
Y[i,] = c(t$`9`,NA)
dates= c(t$`6`,NA)
X[i,]= dates
}
else{
  Y[i,] = c(t$`9`)
  dates= (t$`6`)
  X[i,]=dates
  }
}

###plot Bond curve 
t1=1 #start date position index for plot (make sure difference between t1 and t2 is 3)
t2=34 #end date position index for plot (max=34)  
s=t2-t1
Xd=X[,t1:t2]
Yd=Y[,t1:t2]
a =as.Date(Xd[1,1] ,format="%d.%m.%Y")
b =as.Date(Xd[1,(s-1)] ,format="%d.%m.%Y")
plot(as.Date(Xd[1,1:(s-1)], format="%d.%m.%Y"),                            
     rep(0,s-1),
     type = "b",
     col = 2,
     ylim = c(min(Yd), max(Yd)),
     Xlim = as.Date(c(a,b)),
     xlab = "Maturity Date",
     ylab = "Price of Bond")
for (j in 1:11){
lines(as.Date(Xd[j,], format="%d.%m.%Y"),                           
        Yd[j,],
        type = "b",
        col = j+1)  
}
legend("bottomleft",                           
       c("01.08.2022", "02.08.2022", "03.08.2022", "04.08.2022","05.08.2022",
         "08.08.2022","09.08.2022","10.08.2022","11.08.2022","12.08.2022","15.08.2022"),
       lty = 1,
       col = 2:12, ncol=3)
