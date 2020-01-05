############CSV import Laspeyres
#si nécessaire
#install.packages("xts")
library(xts)

#library(seasonal)

Im_L0<-my_data3[my_data3$Flux=='I',]
Im_L1<-xts(Im_L0[,c("Ind_Lasp")], as.Date(Im_L0$temps, format="%Y-%m-%d"))
Im_L2<-ts(Im_L1, start=2010, frequency = 12)
CSV_Im_L3<-seas(Im_L2)
final(CSV_Im_L3)
plot(CSV_Im_L3)
summary(CSV_Im_L3)

############CSV export Laspeyres

Ex_L0<-my_data3[my_data3$Flux=='ET',]
Ex_L1<-xts(Ex_L0[,c("Ind_Lasp")], as.Date(Ex_L0$temps, format="%Y-%m-%d"))
Ex_L2<-ts(Ex_L1, start=2010, frequency = 12)
CSV_Ex_L3<-seas(Ex_L2)
final(CSV_Ex_L3)
plot(CSV_Ex_L3)
summary(CSV_Ex_L3)
