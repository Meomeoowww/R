#Préparation ETL2/ITL2

##################Export
#Mod ETL
ETL2<-ETL%>%
  mutate(NID=paste(BUREAU, MODTRANSPORT, PARTENAIRE, HS6, sep =""))%>%
  #mutate(HS4=substr(HS6,1,4), HS2=substr(HS6,1,2))%>%
  mutate(VT2=as.numeric(VT), PT2=as.numeric(PT), Med2=as.numeric(Med))%>%
  select(Year, Period, temps, BUREAU, MODTRANSPORT, PARTENAIRE, NID, HS6, Med2, VT2, PT2)

##################Import
#Mod ITL
ITL2<-ITL%>%
  mutate(NID = paste(BUREAU,MODTRANSPORT,PARTENAIRE,HS6, sep =""))%>%
  #mutate(HS4=substr(HS6,1,4), HS2=substr(HS6,1,2))%>%
  mutate(VT2=as.numeric(VT), PT2=as.numeric(PT), Med2=as.numeric(Med))%>%
  select(Year, Period, temps, BUREAU,MODTRANSPORT,PARTENAIRE,NID, HS6, Med2, VT2, PT2)

#Vider mémoire
#rm(ETL,ITL,n, F_Index)