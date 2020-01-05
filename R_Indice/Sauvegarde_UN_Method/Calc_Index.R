#Calcul de L'indice élémentaire

##################Export
#Mod ETL
ETL3<-ETL%>%
  mutate(NID=paste(BUREAU, MODTRANSPORT, PARTENAIRE, HS6, sep =""))%>%
  group_by(.$NID)%>%
  mutate(enn=n())%>%
  rowwise%>%
  mutate(d1= ifelse((Year!=2015 & enn==1),1,0))%>%
  filter(d1==0)#%>%
  #F_Index2(.)
  
ETL3<-ETL3 %>%
  mutate(HS4=substr(HS6,1,4), HS2=substr(HS6,1,2))%>%
  #left_join(.,ETL, by=c("Year"="Year", "Period"="Period", "BUREAU"="BUREAU", "MODTRANSPORT"="MODTRANSPORT", "PARTENAIRE"="PARTENAIRE", "HS6"="HS6"))%>%
  mutate(VT2=as.numeric(VT), PT2=as.numeric(PT), Med2=as.numeric(Med))%>%
  select(Year, Period, temps, BUREAU,MODTRANSPORT, PARTENAIRE, NID, HS6, HS4, HS2, Med2, VT2, PT2)

##################Import
#Mod ITL
ITL2<-ITL%>%
  mutate(NID = paste(BUREAU,MODTRANSPORT,PARTENAIRE,HS6, sep =""))#%>%
  #group_by(.$NID)%>%
  #mutate(enn=n())%>%
  #rowwise%>%
  #mutate(d1= ifelse((Year!=2015 & enn==1),1,0))%>%
  #filter(d1==0)%>%
  #F_Index2(.)

#Import
ITL2<-ITL2 %>%
  mutate(HS4=substr(ITL2$HS6,1,4), HS2=substr(ITL2$HS6,1,2))%>%
  left_join(.,ITL)%>%
  mutate(VT2=as.numeric(VT), PT2=as.numeric(PT), Med2=as.numeric(Med))%>%
  select(Year, Period, temps, BUREAU,MODTRANSPORT,PARTENAIRE,HS6, HS4, HS2, Med2, VT2, PT2)

#Vider mémoire
#rm(ETL,ITL,n, F_Index)