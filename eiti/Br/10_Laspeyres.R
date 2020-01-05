#library("dplyr")
##Nettoyage Dico
DIC_HS<-DIC_HS%>%
  select(Code, Libellé=Lib_FR)
DIC_SITC<-DIC_SITC%>%
  select(Code, Libellé=Lib_FR)

REL_HS2SITC<-REL_HS2SITC%>%
  mutate(HS6=substr(Code,1,6))
REL_HS2SITC<-REL_HS2SITC%>%
  select(HS6, TRS_SITC)%>%
  group_by(HS6, TRS_SITC)%>%
  summarise(.)


#####################################################EXPORTATION
#Expand sur la base des produits de l'année de base
E<-expand.grid(Year=unique(EDat$Year),Period=unique(EDat$Period),HS6=unique(EDat$HS6))
E1<-E%>%
  rowwise %>%
  mutate(d1= ifelse((Year==2015 & Period %in% c("01","02","03","04","05","06","07","08","09","10","11","12"))|!Year==2015 & Period=="00",1,0))%>%
  filter((d1==0)) %>% 
  select(Year, Period, HS6)%>%
  left_join(.,EDat)%>%
  arrange(Year, Period, HS6)

library("tidyverse")
E2<-E1 %>%
  group_by(HS6)%>%
  fill(Carli)%>%
  mutate(HS4=substr(HS6,1,4), HS2=substr(HS6,1,2))%>%
  group_by(HS6)%>%
  mutate(Pond0=sum(Pond10000[which(Year==2015)], na.rm = T))%>%
  ungroup(.)%>%
  group_by(HS2)%>%
  mutate(PondHS2=sum(Pond10000[which(Year==2015)], na.rm = T))%>%
  ungroup(.)%>%
  group_by(HS4)%>%
  mutate(PondHS4=sum(Pond10000[which(Year==2015)], na.rm = T))%>%
  left_join(.,REL_HS2SITC, by=c("HS6"="HS6"))%>%
  mutate(SITC1=substr(TRS_SITC,1,1), SITC2=substr(TRS_SITC,1,2))%>%
  ungroup(.)%>%
  group_by(SITC1)%>%
  mutate(PondSITC1=sum(Pond10000[which(Year==2015)], na.rm = T))%>%
  ungroup(.)%>%
  group_by(SITC2)%>%
  mutate(PondSITC2=sum(Pond10000[which(Year==2015)], na.rm = T))

#HS  
Exp_HS2_Lasp<- E2%>%
  group_by(Year,Period,HS2)%>%
  summarise(Export_Lasp_HS2=sum(Carli*Pond0, na.rm = T)/mean(PondHS2, na.rm = T), Pond=mean(PondHS2, na.rm = T))%>%
  mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))%>%
  left_join(.,DIC_HS, by=c("HS2"="Code"))
  
Exp_HS4_Lasp<- E2%>%
  group_by(Year,Period,HS4)%>%
  summarise(Export_Lasp_HS4=sum(Carli*Pond0, na.rm = T)/mean(PondHS4, na.rm = T), Pond=mean(PondHS4, na.rm = T))%>%
  mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))%>%
  left_join(.,DIC_HS, by=c("HS4"="Code"))

#SITC
Exp_SITC1_Lasp<- E2%>%
  group_by(Year,Period,SITC1)%>%
  summarise(Export_Lasp_SITC1=sum(Carli*Pond0, na.rm = T)/mean(PondSITC1, na.rm = T), Pond=mean(PondSITC1, na.rm = T))%>%
  mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))%>%
  left_join(.,DIC_SITC, by=c("SITC1"="Code"))

Exp_SITC2_Lasp<- E2%>%
  group_by(Year,Period,SITC2)%>%
  summarise(Export_Lasp_SITC2=sum(Carli*Pond0, na.rm = T)/mean(PondSITC2, na.rm = T), Pond=mean(PondSITC2, na.rm = T))%>%
  mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))%>%
  left_join(.,DIC_SITC, by=c("SITC2"="Code"))

#Global
Exp_Gl_Lasp<-E2%>%
  group_by(Year, Period)%>%
  summarize(Export_Lasp=sum(Carli*Pond0, na.rm = T)/10000)%>%
  mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))#%>%
  #filter(!is.na(temps))


#####################################################IMPORTATION  
#Expand sur la base des produits de l'année de base
I<-expand.grid(Year=unique(IDat$Year),Period=unique(IDat$Period),HS6=unique(IDat$HS6))
I1<-I%>%
  rowwise %>%
  mutate(d1= ifelse((Year==2015 & Period %in% c("01","02","03","04","05","06","07","08","09","10","11","12"))|!Year==2015 & Period=="00",1,0))%>%
  filter((d1==0)) %>% 
  select(Year, Period, HS6)%>%
  left_join(.,IDat)%>%
  arrange(Year, Period, HS6)

#library("tidyverse")
I2<-I1 %>%
  group_by(HS6)%>%
  fill(Carli)%>%
  mutate(HS4=substr(HS6,1,4), HS2=substr(HS6,1,2))%>%
  group_by(HS6)%>%
  mutate(Pond0=sum(Pond10000[which(Year==2015)], na.rm = T))%>%
  ungroup(.)%>%
  group_by(HS2)%>%
  mutate(PondHS2=sum(Pond10000[which(Year==2015)], na.rm = T))%>%
  ungroup(.)%>%
  group_by(HS4)%>%
  mutate(PondHS4=sum(Pond10000[which(Year==2015)], na.rm = T))%>%
  left_join(.,REL_HS2SITC, by=c("HS6"="HS6"))%>%
  mutate(SITC1=substr(TRS_SITC,1,1), SITC2=substr(TRS_SITC,1,2))%>%
  ungroup(.)%>%
  group_by(SITC1)%>%
  mutate(PondSITC1=sum(Pond10000[which(Year==2015)], na.rm = T))%>%
  ungroup(.)%>%
  group_by(SITC2)%>%
  mutate(PondSITC2=sum(Pond10000[which(Year==2015)], na.rm = T))
         
#HS
Imp_HS2_Lasp<- I2%>%
  group_by(Year,Period,HS2)%>%
  summarise(Import_Lasp_HS2=sum(Carli*Pond0, na.rm = T)/mean(PondHS2, na.rm = T), Pond=mean(PondHS2, na.rm = T))%>%
  mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))%>%
  left_join(.,DIC_HS, by=c("HS2"="Code"))

  Imp_HS4_Lasp<- E2%>%
  group_by(Year,Period,HS4)%>%
  summarise(Export_Lasp_HS4=sum(Carli*Pond0, na.rm = T)/mean(PondHS4, na.rm = T), Pond=mean(PondHS4, na.rm = T))%>%
  mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))%>%
  left_join(.,DIC_HS, by=c("HS4"="Code"))
#SITC
  Imp_SITC1_Lasp<- I2%>%
    group_by(Year,Period,SITC1)%>%
    summarise(Import_Lasp_SITC1=sum(Carli*Pond0, na.rm = T)/mean(PondSITC1, na.rm = T), Pond=mean(PondSITC1, na.rm = T))%>%
    mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))%>%
    left_join(.,DIC_SITC, by=c("SITC1"="Code"))
  
  Imp_SITC2_Lasp<- I2%>%
    group_by(Year,Period,SITC2)%>%
    summarise(Import_Lasp_SITC2=sum(Carli*Pond0)/mean(PondSITC2), Pond=mean(PondSITC2))%>%
    mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))%>%
    left_join(.,DIC_SITC, by=c("SITC2"="Code"))
  
#Global
Imp_Gl_Lasp<-I2%>%
  group_by(Year, Period)%>%
  summarize(Import_Lasp=sum(Carli*Pond0, na.rm = T)/10000)%>%
  mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))#%>%
#filter(!is.na(temps))

rm(E,E1,E2,EPonderation,ETL,ETL2,I,I1,I2,IPonderation,ITL,ITL2,dbconnection,F_Index4)
