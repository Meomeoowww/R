##Nettoyage Dico
#DIC_HS<-DIC_HS%>%
#  select(Code, Libellé=Lib_FR)
#DIC_SITC<-DIC_SITC%>%
#  select(Code, Libellé=Lib_FR)
#REL_HS2SITC<-REL_HS2SITC%>%
#  mutate(HS6=substr(Code,1,6))

#####################################################EXPORTATION
#Expand sur la base des produits de l'année de base
E<-expand.grid(Year=unique(EDat$Year),Period=unique(EDat$Period),HS6=unique(EDat$HS6))
E1<-E%>%
  rowwise %>%
  mutate(d1= ifelse((Year==2015 & Period %in% c("01","02","03","04","05","06","07","08","09","10","11","12"))|!Year==2015 & Period=="00",1,0))%>%
  filter((d1==0)) %>% 
  select(Year, Period, HS6)%>%
  left_join(.,EDat)%>%
  arrange(Year, Period, HS6)%>%
  mutate(Pond10000 = if_else(is.na(Pond10000), 0, Pond10000))

library("tidyverse")
E2<-E1 %>%
  group_by(HS6)%>%
  fill(Carli)%>%
  mutate(HS4=substr(HS6,1,4), HS2=substr(HS6,1,2))%>%
  ungroup(.)%>%
  group_by(Year, Period)%>%
  mutate(Pondt=sum(Pond10000, na.rm=TRUE))%>%
  ungroup(.)%>%
  group_by(Year, Period, HS2)%>%
  mutate(PondHS2=sum(Pond10000, na.rm=TRUE))%>%
  ungroup(.)%>%
  group_by(Year, Period,HS4)%>%
  mutate(PondHS4=sum(Pond10000, na.rm=TRUE))%>%
  left_join(.,REL_HS2SITC, by=c("HS6"="HS6"))%>%
  mutate(SITC1=substr(TRS_SITC,1,1), SITC2=substr(TRS_SITC,1,2))%>%
  ungroup(.)%>%
  group_by(Year, Period,SITC1)%>%
  mutate(PondSITC1=sum(Pond10000, na.rm=TRUE))%>%
  ungroup(.)%>%
  group_by(Year, Period,SITC2)%>%
  mutate(PondSITC2=sum(Pond10000, na.rm=TRUE))

#HS  
Exp_HS2_Paas<- E2%>%
  group_by(Year,Period,HS2)%>%
  summarise(Export_Paas_HS2=1/(mean(PondHS2, na.rm=TRUE)/mean(Pondt, na.rm=TRUE)/mean(Carli, na.rm=TRUE)), Pond=mean(PondHS2, na.rm=TRUE))%>%
  mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))%>%
  left_join(.,DIC_HS, by=c("HS2"="Code"))

Exp_HS4_Paas<- E2%>%
  group_by(Year,Period,HS4)%>%
  summarise(Export_Paas_HS4=1/(mean(PondHS4, na.rm=TRUE)/mean(Pondt, na.rm=TRUE)/mean(Carli, na.rm=TRUE)), Pond=mean(PondHS4, na.rm=TRUE))%>%
  mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))%>%
  left_join(.,DIC_HS, by=c("HS4"="Code"))

#SITC
Exp_SITC1_Paas<- E2%>%
  group_by(Year,Period,SITC1)%>%
  summarise(Export_Paas_SITC1=1/(mean(PondSITC1, na.rm=TRUE)/mean(Pondt, na.rm=TRUE)/mean(Carli, na.rm=TRUE)), Pond=mean(PondSITC1, na.rm=TRUE))%>%
  mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))%>%
  left_join(.,DIC_SITC, by=c("SITC1"="Code"))

Exp_SITC2_Paas<- E2%>%
  group_by(Year,Period,SITC2)%>%
  summarise(Export_Paas_SITC2=1/(mean(PondSITC2, na.rm=TRUE)/mean(Pondt, na.rm=TRUE)/mean(Carli, na.rm=TRUE)), Pond=mean(PondSITC2, na.rm=TRUE))%>%
  mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))%>%
  left_join(.,DIC_SITC, by=c("SITC2"="Code"))

#Global
Exp_Gl_Paas<-E2%>%
  group_by(Year, Period)%>%
  summarise(Export_Paas=1/sum(Pond10000/Pondt/Carli, na.rm=TRUE), Pond=sum(Pond10000, na.rm=TRUE))%>%
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
  arrange(Year, Period, HS6)%>%
  mutate(Pond10000 = if_else(is.na(Pond10000), 0, Pond10000))

library("tidyverse")
I2<-I1 %>%
  group_by(HS6)%>%
  fill(Carli)%>%
  mutate(HS4=substr(HS6,1,4), HS2=substr(HS6,1,2))%>%
  ungroup(.)%>%
  group_by(Year, Period)%>%
  mutate(Pondt=sum(Pond10000, na.rm=TRUE))%>%
  ungroup(.)%>%
  group_by(Year, Period, HS2)%>%
  mutate(PondHS2=sum(Pond10000, na.rm=TRUE))%>%
  ungroup(.)%>%
  group_by(Year, Period,HS4)%>%
  mutate(PondHS4=sum(Pond10000, na.rm=TRUE))%>%
  left_join(.,REL_HS2SITC, by=c("HS6"="HS6"))%>%
  mutate(SITC1=substr(TRS_SITC,1,1), SITC2=substr(TRS_SITC,1,2))%>%
  ungroup(.)%>%
  group_by(Year, Period,SITC1)%>%
  mutate(PondSITC1=sum(Pond10000, na.rm=TRUE))%>%
  ungroup(.)%>%
  group_by(Year, Period,SITC2)%>%
  mutate(PondSITC2=sum(Pond10000, na.rm=TRUE))

#HS  
Imp_HS2_Paas<- I2%>%
  group_by(Year,Period,HS2)%>%
  summarise(Import_Paas_HS2=1/(mean(PondHS2, na.rm=TRUE)/mean(Pondt, na.rm=TRUE)/mean(Carli, na.rm=TRUE)), Pond=mean(PondHS2, na.rm=TRUE))%>%
  mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))%>%
  left_join(.,DIC_HS, by=c("HS2"="Code"))

Imp_HS4_Paas<- I2%>%
  group_by(Year,Period,HS4)%>%
  summarise(Import_Paas_HS4=1/(mean(PondHS4, na.rm=TRUE)/mean(Pondt, na.rm=TRUE)/mean(Carli, na.rm=TRUE)), Pond=mean(PondHS4, na.rm=TRUE))%>%
  mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))%>%
  left_join(.,DIC_HS, by=c("HS4"="Code"))

#SITC
Imp_SITC1_Paas<- I2%>%
  group_by(Year,Period,SITC1)%>%
  summarise(Import_Paas_SITC1=1/(mean(PondSITC1, na.rm=TRUE)/mean(Pondt, na.rm=TRUE)/mean(Carli, na.rm=TRUE)), Pond=mean(PondSITC1, na.rm=TRUE))%>%
  mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))%>%
  left_join(.,DIC_SITC, by=c("SITC1"="Code"))

Imp_SITC2_Paas<- I2%>%
  group_by(Year,Period,SITC2)%>%
  summarise(Import_Paas_SITC2=1/(mean(PondSITC2)/mean(Pondt)/mean(Carli)), Pond=mean(PondSITC2, na.rm=TRUE))%>%
  mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))%>%
  left_join(.,DIC_SITC, by=c("SITC2"="Code"))

#Global
Imp_Gl_Paas<-I2%>%
  group_by(Year, Period)%>%
  summarise(Import_Paas=1/sum(Pond10000/Pondt/Carli, na.rm=TRUE), Pond=sum(Pond10000, na.rm=TRUE))%>%
  mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))#%>%
#filter(!is.na(temps))

rm(DIC_HS,DIC_SITC,E,E1,E2,EDat,EPonderation,ETL,ETL2,I,I1,I2,IDat,IPonderation,ITL,ITL2,REL_HS2SITC,dbconnection,F_Index4)