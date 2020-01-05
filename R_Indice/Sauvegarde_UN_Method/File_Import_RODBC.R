
#RODBC sur le Dataset principal
library(RODBC)
dbconnection <- odbcDriverConnect('Driver=SQL Server; Server=WIN-E1LOQVIE4GD\\EUROTRACE; Database=BENIN; trusted_connection=yes')
Import2K <- sqlQuery(dbconnection, "SELECT * FROM [BENIN].[dbo].[BENIN_DATA_BJCOMEXT] WHERE [Year] in (2015, 2016) and [Period] in ('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12') and [REGIME] like '4%' or [REGIME] like '5%' or [REGIME] like '6%' or [REGIME] like '7%' or [REGIME] like '9%'", as.is=T)
close(dbconnection)

library(dplyr)
Import2K<-Import2K%>% 
  mutate(., Unit.Value=ifelse(.$HS8BENIN!="27160000", as.numeric(.$VALEURINSAE)/as.numeric(.$POIDNET), as.numeric(.$VALEURINSAE)/as.numeric(.$QUANTITE)))
