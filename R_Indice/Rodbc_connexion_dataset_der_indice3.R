#Connexion et import dans res
dbconnection <- odbcDriverConnect('Driver=SQL Server; Server=DESKTOP-O7N9DVH\\EUROTRACE; Database=SQLBENIN; trusted_connection=yes')
res <- sqlQuery(dbconnection, "SELECT [Year], [Period],[FLOW],[HS2012] as SH4, CONVERT(NUMERIC, sum([VALEURINSAE])) as VALEUR, CONVERT(NUMERIC, sum([POIDNET])) as POIDSNET, CONVERT(NUMERIC, sum([QUANTITE])) as QUANTITE FROM [dbo].[SQLBENIN_DATA_INDICE3] WHERE [Period] in ('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12') and [TRADETYPE] in('2', 'G') and [FLOW] in ('I', 'E', 'R') and LEN([HS2012])=4 group by [Year],[Period],[FLOW],[HS2012]", as.is=T)
close(dbconnection)

# Préparation des données
res2<-data.frame(Year=res$Year, Period=res$Period, FLOW = gsub("R", "ET", gsub("E","ET", res$FLOW)), SH4=res$SH4, VALEUR=as.numeric(res$VALEUR), POIDSNET=as.numeric(res$POIDSNET), QUANTITE=as.numeric(res$QUANTITE))
res3<- aggregate(res2[,c("VALEUR","POIDSNET","QUANTITE")],by=list(Year = res2$Year, Period =  res2$Period, FLOW = res2$FLOW, SH4 = res2$SH4), sum, na.rm=TRUE)
res4<-data.frame(Year = res3$Year,Period = res3$Period, FLOW = res3$FLOW, SH4 = res3$SH4, VU=ifelse(res3$SH4!="2716", res3$VALEUR/res3$POIDSNET, res3$VALEUR/res3$QUANTITE))