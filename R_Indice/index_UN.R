library(RODBC)
dbconnection <- odbcDriverConnect('Driver=SQL Server; Server=WIN-E1LOQVIE4GD\\EUROTRACE; Database=BENIN; trusted_connection=yes')
res <- sqlQuery(dbconnection, "SELECT [Year],[Period],[BUREAU],[REGIME],[FLOW],[MODTRANSPORT],[HS2012] as SH10,[PARTENAIRE], CONVERT(NUMERIC, sum([VALEURINSAE])) as VALEUR, CONVERT(NUMERIC, sum([POIDNET])) as POIDSNET, CONVERT(NUMERIC, sum([QUANTITE])) as QUANTITE FROM [dbo].[BENIN_DATA_INDEX] WHERE [Year] in (2010,2011, 2012, 2013, 2014, 2015, 2016, 2017) and [Period] in ('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12') and [TRADETYPE] in('2', 'G') and [FLOW] in ('I', 'E', 'R') and LEN([HS2012])=10 group by [Year],[Period],[BUREAU],[REGIME],[FLOW],[MODTRANSPORT],[HS2012],[PARTENAIRE]", as.is=T)
close(dbconnection)

library(openxlsx) #load the package
write.xlsx(x = res, file = paste("Data_Init", Sys.Date(), ".xlsx"),
           sheetName = "Data_Brute", col.NAmes = TRUE, row.Names = FALSE)

install.packages("univOutl")