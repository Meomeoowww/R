## Fonction Indice El
F_Index2 <- function(df) {
  if (min(df$Year)==2015) {
    Red<-vector(mode="double", length = nrow(df))
    for (i in 1:nrow(df))
      Red[i]<-as.numeric(df$Med2[i]) / as.numeric(df$Med2[which(df$Year==2015)])
  } 
  else {
    Red<-vector(mode="double", length = nrow(df))
    Red<-""}
  
  Mx<-data.frame(Year=df$Year, Period=df$Period, temps= df$temps, BUREAU=df$BUREAU, MODTRANSPORT=df$MODTRANSPORT, PARTENAIRE=df$PARTENAIRE, HS6=df$HS6, Ind=df$NID, Red)
  return(Mx)
}