#Calcul de L'indice élémentaire2:Fonction Index
## Fonction Indice El
F_Index4 <- function(df) {
  if (min(df$Year)==2015) {
    Red<-vector(mode="double", length = nrow(df))
    for (i in 1:nrow(df))
      Red[i]<-as.numeric(df$Med2[i]) / as.numeric(df$Med2[which(df$Year==2015)])
  } 
  else {
    Red<-vector(mode="double", length = nrow(df))
    Red<-""}
  
  Mx<-data.frame(Year=df$Year, Period=df$Period, temps= df$temps, BUREAU=df$BUREAU, MODTRANSPORT=df$MODTRANSPORT, PARTENAIRE=df$PARTENAIRE, HS6=df$HS6, NID=df$NID, Red=as.factor(Red))
  return(Mx)
}