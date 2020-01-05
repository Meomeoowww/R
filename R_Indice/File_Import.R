#Import données en df

library(openxlsx)

miniech<-read.xlsx("Exemple Indice.xlsx", sheet= 5)

rechtri<-read.xlsx("Exemple Indice.xlsx", sheet= 6)

B21SQL<-read.xlsx("B21SQL.xlsx", sheet=3)

#Indice<-unique(rech[1])

#SP<-split(rech, rech$Partner)

#DP<-sapply(Indice, function(x) MMIRIQ(rech, Unit.Value))

MMIRIQ(miniech, Unit.Value)

MMIRIQ(rechtri, Unit.Value)