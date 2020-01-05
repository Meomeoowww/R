as.vector(by(ages[c("Age","W")],
             list(ages$Indiv),
             function(x) {
               do.call(weighted.mean, unname(x))
             }
))


DOZO <- as.data.frame.vector(by(my_data2[c("PONDERATION", "Ind_Prod_Elt")], 
              list(my_data2$Flux, my_data2$Annee, my_data2$Mois, my_data2$SH2),
              function(x) {
                do.call(weighted.mean, unname(x))
              }
              
))

library(dplyr)
my_data2 %>% 
  group_by(Flux, Annee, Mois, SH2) %>% 
  summarise(mean_80 = weighted.mean(Ind_Prod_Elt, PONDERATION)/sum(PONDERATION))

my_data2 %>% 
  group_by(Flux, Annee, Mois, SH2, Libellé_SH2) %>% 
  summarise(mean_80 = weighted.mean(Ind_Prod_Elt, PONDERATION)*sum(PONDERATION)/mean(PONDSH2))

DOZO<-my_data2 %>% 
  group_by(Flux, Temps, SH2, Libellé_SH2) %>% 
  summarise(mean_80 = weighted.mean(Ind_Prod_Elt, PONDERATION)*sum(PONDERATION)/mean(PONDSH2))

DOZO2<-my_data2 %>% 
  group_by(Flux, Temps, Libellé_Statut) %>% 
  summarise(mean_80 = weighted.mean(Ind_Prod_Elt, PONDERATION)*sum(PONDERATION)/mean(PONDSH2))

