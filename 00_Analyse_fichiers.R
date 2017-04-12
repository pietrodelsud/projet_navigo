library(date)
library(Hmisc)
library(dplyr)
library(data.table)

setwd("/Volumes/HD Bastien/Data science/Formation ENSAE/Projet SNCF/Data/R dataFrame")
getwd()

# n = 1 janvier, n=2 Février , n=3 Mars
charg <- function(n){
  
  m <- switch(n,
              "1"="/Volumes/HD Bastien/Data science/Formation ENSAE/Projet SNCF/Data/Transactions_201501.csv",
              "2"="/Volumes/HD Bastien/Data science/Formation ENSAE/Projet SNCF/Data/Transactions_201502.csv",
              "3"="/Volumes/HD Bastien/Data science/Formation ENSAE/Projet SNCF/Data/Transactions_201503.csv")
  
  mobilite <-fread(m)
  mob1 <- as.tbl(mobilite) 
  
  #### selection des pass telebilletique
  
  mob_Nav1 <- mob1 %>%
    filter(TEVA_IDE_TEC_VAL==1 & (
      ((TEVA_IDE_TEC_VAL==0) & (VALI_VER==1) & (VALI_IDE_VER==0)) |
        ((TEVA_IDE_TEC_VAL==1) & (VALI_VER==0) & (VALI_IDE_VER==0)) |
        ((TEVA_IDE_TEC_VAL==1) & (VALI_VER==1) & (VALI_IDE_VER==1)) |
        ((TEVA_IDE_TEC_VAL==1) & (VALI_VER==2) & (VALI_IDE_VER==1)) |
        ((TEVA_IDE_TEC_VAL==1) & (VALI_VER==3) & (VALI_IDE_VER==1)) |
        ((TEVA_IDE_TEC_VAL==1) & (VALI_VER==4) & (VALI_IDE_VER==1)) 
      
    ) 
    )
  
  rm(mob1,mobilite)
  
  ### selection des colonnes utiles 
  mobjan <- mob_Nav1 [,c(1,3:7,14:15,20,22:24,27:28,31:33)]
  
  ## tri par navigo - date heure - sens de passage
  mobjan <-mobjan %>%
    arrange(TRSC_IDE_CAR,TRSC_DAT_VAL ,SEPA_IDE_SEN_PAS)
  
  ### Solution Lag pour créer les tronçons
  ##
  # t <- mobjan[,c(4,6,7,8,15,16,17)]
  mobjan$TRSC_IDE_CAR <- as.character(mobjan$TRSC_IDE_CAR)
  
  
  ## Création tronçons
  
  mobjan$ECS_IDE_ECS2 <- Hmisc::Lag(mobjan$ECS_IDE_ECS,-1)
  mobjan$SEPA_IDE_SEN_PAS2 <- Hmisc::Lag(mobjan$SEPA_IDE_SEN_PAS,-1)
  mobjan$TRSC_IDE_CAR2 <- Hmisc::Lag(mobjan$TRSC_IDE_CAR,-1)
  mobjan$TRSC_DAT_VAL2 <- Hmisc::Lag(mobjan$TRSC_DAT_VAL,-1)
  mobjan$TRSC_DT_VAL_JOUR2 <- Hmisc::Lag(mobjan$TRSC_DT_VAL_JOUR,-1)
  mobjan$TRSC_DT_VAL_MOIS2 <- Hmisc::Lag(mobjan$TRSC_DT_VAL_MOIS,-1)
  mobjan$TRSC_DT_VAL_ANNEE2 <- Hmisc::Lag(mobjan$TRSC_DT_VAL_ANNEE,-1)
  
  ## Création variable tronçon
  mobjan$tron <- paste(mobjan$SEPA_IDE_SEN_PAS,mobjan$SEPA_IDE_SEN_PAS2,sep="")
  
  
  ##
  mobjan2 <- mobjan %>%
    filter(TRSC_IDE_CAR == TRSC_IDE_CAR2 & TRSC_DT_VAL_JOUR==TRSC_DT_VAL_JOUR2 &
             TRSC_DT_VAL_MOIS == TRSC_DT_VAL_MOIS2 &
             TRSC_DT_VAL_ANNEE == TRSC_DT_VAL_ANNEE2
    )
  
  ### choix des tronçons retenus
  sel <-c("12","13","14","32","33","34")
  
  mobjan3 <- mobjan2[mobjan2$tron %in% sel,]
  
  # calcul durée tronçon
  
  mobjan3$timestampIn <- strptime(mobjan3$TRSC_DAT_VAL,"%Y-%m-%d %H:%M:%S")
  mobjan3$timestampOut <- strptime(mobjan3$TRSC_DAT_VAL2,"%Y-%m-%d %H:%M:%S")
  
  mobjan3$duree <- difftime(mobjan3$timestampOut, mobjan3$timestampIn,units = 'mins')
  
  ## ecriture de sortie
  o <- switch(n,
              "1"="J2015.Rda",
              "2"="F2015.Rda",
              "3"="M2015.Rda")
  
  saveRDS(mobjan3,file=o)
  rm(mob_Nav1,mobjan2,mobjan3)
  
}

charg("1")
charg("2")
charg("3")


#lecture fichiers
Jan <- readRDS("J2015.Rda")
Fev <- readRDS("F2015.Rda")
Mars <- readRDS("M2015.Rda")

## concaténation des 3 mois 
mobt1 <- rbind(Jan,Fev,Mars)

saveRDS(mobt1,file="sncf_t1_2015.Rda")

mobt1 <- readRDS("sncf_t1_2015.Rda")


## Quelques stats
# hist(as.numeric(mobjan3$duree))

summary(as.numeric(mobt1$duree))
quantile(as.numeric(mobt1$duree),probs = c(0.8,0.90,0.95),na.rm = TRUE)


aggregate(round(as.numeric(mobt1$duree),digits = 2),list(Parcours = mobt1$tron),function(x) summary(x))




