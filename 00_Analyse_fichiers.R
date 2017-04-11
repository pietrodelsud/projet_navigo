
### Lecture des fichiers
# setwd("/Volumes/HD Bastien/Data science/Formation ENSAE/Projet SNCF/Data/R dataFrame")
# mob1 <- read.csv("/Volumes/HD Bastien/Data science/Formation ENSAE/Projet SNCF/Data/Transactions_201501.csv",header=TRUE)

## test import dplyr

## definition du repertoire de travail
getwd()
setwd("/Volumes/HD Bastien/Data science/Formation ENSAE/Projet SNCF/Data/R dataFrame")

## installation de packages
# install.packages("dplyr")
# install.packages("data.table")
# install.packages("Hmisc")
# install.packages("survival")
# install.packages("date")
library(date)
library(Hmisc)
library(dplyr)
library(data.table)

## chargement du csv source avec fread (environ 10 minutes pour le mois de janvier)
#mobilite1 <-fread("/Volumes/HD Bastien/Data science/Formation ENSAE/Projet SNCF/Data/Transactions_201501.csv")
# dim(mobilite1)
# mobilite2 <-fread("/Volumes/HD Bastien/Data science/Formation ENSAE/Projet SNCF/Data/Transactions_201502.csv")
# dim(mobilite2)
mobilite3 <-fread("/Volumes/HD Bastien/Data science/Formation ENSAE/Projet SNCF/Data/Transactions_201503.csv")
# dim(mobilite3)

# mobilite <-rbind(mobilite1,mobilite2,mobilite3)

##echantillonage des 10 000 premières lignes
# mobilite1_light <- mobilite1[1:10000,]

#sauvegarde de l'echantillon
# save(mobilite1_light,file="mob_janvier10000.Rda")

#chargemetn de l'échantillon sauvegardé
# mobilite1_light <- load("mob_janvier10000.Rda")
# dim(mobilite1_light)

# mise au format pour dplyr
# mob1 <- as.tbl(mobilite1_light) 

#   OU   table Entière
# mob1 <- as.tbl(mobilite1) 
# mob1 <- as.tbl(mobilite2) 
mob1 <- as.tbl(mobilite3) 

#### selection des pass telebilletique

mob_Nav1 <- mob1 %>%
  filter(TEVA_IDE_TEC_VAL==1 & VALI_IDE_VER==1)

### selection des colonnes utiles 
mobjan <- mob_Nav1 [,c(1,3:7,14:15,20,22:24,27:28,31:33)]

# TEVA_IDE_TEC_VAL : Identifiant de la technologie de validation
# COZO_IDE_CPL_ZON : Identifiant couple de zones
# PRTA_IDE_PDT_TAR : IIdentifiant produit 
# ECS_IDE_ECS :Identifiant ECS => permet de savoir à quelle gare c'est rattaché.
# EQVA_IDE_EQT_VAL : Identifiant  equipement de validation =>  permet de retrouver le type de CAB (G1 = tourniquet, M1 = porte effaçable, borne = les bornes jaunes qui n'empechent pas le passage)
# SEPA_IDE_SEN_PAS : sens de passage
# TRSC_IDE_CAR : numero de carte
# TRSC_DAT_VAL : date & heure de validation
# TRSC_FAMI : type de CAB
# VALI_VER : UTILE à fusionner pour retrouver les validations succès (cf code SAS fanette)
# PRTA_IDE_PDT_TAR : Indicateur titre spécial => titre tarifaire
# VALI_IDE_VER : UTILE à fusionner pour retrouver les validations succès (cf code SAS fanette)
# TRSC_ANNEE : 
# TRSC_NB_TRSC : nombre de validations (dans cette table, c'est toujours égal à 1. On part de cette variable pour faire des summary)
# TRSC_DT_VAL_JOUR : jour
# TRSC_DT_VAL_MOIS : mois 
# TRSC_DT_VAL_ANNEE : annee

# summary(mobjan)

# table(mobjan$COZO_IDE_CPL_ZON)
# 0 : inconnu  
# 3 : Zone 1-2
# 6 : Zone 2-3
# 7 : Zone 1-3
# 12 : Zone 3-4
# 14 : Zone 2-4
# 15 : Zone 1-4 
# 24 : Zone 4-5
# 28 : Zone 3-5
# 30 : Zone 2-5
# 31 : Zone 1-5
# 56 : Zone 4-6
# 63 : Zone 1-6
# 127 : Zone 1-7
# 255 : Zone 1-8

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
# mobjan$TRSC_DT_VAL_MOIS <- Hmisc::Lag(mobjan$TRSC_DT_VAL_MOIS,-1)
mobjan$TRSC_DT_VAL_ANNEE2 <- Hmisc::Lag(mobjan$TRSC_DT_VAL_ANNEE,-1)

## Création variable tronçon
mobjan$tron <- paste(mobjan$SEPA_IDE_SEN_PAS,mobjan$SEPA_IDE_SEN_PAS2,sep="")


##
mobjan2 <- mobjan %>%
  filter(TRSC_IDE_CAR == TRSC_IDE_CAR2 & TRSC_DT_VAL_JOUR==TRSC_DT_VAL_JOUR2 &
           TRSC_DT_VAL_MOIS == TRSC_DT_VAL_MOIS2 &
           TRSC_DT_VAL_ANNEE == TRSC_DT_VAL_ANNEE2
  )

table(mobjan2$tron)
100*(table(mobjan2$tron)/nrow(mobjan2))

### choix des tronçons retenus
sel <-c("12","13","14","32","33","34")

mobjan3 <- mobjan2[mobjan2$tron %in% sel,]

# calcul durée tronçon

mobjan3$timestampIn <- strptime(mobjan3$TRSC_DAT_VAL,"%Y-%m-%d %H:%M:%S")
mobjan3$timestampOut <- strptime(mobjan3$TRSC_DAT_VAL2,"%Y-%m-%d %H:%M:%S")

mobjan3$duree <- difftime(mobjan3$timestampOut, mobjan3$timestampIn,units = 'mins')

# saveRDS(mobjan3,file="Janvier2015.Rda")
# saveRDS(mobjan3,file="Fevrier2015.Rda")
# saveRDS(mobjan3,file="Mars2015.Rda")



#lecture fichiers
# Jan <- readRDS("Janvier2015.Rda")
# Fev <- readRDS("Fevrier2015.Rda")
# Mars <- readRDS("Mars2015.Rda")

## concaténation des 3 mois 
# mobt1 <- rbind(Jan,Fev,Mars)

# saveRDS(mobt1,file="trim1_2015.Rda")
mobt1 <- readRDS("trim1_2015.Rda")
  
## Quelques stats
# hist(as.numeric(mobjan3$duree))

summary(as.numeric(mobt1$duree))
quantile(as.numeric(mobt1$duree),probs = c(0.8,0.90,0.95),na.rm = TRUE)


aggregate(round(as.numeric(mobt1$duree),digits = 2),list(Parcours = mobt1$tron),function(x) summary(x))


