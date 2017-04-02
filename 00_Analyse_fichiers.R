
### Lecture des fichiers
# setwd("/Volumes/HD Bastien/Data science/Formation ENSAE/Projet SNCF/Data/R dataFrame")
# mob1 <- read.csv("/Volumes/HD Bastien/Data science/Formation ENSAE/Projet SNCF/Data/Transactions_201501.csv",header=TRUE)

## test import dplyr

## definition du repertoire de travail
getwd()
setwd("/Volumes/HD Bastien/Data science/Formation ENSAE/Projet SNCF/Data/R dataFrame")

## installation de packages
install.packages("dplyr")
install.packages("data.table")
library(dplyr)
library(data.table)

## chargement du csv source avec fread (environ 10 minutes pour le mois de janvier)
mobilite1 <-fread("/Volumes/HD Bastien/Data science/Formation ENSAE/Projet SNCF/Data/Transactions_201501.csv")
dim(mobilite1)

##echantillonage des 10 000 premières lignes
mobilite1_light <- mobilite1[1:10000,]

#sauvegarde de l'echantillon
save(mobilite1_light,file="mob_janvier10000.Rda")

### echantillon à 100 000
tempmob <- mobilite1 %>%
  arrange(TRSC_IDE_CAR,TRSC_DAT_VAL ,SEPA_IDE_SEN_PAS)

mobilite1_light <- tempmob[1:100000,]
save(mobilite1_light,file="mob_janvier100000.Rda")

#1 000 000 lignes
mobilite1_light <- tempmob[1:1000000,]
save(mobilite1_light,file="mob_janvier1000000.Rda")

#chargemetn de l'échantillon sauvegardé
mobilite1_light <- load("mob_janvier10000.Rda")
dim(mobilite1_light)

# mise au format pour dplyr
mob1 <- as.tbl(mobilite1_light) 

#### selection des pass telebilletique

mob_Nav1 <- mob1 %>%
  filter(TEVA_IDE_TEC_VAL==1)

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

summary(mobjan)

## Stats sur les zonnes du pass
table(mobjan$COZO_IDE_CPL_ZON)
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

### 
table(mobjan$PRTA_IDE_PDT_TAR)
table(mobjan$ECS_IDE_ECS)
table(mobjan$EQVA_IDE_EQT_VAL)
table(mobjan$SEPA_IDE_SEN_PAS)
# 1 : Entrée
# 2 : sortie
# 3 : entrée correspondance 
# 4 : sortie correspondance

#
table(mobjan$TRSC_FAMI)
table(mobjan$PRTA_IDE_PDT_TAR)
table(mobjan$VALI_IDE_VER)
table(mobjan$TRSC_ANNEE)
table(mobjan$TRSC_DT_VAL_JOUR)
table(mobjan$TRSC_DT_VAL_MOIS)
table(mobjan$TRSC_DT_VAL_ANNEE)

##
t <- mobjan %>%
  arrange(TRSC_IDE_CAR,TRSC_DAT_VAL ,SEPA_IDE_SEN_PAS)
