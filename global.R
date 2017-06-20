options(encoding = "UTF-8")

library(data.table)
library(plyr)
library(dplyr)

lignee_4 <- readRDS("/Users/ingacarrelet/formation_CEPE/projet_groupe/shiny_fanette/lignee_4.rds")

lignee_5 <- readRDS("/Users/ingacarrelet/formation_CEPE/projet_groupe/shiny_fanette/lignee_5.rds")

#choixgare_e <- lignee_4 %>% distinct(GARE_E)
#choixgare_s <- lignee_4v %>% distinct(GARE_S)

#doublons <- which(duplicated(lignee_4$GARE_S)) #quelles sont les séquences en double dans la colonne V2 de mon jeu de données?
#GARE_S <- lignee_4[-doublons,] 

#GARE_S2 <- GARE_S [,c(4,6,8)]

#GARE_S2$n <- 1

      
#which(is.na(lignee_4))
 
base_valid <- readRDS("/Users/ingacarrelet/formation_CEPE/projet_groupe/gares_valid_jsem.Rds")


base_valid$libgar_orig_recod <- revalue(base_valid$libgar_orig,c("STADE DE FRANCE"="LA PLAINE-STADE DE FRANCE",
                                                                 "ST DENIS"="SAINT-DENIS",
                                                                 "PARIS NORD"="GARE DU NORD",
                                                                 "HAUSSMANN"="OPERA",
                                                                 "COURNEUVE"="LA COURNEUVE-AUBERVILLIERS",
                                                                 "AULNAY SS BOIS"="AULNAY-SOUS-BOIS",
                                                                 "AEROPORT CDG1"="AEROPORT CHARLES DE GAULLE 1"))

base_valid$jour_server <- revalue(base_valid$jsem,c("lundi"=1,"mardi"=2,"mercredi"=3,"jeudi"=4,"vendredi"=5,"samedi"=6,"dimanche"=7))
base_valid_tri <- base_valid %>% arrange(jour_server)

choixgare_dep_valid <- base_valid %>% distinct(libgar_orig_recod)

erreur <- readRDS("/Users/ingacarrelet/formation_CEPE/projet_groupe/Erreur_echantillon_Foret")

erreur_df <- as.data.frame(erreur)

erreur_df$garorig_recod<- revalue(erreur_df$garorig,c("STADE DE FRANCE"="LA PLAINE-STADE DE FRANCE",
                                                      "ST DENIS"="SAINT-DENIS",
                                                      "PARIS NORD"="GARE DU NORD",
                                                      "HAUSSMANN"="OPERA",
                                                      "COURNEUVE"="LA COURNEUVE-AUBERVILLIERS",
                                                      "AULNAY SS BOIS"="AULNAY-SOUS-BOIS",
                                                      "AEROPORT CDG1"="AEROPORT CHARLES DE GAULLE 1"))

erreur_df$garedest_recod<- revalue(erreur_df$garedest,c("STADE DE FRANCE"="LA PLAINE-STADE DE FRANCE",
                                                        "ST DENIS"="SAINT-DENIS",
                                                        "PARIS NORD"="GARE DU NORD",
                                                        "HAUSSMANN"="OPERA",
                                                        "COURNEUVE"="LA COURNEUVE-AUBERVILLIERS",
                                                        "AULNAY SS BOIS"="AULNAY-SOUS-BOIS",
                                                        "AEROPORT CDG1"="AEROPORT CHARLES DE GAULLE 1"))

base <- readRDS("/Users/ingacarrelet/formation_CEPE/projet_groupe/Modelisation_echantillon_Foret")

base$garorig_recod <- revalue(base$garorig,c("STADE DE FRANCE"="LA PLAINE-STADE DE FRANCE",
                                             "ST DENIS"="SAINT-DENIS",
                                             "PARIS NORD"="GARE DU NORD",
                                             "HAUSSMANN"="OPERA",
                                             "COURNEUVE"="LA COURNEUVE-AUBERVILLIERS",
                                             "AULNAY SS BOIS"="AULNAY-SOUS-BOIS",
                                             "AEROPORT CDG1"="AEROPORT CHARLES DE GAULLE 1"))


base$garedest_recod <- revalue(base$garedest,c("STADE DE FRANCE"="LA PLAINE-STADE DE FRANCE",
                                               "ST DENIS"="SAINT-DENIS",
                                               "PARIS NORD"="GARE DU NORD",
                                               "HAUSSMANN"="OPERA",
                                               "AULNAY SS BOIS"="AULNAY-SOUS-BOIS",
                                               "AEROPORT CDG1"="AEROPORT CHARLES DE GAULLE 1"))

choixgare_dep <- base %>% distinct(garorig_recod)
choixgare_arr <- base %>% distinct(garedest_recod)

base$type_server <- revalue(base$type, c("prediction arbre" =3 , "prediction Lasso min" =1,"prediction Ridge min"=2,"prediction forest"=4,"prediction XG Boost"=5))
base$jour_server <- revalue(base$jsem,c("Lundi"=1,"Mardi"=2,"Mercredi"=3,"Jeudi"=4,"Vendredi"=5,"Samedi"=6,"Dimanche"=7))

coord_gares <- fread("/Users/ingacarrelet/formation_CEPE/projet_groupe/emplacement-des-gares-idf-data-generalisee.csv",header=TRUE,sep=";")

position <- as.character(coord_gares$Geo_Point)
vecteur <- do.call('rbind',strsplit(position,',',fixed=TRUE))
coord_gares$longitude <- as.numeric(vecteur[,2])
coord_gares$latitude <- as.numeric(vecteur[,1])
