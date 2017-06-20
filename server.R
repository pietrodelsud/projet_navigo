#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

options(encoding = "UTF-8")

library(shiny)
library(leaflet)
library(RColorBrewer)
library(htmltools)
library(dplyr)
library(ggplot2)
library(sp)
library(dtplyr)


#http://www.dacta.fr/blog/cartographie-r-leaflet.html



server <- function(input, output, session) {
  
  filteredData_dep1 <- reactive({lignee_4[lignee_4$GARE_E==input$id_gare_dep1,]})
  filtreligne5 <- reactive({lignee_5[(lignee_5$GARE_E==input$id_gare_dep2) & (lignee_5$fr_lignee.libl_titre==input$titre),]})
  
  output$mymap1 <- renderLeaflet({
    ColorPal <- colorNumeric(scales::seq_gradient_pal(low = "#132B43", high = "#56B1F7", space = "Lab"), domain = c(0,1))
    m <- leaflet(data = lignee_4) %>%
      addTiles() %>%
      fitBounds(~min(LONGITUDE_s), ~min(LATITUDE_s), ~max(LONGITUDE_s), ~max(LATITUDE_s))
    m
  })
  
  observe({
    if (!is.null(filteredData_dep1())) {
      don_filtree_dep <- filteredData_dep1()
      leafletProxy("mymap1", data = don_filtree_dep) %>%
        clearMarkers() %>%
        addCircles(~ LONGITUDE_s, ~ LATITUDE_s, popup = ~paste(GARE_S, ":", nbtrajet),
                   radius = ~ sqrt(nbtrajet)*20,
                   color = "#a500a5",
                   stroke = TRUE, fillOpacity = 0.5)
      leafletProxy("mymap1", data = don_filtree_dep) %>%
        clearMarkers() %>%
        addAwesomeMarkers(~ LONGITUDE_e, ~ LATITUDE_e, popup = ~ GARE_E,label = htmlEscape(as.character(~ GARE_E)), 
                          labelOptions = labelOptions(noHide = F))
    }
  })
  
  
  output$distPlot1 <- renderPlot({
    ggplot(lignee_4,aes(y=nbtrajet,x=GARE_S)) + geom_bar(stat="identity", color="blue", fill="steelblue") + geom_text(aes(label=round(duree_moy,2)), 
                                                                                                                      vjust=-0.3, size=5) + ggtitle("Top 10 des destinations a partir de la gare choisie (taille baton) et duree precisee au-dessus du baton") 
  })
  
  output$distPlot1 <- renderPlot({
    if (!is.null(filteredData_dep1())) {
      don_filtree_dep <- filteredData_dep1()
      don_filtree_dep2 <- arrange(don_filtree_dep, desc(nbtrajet))
      don_filtree_dep3 <- head(don_filtree_dep2,10)
      
      ggplot(don_filtree_dep3,aes(y=nbtrajet,x=reorder(GARE_S,-nbtrajet))) + geom_bar(stat="identity", color="blue", fill="steelblue") + geom_text(aes(label=round(duree_moy,2)), vjust=-0.3, size=5) + ggtitle("Top 10 des destinations a partir de la gare choisie (taille baton) et duree precisee au-dessus du baton") 
    }
  })
  
  
  output$distPlot2 <- renderPlot({
    ggplot(lignee_5,aes(y=nbtrajet,x=fr_lignee.heure_valid_e)) + geom_bar(stat="identity", color="blue", fill="steelblue") + geom_text(aes(label=round(duree_moy,2)), vjust=-0.3, size=5) + ggtitle("Top 10 des destinations a partir de la gare choisie (taille baton) et duree precisee au-dessus du baton") 
  })
  
  observe({
    
    if (!is.null(filtreligne5()))  {
      
      don_filtree_deptitre <- filtreligne5()
      output$distPlot2 <- renderPlot({
        #ggplot(don_filtree_deptitre,aes(y=nbtrajet,x=as.factor(fr_lignee.heure_valid_e))) + geom_bar(stat="identity", color="blue", fill="steelblue") +  ggtitle("Répartition horaire") 
        ggplot(don_filtree_deptitre,aes(y=nbtrajet,x=fr_lignee.heure_valid_e)) + geom_bar(stat="identity", color="blue", fill="steelblue") +  ggtitle("Répartition horaire") 
      })
    }
  })
  
  
  
  
  
  output$text1 <- renderText ({
    "Base de donnees validations telebillettiques sur le reseau transilien de janvier a mars 2015"
  })
  
  output$text2 <- renderText ({
    " "
  })
  
  output$text3 <- renderText ({
    "Sur 18,9 millions de validations observees en entree sur la ligne E, reconstitution des 6,2 millions de trajets."
  })
  
  output$text4 <- renderText ({
    " "
  })
  
  output$text5 <- renderText ({
    "(Perte d'1/3 que l'on ne voit pas sortir, compte tenu de gares ouvertes ou de correspondances sur le reseau RATP"
  })
  filteredData_dep2 <- reactive({coord_gares[coord_gares$NOM_LONG==input$id_gare_dep3,]})
  filteredData_arr <- reactive({coord_gares[coord_gares$NOM_LONG==input$id_gare_arr,]})
  
  
  #filtrebase <- reactive({base[(base$garorig_recod==input$id_gare_dep) & (base$garedest_recod==input$id_gare_arr),]})
  
  filtrebase_model <- reactive({base[(base$garorig_recod==input$id_gare_dep3) & (base$garedest_recod==input$id_gare_arr) & ((base$type_server==input$modele) | (base$type_server=="reel")) & (base$jour_server %in% input$jour_model),]})
  filtrebase_valid <- reactive({base_valid_tri[(base_valid_tri$libgar_orig_recod==input$id_gare_dep3) & (base_valid_tri$jour_server %in% input$jour_valid),]})
  
  filtrebase_erreur <- reactive({erreur_df[(erreur_df$garorig_recod==input$id_gare_dep3) & (erreur_df$garedest_recod==input$id_gare_arr),]})
  
  output$mymap2 <- renderLeaflet({
    leaflet(coord_gares) %>% addTiles() %>% fitBounds(~min(coord_gares$longitude), ~min(coord_gares$latitude), ~max(coord_gares$longitude), ~max(coord_gares$latitude)) })
  
  #addPolylines(lng=geojson$geo_point_2d2,lat=geojson$geo_point_2d1,
  # label=~paste0("ligne :",geojson$indice_lig))
  
  observe({
    if (!is.null(filteredData_dep2())) {
      don_filtree_dep_modl <- filteredData_dep2()
      leafletProxy("mymap2", data = don_filtree_dep_modl) %>%
        clearMarkers() %>%
        addAwesomeMarkers(popup = don_filtree_dep_modl$NOM_LONG,label = htmlEscape(as.character(don_filtree_dep_modl$NOM_LONG)), labelOptions = labelOptions(noHide = F))
    }
    
  })
  observe({
    if (!is.null(filteredData_arr())) {
      don_filtree_arr <- filteredData_arr()
      leafletProxy("mymap2", data = don_filtree_arr) %>%
        #clearMarkers() %>%
        addAwesomeMarkers(popup = don_filtree_arr$NOM_LONG,label = htmlEscape(as.character(don_filtree_arr$NOM_LONG)), labelOptions = labelOptions(noHide = F))
    }
    
  })
  
  # Reactive expression for the data subsetted to what the user selected
  output$conditionalInput = renderUI(
    if(input$indicateur==1){
      wellPanel(checkboxGroupInput("jour_valid", "Choix du jour/semaine :", choices = list("Lundi"=1, "Mardi"=2, "Mercredi"=3, "Jeudi"=4 , "Vendredi"=5, 
                                                                                           "Samedi"=6,"Dimanche"=7), selected = c(1,2,3)))}
    else if (input$indicateur==2){
      wellPanel(selectInput("modele", "Choix du modele:", choices = list("Regression lasso"=1,"Regression ridge"=2,"Arbre"=3,"Random Forest"=4,"XG Boost"=5)),
                checkboxGroupInput("jour_model", "Choix du jour/semaine :", choices = list("Lundi"=1, "Mardi"=2, "Mercredi"=3, "Jeudi"=4 , "Vendredi"=5, 
                                                                                           "Samedi"=6,"Dimanche"=7), selected = c(1,2,3)),
                textOutput("text"))
    })
  
  observe({
    if (input$indicateur==1){output$histtrajet <- renderPlot({
      ggplot(filtrebase_valid(),aes(y=nb_valid,x=jour_server,ymax=max(filtrebase_valid()$nb_valid))) + geom_bar(stat="identity",position='dodge') + scale_y_continuous(name="nombre de validations en entrée") + scale_x_discrete(name="jour de la semaine",labels=filtrebase_valid()$jsem) + ggtitle(paste("Nombre de validations",filtrebase_valid()$libgar_orig_recod))
      
    })
    }
    
    else if(input$indicateur==2){
      output$histtrajet <- renderPlot({
        ggplot(filtrebase_model(),aes(y=duree,x=periode,colour=type)) + geom_point(size=1) + ggtitle("Temps de trajet") + geom_line()
      })
      output$text <- renderText({ 
        if (input$modele==1) {
          paste("Erreur : ", filtrebase_erreur()$lassomin_mean)
        }
        else if (input$modele==2){
          paste("Erreur : ", filtrebase_erreur()$ridgemin_mean)
        }
        else if (input$modele==3){
          paste("Erreur : ", filtrebase_erreur()$arbre_mean)
        }
        else if (input$modele==4){
          paste("Erreur : ", filtrebase_erreur()$forest_mean)
        }
        else if (input$modele==5){
          paste("Erreur : ", filtrebase_erreur()$XGB_mean)
        }
      })
    }
  })
}

