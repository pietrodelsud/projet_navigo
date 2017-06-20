#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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




ui <- 


# MainPanel divided into many tabPanel
  mainPanel(
    tabsetPanel(
      tabPanel("Intro : le projet CEPE",
               textOutput("text1"),
               textOutput("text2"),
               textOutput("text3"),
               textOutput("text4"),
               textOutput("text5")),
      
      tabPanel("Ou vont les clients de ma gare ?",
               bootstrapPage(
                titlePanel("Les clients de la ligne E"),
                fluidRow(column(2,img(src="https://www.transilien.com/sites/default/files/styles/plan_ligne/public/thumbnails/file/schema_ligne_e.jpg?itok=BLI4e6Tl",height=100,width=700))),
                titlePanel(" "),
                leafletOutput("mymap1"),
                absolutePanel(top = 10, right = 15,br(),br(), draggable = TRUE,
                      wellPanel(selectInput("id_gare_dep1", "Gare de depart :", choices = lignee_4$GARE_E))),
                plotOutput("distPlot1"))),
      
      tabPanel("Repartition horaire",
              bootstrapPage(
              titlePanel("Les clients de la ligne E"),
              absolutePanel(top = 10, right = 15,br(),br(), draggable = TRUE,
                      wellPanel(selectInput("id_gare_dep2", "Gare de depart :", choices = lignee_4$GARE_E)),
                      wellPanel(selectInput("titre", "Titre de transport :", choices = lignee_5$fr_lignee.libl_titre))
                       ),
              plotOutput("distPlot2"))),
      
      tabPanel("Modelisation des temps de trajets",
               bootstrapPage(
                 tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                 leafletOutput("mymap2"),
                 absolutePanel(top = 10, right = 15, draggable = TRUE,
                               wellPanel(selectInput("id_gare_dep3", "Ma gare de depart :", choices = choixgare_dep$garorig_recod)),
                               wellPanel(selectInput("id_gare_arr", "Ma gare d'arrivee :", choices = choixgare_arr$garedest_recod))),
                 absolutePanel(top = 10, left = 10,width="25%",br(),br(),draggable = TRUE,
                               wellPanel(selectInput("indicateur", "Choix de votre indicateur:", choices = list("Nombre de validations"=1,"temps de trajet"=2)),
                                         uiOutput("conditionalInput"))
                 ),
                 absolutePanel(bottom=2,left=15,width="50%",draggable = TRUE,
                               wellPanel(plotOutput("histtrajet")))
               ))
     ) 
    )
