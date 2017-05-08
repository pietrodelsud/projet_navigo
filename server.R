library(shiny)
library(leaflet)



server <- function(input, output, session) {
  
  
  output$mymap <- renderLeaflet({
    
    leaflet(data = don) %>%
      addTiles() %>%
      
      #addCircles(~ longitude, ~ latitude, popup = ~ sprintf("<b> Available bikes: %s</b>",as.character(available_bikes)),
      #           radius = ~ sqrt(bike_stands),
      #         color = ~ ColorPal( available_bikes / (available_bikes + available_bike_stands)),
       #          stroke = TRUE, fillOpacity = 0.75)
     
      addMarkers(~LONGITUDE, ~LATITUDE,popup=paste(don$commune,"<br>","gare :",don$GARE_IDE_GAR,"<br>", "ligne :",don$ZONE_GARE_DES)) %>%
      setView(lng=2,lat=50,zoom=8)
      
  
  })
}

