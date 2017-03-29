library(shiny)
library(leaflet)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  leafletOutput("mymap")
)

server <- function(input, output, session) {
  

  output$mymap <- renderLeaflet({
    velib2<-read.csv("stations-velib-disponibilites-en-temps-reel.csv",row.names=1,sep=";")
    position <- as.character(velib2$position)
    vecteur <- do.call('rbind',strsplit(position,',',fixed=TRUE))
    velib2$longitude <- as.numeric(vecteur[,2])
    velib2$latitude <- as.numeric(vecteur[,1])
    
    ColorPal <- colorNumeric(scales::seq_gradient_pal(low = "#132B43", high = "#56B1F7", space = "Lab"), domain = c(0,1))
    m <- leaflet(data = velib2) %>%
      addTiles() %>%
      addCircles(~ longitude, ~ latitude, popup = ~ sprintf("<b> Available bikes: %s</b>",as.character(available_bikes)),
                 radius = ~ sqrt(bike_stands),
                 color = ~ ColorPal( available_bikes / (available_bikes + available_bike_stands)),
                 stroke = TRUE, fillOpacity = 0.75)
    
    m
  })
}

shinyApp(ui, server)