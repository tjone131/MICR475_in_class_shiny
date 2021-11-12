library(shiny)
library(tidyverse)
library(ggmap)
WSKO <- read.csv("territorymapexample.csv")
register_google(key = "AIzaSyAinMZGtkgpmpXIYJguj5K08Ja0sBv6cr4")

ui <- fluidPage(
                titlePanel("Kernal Density Home Range of a Specific Sparrow"),
                sliderInput(inputId = "h",
                            label = "Kernal Width (h)",
                            min = 0.00005,
                            max = 0.001,
                            value = 0),
                plotOutput(outputId = "densityplot")
                #submitButton(text = "New Plot")

)


server <- function(input, output){
  output$densityplot <- renderPlot({
    map <- ggmap(get_googlemap(center = c(lon = -122.477087, lat = 37.80741702),
                               zoom = 20, scale = 2,
                               maptype ='satellite',
                               color = 'color')) +
      labs(x = "Longitude", y = "Latitude") +
      theme(axis.text.x =  element_text(angle = 45))

    map + stat_density_2d(aes(x = Longitude, y = Latitude, fill = ..level..),
                          alpha = .6, h = input$h, n = 90,
                          data=WSKO, geom = "polygon")+
      scale_fill_viridis_b()
    })
}


shinyApp(ui = ui, server = server)
