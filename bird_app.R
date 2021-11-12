library(shiny)
library(tidyverse)
library(ggmap)
WSKO <- read.csv("territorymapexample.csv") ## location data from WS/KO, one sparrow next to the Golden Gate Bridge
register_google(key = "AIzaSyAinMZGtkgpmpXIYJguj5K08Ja0sBv6cr4") ## register API key to access Google maps through ggmap

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
    map <- ggmap(get_googlemap(center = c(lon = -122.477087, lat = 37.80741702), ## get a base map from Google, specify map center
                               zoom = 20, scale = 2, # specify map scale
                               maptype ='satellite',
                               color = 'color')) +
      labs(x = "Longitude", y = "Latitude") +
      theme(axis.text.x =  element_text(angle = 45))

    map + stat_density_2d(aes(x = Longitude, y = Latitude, fill = ..level..), # make a heat map on top of the Google map
                          alpha = .6, h = input$h, n = 90, # h is the kernel width (smoothing factor), n is number of locations sampled
                          data=WSKO, geom = "polygon")+
      scale_fill_viridis_b()
    })
}


shinyApp(ui = ui, server = server)
