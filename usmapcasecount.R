
library(shiny)
library(ggplot2)
library(tidyverse)
library(leaflet)
library(dplyr)
library(leaflet.extras)

#UI COMPONENT
ui <- fluidPage(
    
    # Application title
    titlePanel("Confirmed Covid Cases in Selected US States"),
    
    
    sliderInput(inputId = "selection",
                
                "Choose Date:", 
                min = as.Date("2020-01-22"), max = as.Date("2020-05-10"), value = as.Date("2020-04-10")
    ),
    
    mainPanel(
        leafletOutput(outputId = "mymap"), 
        
    )
)

#SERVER COMPONENT
library(lubridate)
server <- function(input, output) {
    
    
    confirmed <- read.csv("state.csv")
    head(confirmed)
    
    df <- confirmed[(confirmed$state == "California" | confirmed$state =="Wyoming" |
                         confirmed$state =="Oklahoma" | confirmed$state =="Washington" |
                         confirmed$state =="South Dakota" | confirmed$state =="Texas" | confirmed$state=="New York" | confirmed$state=="Florida"), ]
    df$date_col <- ymd(df$date_col)
    
    year_data <- reactive({
        print(input$selection)
        df[(df$date_col == input$selection),]
    })
    
    output$mymap <- renderLeaflet({
        print(year_data)
        year_data() %>%
            leaflet() %>% 
            setView(lng = -99, lat = 45, zoom = 4)  %>% #map should only include US, as specified with these coordinates
            addTiles() %>% 
            addCircles( lat = ~lat, lng = ~long, weight = 1, radius = ~cases*3, label = ~as.character(paste0("Cases: ", sep = " ", cases)), fillOpacity = 0.5)
    })

}

# RUN SHINY APP
shinyApp(ui = ui, server = server)


