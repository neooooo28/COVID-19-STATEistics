#libraries
library(lubridate)
library(shiny)
library(ggplot2)
library(tidyverse)
library(leaflet)
library(dplyr)
library(leaflet.extras)

#reformatting
covidtracking <- read.csv("covidtracking.csv")
df <- covidtracking[!(covidtracking$state == "AS" | covidtracking$state == "GU" |
                          covidtracking$state == "MP"  | covidtracking$state == "VI"), ]
df$date <- ymd(df$date)

#adding latitude and longitude
latlong <- read.csv("latlong.csv")
join <- left_join(df, latlong, by="state")


#UI COMPONENT
ui <- fluidPage(
    
    # Application title
    titlePanel("Confirmed Covid Cases in Selected US States"),
    
    
    sliderInput(inputId = "selection",
                
                "Choose Date:", 
                min = as.Date("2020-01-22"), max = as.Date("2020-05-19"), value = as.Date("2020-04-10")
    ),
    
    mainPanel(
        leafletOutput(outputId = "mymap"), 
        
    )
)

#SERVER COMPONENT
library(lubridate)
server <- function(input, output) {
    
    year_data <- reactive({
        print(input$selection)
        join[(join$date == input$selection),]
    })
    
    output$mymap <- renderLeaflet({
        print(year_data)
        year_data() %>%
            leaflet() %>% 
            setView(lng = -99, lat = 45, zoom = 4)  %>% #map centered on the US
            addTiles() %>% 
            addCircles( lat = ~lat, lng = ~long, weight = 1, radius = ~positive*3, label = ~as.character(paste0("Cases: ", sep = " ", positive)), fillOpacity = 0.5)
    })
    
}

# RUN SHINY APP
shinyApp(ui = ui, server = server)















