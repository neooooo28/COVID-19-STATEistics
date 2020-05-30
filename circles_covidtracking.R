#libraries
library(lubridate)
library(shiny)
library(ggplot2)
library(tidyverse)
library(leaflet)
library(dplyr)
library(leaflet.extras)

#reading in the data from github/google 
urlfile<-'https://raw.githubusercontent.com/COVID19Tracking/covid-tracking-data/master/data/states_daily_4pm_et.csv'
covidtracking<-read.csv(urlfile)

#reformatting
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
                min = as.Date("2020-01-22"), max = max(join$date), value = as.Date("2020-01-22"), animate = T
    ),
    
    mainPanel(
        leafletOutput(outputId = "mymap"), 
        
    )
)

#SERVER COMPONENT
server <- function(input, output) {
    
    year_data <- reactive({
        print(input$selection)
        join[(join$date == input$selection),]
    })
    
    output$mymap <- renderLeaflet({
        print(year_data)
        year_data() %>%
            leaflet() %>% 
            setView(lng = -99, lat = 40, zoom = 4)  %>% #map centered on the US
            addTiles() %>% 
            addCircles(lat = ~lat, lng = ~long, weight = 1, radius = ~positive*2, color = "red", label = ~as.character(paste0("Cases and State: ", sep = " ", positive,sep=", ",state)), fillOpacity = 0.5)      
    })
    
}

# RUN SHINY APP
shinyApp(ui = ui, server = server)









