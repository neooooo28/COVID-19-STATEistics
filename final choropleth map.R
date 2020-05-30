library(tigris)
library(dplyr)
library(leaflet)
library(sp)
library(lubridate)
library(shiny)
library(ggplot2)
library(tidyverse)
library(leaflet.extras)
library(sf)

states <- states(cb=T)

#reading in the data from github
urlfile<-'https://raw.githubusercontent.com/COVID19Tracking/covid-tracking-data/master/data/states_daily_4pm_et.csv'

#name of the file from github = 'states_daily_4pm_et.csv'
covidtracking<-read.csv(urlfile)

#reformatting to remove specific states
df <- covidtracking[!(covidtracking$state == "AS" | covidtracking$state == "GU" |
                          covidtracking$state == "MP"  | covidtracking$state == "VI"), ]
df$date <- ymd(df$date)

#adding latitude and longitude
latlong <- read.csv("latlong.csv")
join <- left_join(df, latlong, by="state")

#UI COMPONENT OF SHINY APP
ui <- fluidPage(
    
    titlePanel("Positive Case Count Increase by State"),
    
    
    sliderInput(inputId = "selection",
                
                "Choose Date:", 
                min = min(join$date, na.rm = TRUE), max = max(join$date, na.rm = TRUE), value = as.Date("2020-01-22"), animate = TRUE
    ),
    
    mainPanel(
        leafletOutput(outputId = "mymap"), 
        
    )
)

#SERVER COMPONENT OF SHINY APP
server <- function(input, output) {
    
    data_by_year <- reactive({
        join = join[join$date ==  input$selection, ]   
        data_by_year <- sp::merge(states, join,by.x="STUSPS", by.y="state" ,duplicateGeoms = TRUE)
    
    })
    
    output$mymap <- renderLeaflet({
        data_by_year <- data_by_year()
        data_by_year <- subset(data_by_year, !is.na(positiveIncrease))
        print(summary(data_by_year$positiveIncrease))
        pal <- colorNumeric("Reds", domain=data_by_year$positiveIncrease) 
        popup_total <- paste0("state: ",data_by_year$STUSPS, "<br>", "positive increase:", sep = " ", data_by_year$positiveIncrease)
    
            leaflet() %>%
            addProviderTiles("CartoDB.DarkMatter") %>%
            setView(-99, 40, zoom = 4) %>% 
            addPolygons(data = data_by_year, #() , 
                        fillColor = ~pal(data_by_year$positiveIncrease), 
                        fillOpacity = 0.7, 
                        weight = 0.2, 
                        smoothFactor = 0.2, 
                        popup = ~popup_total) %>%
            addLegend(pal = pal, 
                      values = data_by_year$positiveIncrease, 
                      position = "bottomright", 
                      title = "Covid Case Increase")   
            })
    
}


# RUN SHINY APP
shinyApp(ui = ui, server = server)



