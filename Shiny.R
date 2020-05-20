library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)

new_confirm <- read.csv("~/Dropbox/School/UCLA/stat 141sl/Final Project/shiny/data/to.visualize2.csv")
new_confirm <- new_confirm[,-1]
new_confirm$date <- as.Date(new_confirm$date, "%Y-%m-%d")

jh_testing <- read.csv("~/Dropbox/School/UCLA/stat 141sl/Final Project/jh_testing.csv")
jh_testing$date <- as.Date(jh_testing$date, "%Y-%m-%d")
jh_testing <- jh_testing %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))


mobility <- read.csv("~/Dropbox/School/UCLA/Datafest 2020/state_mobility.csv")
mobility$date <- as.Date(mobility$date, "%Y-%m-%d")
mobility <- gather(mobility, sector, mob, work:residential, factor_key=TRUE)
mobility$sub_region_1 <- as.character(mobility$sub_region_1)
mobility[which(mobility$sub_region_1 == "California"),3] = "CA"
mobility[which(mobility$sub_region_1 == "Florida"),3] = "FL"
mobility[which(mobility$sub_region_1 == "New York"),3] = "NY"
mobility$sub_region_1 %>% unique()

#mobility %>% filter(sub_region_1 == "California") %>% ggplot() + geom_line(aes(x = date, y = mob, col = sector))

ui <- fluidPage(
  titlePanel("COVID-19 Analysis"),
  fluidRow(
    column(3,selectInput("state", 
                label = "Choose a state to display",
                choices = c("CA", "FL", "NY", "OK", "SD","WA", "WY"),
                selected = "CA")),
    
    column(3,sliderInput("DatesMerge",
                "Range of dates to investigate:",
                min = as.Date("2020-01-23","%Y-%m-%d"),
                max = as.Date("2020-05-16","%Y-%m-%d"),
                value=c(as.Date("2020-01-23"),as.Date("2020-02-23")),timeFormat="%Y-%m-%d")),
    
    column(3,checkboxGroupInput("checkGroup", 
                       "Choose which sector you want to see", 
                       choices = list("work" = "work", 
                                      "retail" = "retail", 
                                      "transit" = "transit",
                                      "park" = "park",
                                      "grocery" = "grocery",
                                      "residential" = "residential"),
                       selected = "work"))
  ),
  splitLayout(
    plotOutput("ts_plot"),
    plotOutput("mob_plot")
))

server <- function(input, output) {
  datasetInput <- reactive({
    new_confirm %>% filter(state == input$state & date > input$DatesMerge[1] & date < input$DatesMerge[2])
  })
  
  datasetInput2 <- reactive({
    if(!is.na(input$checkGroup[1])){
      mobility %>% filter(sub_region_1 == input$state & sector == input$checkGroup[1] & date > input$DatesMerge[1] & date < input$DatesMerge[2])
    }
    else{
      mobility %>% filter(sub_region_1 == input$state & date > input$DatesMerge[1] & date < input$DatesMerge[2])
    }
    
  })
  
  datasetInput3 <- reactive({
    jh_testing %>% filter(state == input$state & date > input$DatesMerge[1] & date < input$DatesMerge[2])
  })
  
  output$min_max <- renderText({ 
    paste("You have chosen a range that goes from",
          input$DatesMerge[1], "to", input$DatesMerge[2])
  })
  
  output$choice <- renderText({
    paste("This is your choice",
          input$checkGroup[1], "sector")
  })
  
  output$ts_plot <- renderPlot({
    dataset <- datasetInput()
    ggplot(dataset, aes(x = date)) + geom_line(aes(y = count, col = for.what)) +
      ylab("Count")+
      labs(colour="Legend") +
      ggtitle("Count for Confirmed, new positive, deaths")})
  
  output$mob_plot <- renderPlot({
    dataset <- datasetInput2()
    dataset %>% ggplot() + geom_line(aes(x = date, y = mob)) +
      ggtitle("Mobility of the Sector that you choose")})
}

shinyApp(ui = ui, server = server)
