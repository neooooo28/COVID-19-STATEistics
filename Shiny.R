library(shiny)
library(dplyr)

new_confirm <- read.csv("~/Dropbox/School/UCLA/stat 141sl/Final Project/shiny/data/r_0_confirmed.csv")
new_confirm <- new_confirm[,-1]
new_confirm$date <- as.Date(new_confirm$date, "%Y-%m-%d")

ui <- fluidPage(
  titlePanel("COVID-19 Analysis"),
  sidebarLayout(
    selectInput("state", 
                label = "Choose a state to display",
                choices = c("CA", "FL", "NY", "OK", "SD","WA", "WY"),
                selected = "CA"),
    
    sliderInput("DatesMerge",
                "Range of dates to investigate:",
                min = as.Date("2020-01-23","%Y-%m-%d"),
                max = as.Date("2020-05-10","%Y-%m-%d"),
                value=c(as.Date("2020-01-23"),as.Date("2020-02-23")),timeFormat="%Y-%m-%d")
  ),
  mainPanel(
    h1("New Confirmed cases"),
    plotOutput("ts_plot"),
    verbatimTextOutput("summary"),
    textOutput("min_max")
))

server <- function(input, output) {
  datasetInput <- reactive({
    new_confirm %>% filter(state == input$state & date > input$DatesMerge[1] & date < input$DatesMerge[2])
  })
  
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset$diff)
  })
  
  output$min_max <- renderText({ 
    paste("You have chosen a range that goes from",
          input$DatesMerge[1], "to", input$DatesMerge[2])
  })
  
  output$ts_plot <- renderPlot({
    dataset <- datasetInput()
    ggplot(dataset, aes(x = date, y = diff)) + geom_line(colour = 'black')})
}

shinyApp(ui = ui, server = server)
