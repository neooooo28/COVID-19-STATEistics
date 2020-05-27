library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)

new_confirm <- read.csv("~/Dropbox/School/UCLA/stat 141sl/Final Project/shiny/data/to.visualize2.csv")
new_confirm <- new_confirm[,-1]
new_confirm$date <- as.Date(new_confirm$date, "%Y-%m-%d")

jh_testing <- read.csv("~/Dropbox/School/UCLA/stat 141sl/Final Project/jh_testing.csv")
jh_testing$date <- as.Date(jh_testing$date, "%Y-%m-%d")
jh_testing <- jh_testing %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))


mobility <- read.csv("~/Dropbox/School/UCLA/stat 141sl/Final Project/shiny/data/state_mobility.csv")
mobility$date <- as.Date(mobility$date, "%m/%d/%Y")
#mobility <- gather(mobility, sector, mob, work:residential, factor_key=TRUE)
mobility$sub_region_1 <- as.character(mobility$sub_region_1)

ca_m <- mobility %>% filter(sub_region_1 == "CA") #select state
x <- ca_m$date %>% unique()
fig <- plot_ly(ca_m,x = ~x, y = ca_m$work, name = 'Work Sector', type = 'scatter', mode = 'lines')
fig <- fig %>% add_trace(y = ca_m$retail, name = 'Retail Sector', mode = 'lines') 
fig <- fig %>% add_trace(y = ca_m$transit, name = 'Transit Sector', mode = 'lines') 
fig <- fig %>% add_trace(y = ca_m$park, name = 'Park Sector', mode = 'lines') 
fig <- fig %>% add_trace(y = ca_m$grocery, name = 'Grocery Sector', mode = 'lines') 
fig <- fig %>% add_trace(y = ca_m$residential, name = 'Residential Sector', mode = 'lines') 
fig <- fig %>% layout(
  title = 'Mobility',
  xaxis = list(
    title = 'date'
  ),
  yaxis = list(
    title = 'scaled mobility'
  )
)

fig

#mobility %>% filter(sub_region_1 == "California") %>% ggplot() + geom_line(aes(x = date, y = mob, col = sector))

ui <- fluidPage(
  titlePanel("COVID-19 Analysis"),
  fluidRow(
    column(3,selectInput("state", 
                label = "Choose a state to display",
                choices = c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "IA",
                            "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO",
                            "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK",
                            "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY"),
                selected = "CA")),
    
    column(3,sliderInput("DatesMerge",
                "Range of dates to investigate:",
                min = as.Date("2020-01-23","%Y-%m-%d"),
                max = as.Date("2020-05-16","%Y-%m-%d"),
                value=c(as.Date("2020-01-23"),as.Date("2020-02-23")),timeFormat="%Y-%m-%d")),
    
    column(3,h4("Choose or drop sector by clicking legends"))
  ),
  splitLayout(
    plotOutput("ts_plot"),
    plotlyOutput("mob_plot")
  )
)

server <- function(input, output) {
  datasetInput <- reactive({
    new_confirm %>% filter(state == input$state & date > input$DatesMerge[1] & date < input$DatesMerge[2])
  })
  
  datasetInput2 <- reactive({
      mobility %>% filter(sub_region_1 == input$state & date > input$DatesMerge[1] & date < input$DatesMerge[2])
    
  })
  
  datasetInput3 <- reactive({
    jh_testing %>% filter(state == input$state & date > input$DatesMerge[1] & date < input$DatesMerge[2])
  })
  
  output$min_max <- renderText({ 
    paste("You have chosen a range that goes from",
          input$DatesMerge[1], "to", input$DatesMerge[2])
  })
  
  output$ts_plot <- renderPlot({
    dataset <- datasetInput()
    ggplot(dataset, aes(x = date)) + geom_line(aes(y = count, col = for.what)) +
      ylab("Count")+
      labs(colour="Legend") +
      ggtitle("Count for Confirmed, new positive, deaths")})
  
  output$mob_plot <- renderPlotly({
    dataset <- datasetInput2()
    x <- dataset$date %>% unique()
    fig <- plot_ly(dataset,x = ~x, y = dataset$work, name = 'Work Sector', type = 'scatter', mode = 'lines')
    fig <- fig %>% add_trace(y = dataset$retail, name = 'Retail Sector', mode = 'lines') 
    fig <- fig %>% add_trace(y = dataset$transit, name = 'Transit Sector', mode = 'lines') 
    fig <- fig %>% add_trace(y = dataset$park, name = 'Park Sector', mode = 'lines') 
    fig <- fig %>% add_trace(y = dataset$grocery, name = 'Grocery Sector', mode = 'lines') 
    fig <- fig %>% add_trace(y = dataset$residential, name = 'Residential Sector', mode = 'lines') 
    fig <- fig %>% layout(
      title = 'Mobility',
      xaxis = list(
        title = 'date'
      ),
      yaxis = list(
        title = 'scaled mobility'
      )
    )
    
    fig
})}

shinyApp(ui = ui, server = server)
