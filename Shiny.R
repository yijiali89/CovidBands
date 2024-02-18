library(tidyverse)
library(EBImage)
library(shiny)
library(patchwork)
library(shinyjs)

source('Bands.R')

jscode <- "shinyjs.refresh_page = function() { history.go(0); }"
ui <- fluidPage(
  titlePanel("Band Quantification"),
  sidebarLayout(
  sidebarPanel = sidebarPanel(
    fileInput("file", "Upload Figures"), 
    useShinyjs(),
    extendShinyjs(text = jscode, functions = "refresh_page"),
    actionButton("refresh", "Refresh app"), 
    br(),
    br(),
    print('Please crop antigen bands to include only control and test bands; 
          keep control bands at upper 1/3 area and test band at the lower 1/3 are; remove margins'), 
    br(),
    img(src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcSnwJgZXEILvM8kF4XLbuoAYK3dxAaSg5bWqA&usqp=CAU", align='middle')
  ),
  
  mainPanel = mainPanel(
    h4("Intensity ratio"),
    verbatimTextOutput("ir"),
    h4("Background"),
    verbatimTextOutput("bg"),
    h4("Plot"),
    plotOutput("Plot")
  )
)
)

server <- function(input, output, session) {
  
  data<-reactive(input)

  output$ir<-renderPrint({
    req(data()$file)
    result<-process(data()$file$datapath)
    output$ir <- renderPrint(result[[2]])
    output$bg<- renderPrint(result[[3]])
  })
  
  output$Plot<-renderPlot({
    req(data()$file)
    result<-process(data()$file$datapath)
    output$Plot <- renderPlot(result[[1]])
  })
  
  observeEvent(input$refresh, {
    js$refresh_page();
  })
}

shinyApp(ui = ui, server = server)
