library(shiny)
library(shinyjs)
library(dplyr)
library(DT)
library(httr)
library(shinythemes)
library(shinyWidgets)
timeout(1000)
#setwd( "C:/Users/koste/Studia/07/PI/github/projekt_interdyscyplinarny/model2")
source("model_from_random_points_combined.R")

ui <- fluidPage(
  theme = shinytheme('flatly'),
  setBackgroundColor(color = c("#FFFFFF", "#4444FF"),
                     gradient = "linear",
                     direction = c("right", "bottom")),
  titlePanel("Criterions "),
  sidebarPanel(
    radioButtons(inputId = "dataChoice", label = "Specify the location preferences",
                 choices = c("Regions","Coordinates"), inline = TRUE),
    
    fluidRow(selectInput(inputId = "regions", label = "Regions selection",
                         choices = list(Polska = c("dolnoslaskie" = "D",
                                                   "kujawsko-pomorskie" = "C",
                                                   "lubelskie" = "L",
                                                   "lubuskie" = "F",
                                                   "lodzkie" = "E",
                                                   "malopolskie" = "K",
                                                   "mazowieckie" = "W",
                                                   "opolskie" = "O",
                                                   "podkarpackie" = "R",
                                                   "podlaskie" = "B",
                                                   "pomorskie" = "G",
                                                   "slaskie" = "S",
                                                   "swietokrzyskie" = "T",
                                                   "warminsko-mazurskie" = "N",
                                                   "wielkopolskie"= "P",
                                                   "zachodniopomorskie" = "Z")), selected = NULL, multiple = TRUE)),
    
    h5("Coordinates selection"),
    numericInput("min_lat_deg", label = "Minimum latitude (degrees)", 
                 value = 20.5, min = 0, max = 90),
    numericInput("max_lat_deg", label = "Maximum lattitude (degrees)", 
                 value = 21, min = 0, max = 90),
    numericInput("min_lon_deg", label = "Minimum longitude (degrees)", 
                 value = 52.8, min = 0, max = 90),
    numericInput("max_lon_deg", label = "Maximum longitude (degrees)", 
                 value = 53.5, min = 0, max = 90),
    
    verbatimTextOutput("value"),
    
    fluidRow(selectInput(inputId = "resources", label = "Resources proximity preferences",
                         choices = list("Rivers" = "rivers", "Crops" = "crops", "Forests" = "forests", "Motorways" = "motorways"), 
                         selected = NULL, multiple = TRUE)),
    actionButton("btn", "Predict")
    
  ),
  mainPanel(
    radioButtons('format', 'Document format', c('HTML', 'Word'), inline = TRUE),
    downloadButton('downloadReport'),
    br(),
    br(),
    h4("Model predictions"),
    DT::dataTableOutput('table')
  ),
  useShinyjs()
)
server <- function(input, output) {
  
  observeEvent(input$dataChoice, {
    if(input$dataChoice == "Coordinates"){
      enable('min_lat_deg') 
      enable('max_lat_deg')
      enable('min_lon_deg')
      enable('max_lon_deg')
      disable('regions')
    }
    else{
      disable('min_lat_deg') 
      disable('max_lat_deg')
      disable('min_lon_deg')
      disable('max_lon_deg')
      enable('regions')
    }})
  
  numbers <- reactive({
    shiny::validate(
      need(is.numeric(input$min_lon_deg), "Please input a number"),
      need(is.numeric(input$max_lon_deg), "Please input a number"),
      need(is.numeric(input$min_lat_deg), "Please input a number"),
      need(is.numeric(input$max_lat_deg), "Please input a number")
    )
  })
  output$value <- renderPrint({ numbers() })
  
  
  
  # Call Onclick
  renderTab <- function(){
    xmin <- input$min_lat_deg
    xmax <- input$max_lat_deg 
    ymin <- input$min_lon_deg  
    ymax <- input$max_lon_deg
    
    result <- predict_from_area(xmin, ymin, xmax, ymax, n = 10)
    prediction <- head(result[order(as.vector(result$score), decreasing=TRUE),],5)
    
    output$table <- DT::renderDataTable(prediction,
                                        colnames = c('Score', 'Gmina', 'Longitude', 'Latitude'))
    
  }
  onclick("btn", renderTab)
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('Report', sep = '.', switch(
        input$format, HTML = 'html', Word = 'docx'
      ))
    },
    
    # Trzeba sie zastanowic do wrzucic do zawartosci raportu
    content = function(file) {
      src <- normalizePath('report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      
      library(rmarkdown)
      out <- render('report.Rmd', switch(
        input$format,
        HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
}   

shinyApp(ui = ui, server = server)

