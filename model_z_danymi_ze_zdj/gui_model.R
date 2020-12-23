library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)

library(leaflet)
library(leaflet.extras)

library(dplyr)
library(DT)
library(httr)

timeout(1000)
source("model_from_random_points_combined.R")

ui <- fluidPage(
  theme = shinytheme('flatly'),
  setBackgroundColor(color = c("#F7FBFF", "#2171B5"),
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
    leafletOutput("mymap", height = 300),
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
  
  ### MAP
  output$mymap <- renderLeaflet({
    leaflet() %>% # initializing a leaflet map
      addProviderTiles("CartoDB", # map theme - to be changed =
                     options = providerTileOptions(minZoom = 5, maxZoom = 8)) %>%
      setView(lng = 19.356389,
            lat = 52.196667, # location of "Nowa Wies" - exact geodetic center of Poland
            zoom = 6) %>%
      setMaxBounds(lng1 = 14, lat1 = 49,
                 lng2 = 24.5, lat2 = 55.5) %>%
      addRectangles(input$min_lat_deg, input$min_lon_deg,
                    input$max_lat_deg, input$max_lon_deg, # dobra skladnia, nie dziala wczyt
                    fillColor = "transparent") %>%
      addDrawToolbar( # a feature to draw rectangles and change the value of input coords
        targetGroup='draw',
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()),
        polylineOptions = FALSE,
        polygonOptions = FALSE,
        circleOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        singleFeature = TRUE) %>%
      addLayersControl(overlayGroups = c('draw'), options =
                         layersControlOptions(collapsed=FALSE))
  })
  
  observeEvent(input$mymap_draw_new_feature,{
    feature <- input$mymap_draw_new_feature
    
    rect_lats <- c(feature$geometry$coordinates[[1]][[1]][[1]],
                   feature$geometry$coordinates[[1]][[2]][[1]],
                   feature$geometry$coordinates[[1]][[3]][[1]],
                   feature$geometry$coordinates[[1]][[4]][[1]])
    rect_longs <- c(feature$geometry$coordinates[[1]][[1]][[2]],
                   feature$geometry$coordinates[[1]][[2]][[2]],
                   feature$geometry$coordinates[[1]][[3]][[2]],
                   feature$geometry$coordinates[[1]][[4]][[2]])
    rect_min_lat_deg <- min(rect_lats)
    rect_max_lat_deg <- max(rect_lats)
    rect_min_lon_deg <- min(rect_lats)
    rect_max_lon_deg <- max(rect_lats)
    # TODO: rect_* wrzucic na koordynaty na serwerze (wtedy tez sie zaktualizuje ten drugi)
  })
  
  # Call Onclick
  renderTab <- function(){
    xmin <- input$min_lat_deg
    xmax <- input$max_lat_deg 
    ymin <- input$min_lon_deg  
    ymax <- input$max_lon_deg
    
    result <- predict_from_area(xmin, ymin, xmax, ymax, n = 5)
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

