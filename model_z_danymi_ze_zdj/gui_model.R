library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(shinybusy)

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
                 choices = c("Coordinates", "Regions"), inline = TRUE, selected = "Coordinates"),
    
    fluidRow(selectInput(inputId = "regions", label = "Regions selection",
                         choices = list(Polska = c("default" = "none",
                                                   "dolnoslaskie" = "D",
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
                                                   "zachodniopomorskie" = "Z")), selected = "none", multiple = FALSE)),
    
    h5("Coordinates selection"),
    numericInput("min_lng_deg", label = "Minimum longitude (degrees)", 
                 value = 20.5, min = 0, max = 90),
    numericInput("max_lng_deg", label = "Maximum longitude (degrees)", 
                 value = 21, min = 0, max = 90),
    numericInput("min_lat_deg", label = "Minimum latitude (degrees)", 
                 value = 52.8, min = 0, max = 90),
    numericInput("max_lat_deg", label = "Maximum latitude (degrees)", 
                 value = 53.5, min = 0, max = 90),
    
    verbatimTextOutput("value"),
    
    actionButton("btn", "Predict")
    
  ),
  mainPanel(
    # radioButtons('format', 'Document format', c('HTML', 'PDF'), inline = TRUE),
    downloadButton('downloadReport', label = "Download HTML report"),
    br(),
    br(),
    leafletOutput("mymap", height = 300),
    h4("Model predictions"),
    DT::dataTableOutput('table')
  ),
  useShinyjs()
)
server <- function(input, output, session) {
  
  disable("downloadReport")
  
  observeEvent(input$dataChoice, {
    if(input$dataChoice == "Coordinates"){
      enable('min_lng_deg')
      enable('max_lng_deg')
      enable('min_lat_deg')
      enable('max_lat_deg')
      disable('regions')
    }
    else{
      disable('min_lng_deg') 
      disable('max_lng_deg')
      disable('min_lat_deg')
      disable('max_lat_deg')
      enable('regions')
    }})
  
  observeEvent(input$regions, {
    if (input$regions == "D") {
      updateNumericInput(session = session, "min_lng_deg", value = 15.00565)
      updateNumericInput(session = session, "max_lng_deg", value = 17.775915)
      updateNumericInput(session = session, "min_lat_deg", value = 50.137525)
      updateNumericInput(session = session, "max_lat_deg", value = 51.811374)
    }
    if (input$regions == "C") {
      updateNumericInput(session = session, "min_lng_deg", value = 17.226259)
      updateNumericInput(session = session, "max_lng_deg", value = 19.798648)
      updateNumericInput(session = session, "min_lat_deg", value = 52.324639)
      updateNumericInput(session = session, "max_lat_deg", value = 53.777326)
    }
    if (input$regions == "L") {
      updateNumericInput(session = session, "min_lng_deg", value = 21.64549)
      updateNumericInput(session = session, "max_lng_deg", value = 24.195892)
      updateNumericInput(session = session, "min_lat_deg", value = 50.292191)
      updateNumericInput(session = session, "max_lat_deg", value = 52.284332)
    }
    if (input$regions == "F") {
      updateNumericInput(session = session, "min_lng_deg", value = 14.455995)
      updateNumericInput(session = session, "max_lng_deg", value = 16.456741)
      updateNumericInput(session = session, "min_lat_deg", value = 51.374567)
      updateNumericInput(session = session, "max_lat_deg", value = 53.109896)
    }
    if (input$regions == "E") {
      updateNumericInput(session = session, "min_lng_deg", value = 18.105708)
      updateNumericInput(session = session, "max_lng_deg", value = 20.65611)
      updateNumericInput(session = session, "min_lat_deg", value = 50.878127)
      updateNumericInput(session = session, "max_lat_deg", value = 52.405142)
    }
    if (input$regions == "K") {
      updateNumericInput(session = session, "min_lng_deg", value = 19.161047)
      updateNumericInput(session = session, "max_lng_deg", value = 21.447614)
      updateNumericInput(session = session, "min_lat_deg", value = 49.227688)
      updateNumericInput(session = session, "max_lat_deg", value = 50.502291)
    }
    if (input$regions == "W") {
      updateNumericInput(session = session, "min_lng_deg", value = 19.205019)
      updateNumericInput(session = session, "max_lng_deg", value = 23.052609)
      updateNumericInput(session = session, "min_lat_deg", value = 50.975075)
      updateNumericInput(session = session, "max_lat_deg", value = 53.503772)
    }
    if (input$regions == "O") {
      updateNumericInput(session = session, "min_lng_deg", value = 16.984406)
      updateNumericInput(session = session, "max_lng_deg", value = 18.710325)
      updateNumericInput(session = session, "min_lat_deg", value = 49.980923)
      updateNumericInput(session = session, "max_lat_deg", value = 51.166966)
    }
    if (input$regions == "R") {
      updateNumericInput(session = session, "min_lng_deg", value = 21.20393)
      updateNumericInput(session = session, "max_lng_deg", value = 23.545463)
      updateNumericInput(session = session, "min_lat_deg", value = 49.010515)
      updateNumericInput(session = session, "max_lat_deg", value = 50.807345)
    }
    if (input$regions == "B") {
      updateNumericInput(session = session, "min_lng_deg", value = 21.579532)
      updateNumericInput(session = session, "max_lng_deg", value = 24.107948)
      updateNumericInput(session = session, "min_lat_deg", value = 52.324639)
      updateNumericInput(session = session, "max_lat_deg", value = 54.408741)
    }
    if (input$regions == "G") {
      updateNumericInput(session = session, "min_lng_deg", value = 16.764548)
      updateNumericInput(session = session, "max_lng_deg", value = 19.622758)
      updateNumericInput(session = session, "min_lat_deg", value = 53.490701)
      updateNumericInput(session = session, "max_lat_deg", value = 54.853885)
    }
    if (input$regions == "S") {
      updateNumericInput(session = session, "min_lng_deg", value = 18.083663)
      updateNumericInput(session = session, "max_lng_deg", value = 19.974479)
      updateNumericInput(session = session, "min_lat_deg", value = 49.376673)
      updateNumericInput(session = session, "max_lat_deg", value = 51.091125)
    }
    if (input$regions == "T") {
      updateNumericInput(session = session, "min_lng_deg", value = 19.688716)
      updateNumericInput(session = session, "max_lng_deg", value = 21.931311)
      updateNumericInput(session = session, "min_lat_deg", value = 50.165683)
      updateNumericInput(session = session, "max_lat_deg", value = 51.347126)
    }
    if (input$regions == "N") {
      updateNumericInput(session = session, "min_lng_deg", value = 19.248992)
      updateNumericInput(session = session, "max_lng_deg", value = 22.854733)
      updateNumericInput(session = session, "min_lat_deg", value = 53.136267)
      updateNumericInput(session = session, "max_lat_deg", value = 54.408741)
    }
    if (input$regions == "P") {
      updateNumericInput(session = session, "min_lng_deg", value = 15.819141)
      updateNumericInput(session = session, "max_lng_deg", value = 19.183033)
      updateNumericInput(session = session, "min_lat_deg", value = 51.168365)
      updateNumericInput(session = session, "max_lat_deg", value = 53.673324)
    }
    if (input$regions == "Z") {
      updateNumericInput(session = session, "min_lng_deg", value = 14.192174)
      updateNumericInput(session = session, "max_lng_deg", value = 16.984424)
      updateNumericInput(session = session, "min_lat_deg", value = 52.605758)
      updateNumericInput(session = session, "max_lat_deg", value = 54.61284)
    }
    if (input$regions == "none") {
      updateNumericInput(session = session, "min_lng_deg", value = 20.5)
      updateNumericInput(session = session, "max_lng_deg", value = 21)
      updateNumericInput(session = session, "min_lat_deg", value = 52.8)
      updateNumericInput(session = session, "max_lat_deg", value = 53.5)
    }
    
    
  })
  
  numbers <- reactive({
    shiny::validate(
      need(is.numeric(input$min_lat_deg), "Please input a number"),
      need(is.numeric(input$max_lat_deg), "Please input a number"),
      need(is.numeric(input$min_lng_deg), "Please input a number"),
      need(is.numeric(input$max_lng_deg), "Please input a number")
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
      addRectangles(input$min_lng_deg, input$min_lat_deg,
                    input$max_lng_deg, input$max_lat_deg, # dobra skladnia, nie dziala wczyt
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
    updateNumericInput(session = session, "min_lng_deg", value = min(rect_lats))
    updateNumericInput(session = session, "max_lng_deg", value = max(rect_lats))
    updateNumericInput(session = session, "min_lat_deg", value = min(rect_longs))
    updateNumericInput(session = session, "max_lat_deg", value = max(rect_longs))

  })
  
  
  # Call Onclick
  renderTab <- function(){
    
    disable("btn")
    show_modal_spinner(
     spin = "cube-grid",
     text = "Please wait. Calculation in progress...",
     color = "#6B8EB7"
   )
    # globalne
    xmin <<- input$min_lng_deg
    xmax <<- input$max_lng_deg 
    ymin <<- input$min_lat_deg  
    ymax <<- input$max_lat_deg
    
    results <- predict_from_area(xmin, ymin, xmax, ymax, n = 50)
    model <<- results[[1]] # globalne
    result <<- results[[2]] # globalne
    explainer <<- results[[3]]
    data_report <<- results[[4]]
    prediction <<- head(result[order(as.vector(result$score), decreasing=TRUE),],5) # global
    
    output$table <- DT::renderDataTable(prediction,
                                        colnames = c('Score', 'Gmina', 'Longitude', 'Latitude'))
    remove_modal_spinner()
    enable("btn")
    enable("downloadReport")
    
  }
  onclick("btn", renderTab)
  
  output$downloadReport <- downloadHandler(
    # filename = function() {
    #   paste('Report', sep = '.', switch(
    #     input$format, HTML = 'html', PDF = 'pdf'
    #   ))
    # },
    filename = "Report.html",
    
    content = function(file) {
      
      library(rmarkdown)
      out <- render('report.Rmd',
                    #switch( input$format,
                    #        HTML = html_document(), PDF = pdf_document()),
                    params = list(table = prediction, 
                                  lat_min = input$min_lat_deg, 
                                  lat_max = input$max_lat_deg, 
                                  lng_min = input$min_lng_deg, 
                                  lng_max = input$max_lng_deg,
                                  explainer = explainer,
                                  data = data_report
                    ))
      file.rename(out, file)
      
      # rmarkdown::render("report.Rmd", output_file = file)
    }
  )
  
  # proba stworzenia animacji podczas generowania raportu
  
  # generateReport <- function() {
  #   disable("downloadReport")
  #   show_modal_spinner(
  #     spin = "cube-grid",
  #     text = "Please wait. Generating report...",
  #     color = "#6B8EB7"
  #   )
  #   
  #   output$downloadReport <- downloadHandler(
  #     # filename = function() {
  #     #   paste('Report', sep = '.', switch(
  #     #     input$format, HTML = 'html', PDF = 'pdf'
  #     #   ))
  #     # },
  #     filename = "Report.html",
  #     
  #     content = function(file) {
  #       
  #       library(rmarkdown)
  #       out <- render('report.Rmd',
  #                     #switch( input$format,
  #                     #        HTML = html_document(), PDF = pdf_document()),
  #                     params = list(table = prediction, 
  #                                   lat_min = input$min_lat_deg, 
  #                                   lat_max = input$max_lat_deg, 
  #                                   lng_min = input$min_lng_deg, 
  #                                   lng_max = input$max_lng_deg,
  #                                   explainer = explainer,
  #                                   data = data_report
  #                     ))
  #       file.rename(out, file)
  #       
  #       # rmarkdown::render("report.Rmd", output_file = file)
  #     }
  #   )
  #   
  #   remove_modal_spinner()
  #   enable("downloadReport")
  # }
  # onclick("downloadReport", generateReport)
  
}   

shinyApp(ui = ui, server = server)

