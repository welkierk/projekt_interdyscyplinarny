library(shiny)
library(shinyjs)
library(dplyr)
library(httr)
timeout(1000)
source("model_from_random_points_combined.R")

ui <- fluidPage(
  numericInput("min_lat_deg", label = "Minimum latitude (degrees)", 
               value = 45, min = 0, max = 90),
  numericInput("min_lat_min", label = "Minimum latitude (minutes)", 
               value = 30, min = 0, max = 60),
  numericInput("max_lat_deg", label = "Maximum lattitude (degrees)", 
               value = 64, min = 0, max = 90),
  numericInput("max_lat_min", label = "Maximum lattitude (minutes)", 
               value = 12, min = 0, max = 60),
  numericInput("min_lon_deg", label = "Minimum longitude (degrees)", 
               value = 32, min = 0, max = 90),
  numericInput("min_lon_min", label = "Minimum longitude (minutes)", 
               value = 17, min = 0, max = 60),
  numericInput("max_lon_deg", label = "Maximum longitude (degrees)", 
               value = 56, min = 0, max = 90),
  numericInput("max_lon_min", label = "Maximum longitude (minutes)", 
               value = 0, min = 0, max = 60),
  
  verbatimTextOutput("value"),
  tableOutput('table'),
  useShinyjs(),
  
  actionButton("btn", "Predict")
  
)
server <- function(input, output) {
  
  numbers <- reactive({
    shiny::validate(
      need(is.numeric(input$min_lon_deg), "Please input a number"),
      need(is.numeric(input$min_lon_min), "Please input a number"),
      need(is.numeric(input$max_lon_deg), "Please input a number"),
      need(is.numeric(input$max_lon_min), "Please input a number"),
      need(is.numeric(input$min_lat_deg), "Please input a number"),
      need(is.numeric(input$min_lat_min), "Please input a number"),
      need(is.numeric(input$max_lat_deg), "Please input a number"),
      need(is.numeric(input$max_lat_min), "Please input a number")
    )
  })
  output$value <- renderPrint({ numbers() })

  

  # Call Onclick
   renderTab <- function(){
     xmin <- input$min_lat_deg + input$min_lat_min * 0.01
     xmax <- input$max_lat_deg + input$max_lat_min * 0.01
     ymin <- input$min_lon_deg + input$min_lon_min * 0.01
     ymax <- input$max_lon_deg + input$max_lon_min * 0.01
     
     result <- predict_from_area(xmin, ymin, xmax, ymax, n = 10)
     prediction <- head(result[order(as.vector(result$score), decreasing=TRUE),],5)
     output$table <- renderTable(prediction)

   }
   onclick("btn", renderTab)
}   

shinyApp(ui = ui, server = server)
