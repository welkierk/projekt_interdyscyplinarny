library(shiny)
library(shinythemes)
library(shinyjs)


ui <- fluidPage(
  theme = shinytheme('flatly'),
  titlePanel("Wybór kryteriów "),
  sidebarPanel(
    radioButtons(inputId = "wybor_danych", label = "Sposób określenia lokalizacji",
                 choices = c("Regiony","Współrzędne geograficzne")),
    useShinyjs(),
    h3("Państwo"),
    fluidRow(selectInput(inputId = "panstwo", label = "",
                         choices = list(Polska = c("dolnośląskie" = "D",
                                                   "kujawsko-pomorskie" = "C",
                                                   "lubelskie" = "L",
                                                   "lubuskie" = "F",
                                                   "łódzkie" = "E",
                                                   "małopolskie" = "K",
                                                   "mazowieckie" = "W",
                                                   "opolskie" = "O",
                                                   "podkarpackie" = "R",
                                                   "podlaskie" = "B",
                                                   "pomorskie" = "G",
                                                   "śląskie" = "S",
                                                   "świętokrzyskie" = "T",
                                                   "warmińsko-mazurskie" = "N",
                                                   "wielkopolskie"= "P",
                                                   "zachodniopomorskie" = "Z")), selected = NULL, multiple = TRUE)),
    h3("Szerokość geograficzna"),
    fluidRow(#wybór szerokości geograficznej
      h4("Minimalna"),
      column(3,
      radioButtons(inputId = "szerokosc_od_polkula", label = "Półkula",
                   choices = c("W","E"))),
      column(3,numericInput(inputId = "szerokosc_od_stopnie", label = "Stopnie",
                   value = 45, min = 0, max = 90)),
      column(3,numericInput(inputId = "szerokosc_od_minuty", label = "Minuty",
                   value = 30, min = 0, max = 59))),
    fluidRow(
      h4("Maksymalna"),
      column(3, radioButtons(inputId = "szerokosc_do_polkula", label = "Półkula",
                   choices = c("W","E"))),
      column(3, numericInput(inputId = "szerokosc_do_stopnie", label = "Stopnie",
                                   value = 45, min = 0, max = 90)),
      column(3, numericInput(inputId = "szerokosc_do_minuty", label = "Minuty",
                                   value = 30, min = 0, max = 59))),
    h3("Długość geograficzna"),
    fluidRow(#wybór długości geograficznej
      h4("Minimalna"),
      column(3, radioButtons(inputId = "dlugosc_od_polkula", label = "Półkula",
                                   choices = c("N","S"))),
        column(3, numericInput(inputId = "dlugosc_od_stopnie", label = "Stopnie",
                                   value = 90, min = 0, max = 180)),
        column(3, numericInput(inputId = "dlugosc_od_minuty", label = "Minuty",
                                   value = 30, min = 0, max = 59))),
    fluidRow(
      h4("Maksymalna"),
      column(3, radioButtons(inputId = "dlugosc_do_polkula", label = "Półkula",
                             choices = c("N","S"))),
      column(3, numericInput(inputId = "dlugosc_do_stopnie", label = "Stopnie",
                             value = 90, min = 0, max = 180)),
      column(3, numericInput(inputId = "dlugosc_do_minuty", label = "Minuty",
                             value = 30, min = 0, max = 59))
    ),
    h3("Bliskość jakich zasobów jest interesująca?"),
    fluidRow(selectInput(inputId = "zasoby", label = "",
    choices = list("Rzeki" = "rzeki", "Uprawy" = "uprawy", "Lasy" = "lasy"), selected = NULL, multiple = TRUE)),
    h3("Czy chcesz uwzględnić przynależność do specjalnej strafy ekonomicznej (SSE)?"),
    fluidRow(column(3, radioButtons(inputId = "strefa_ekon", label = "",choices = c("Tak", "Nie"), selected="Nie"))),
    disabled(actionButton('submit', 'Zatwierdź'))),
  mainPanel(
    textOutput('szukane')
  ))
                    
  server <- function(input, output) {
    #wybor jednej opcji wybrania lokalizacji
    observeEvent(input$wybor_danych, {
      if(input$wybor_danych == "Współrzędne geograficzne"){
        enable('szerokosc_od_polkula') 
        enable('szerokosc_od_stopnie')
        enable('szerokosc_od_minuty')
        enable('szerokosc_do_polkula')
        enable('szerokosc_do_stopnie')
        enable('szerokosc_do_minuty')
        enable('dlugosc_do_polkula')
        enable('dlugosc_do_stopnie')
        enable('dlugosc_do_minuty')
        enable('dlugosc_od_polkula')
        enable('dlugosc_od_stopnie')
        enable('dlugosc_od_minuty')
      disable('panstwo')
    }
      else{
      disable('szerokosc_od_polkula') 
      disable('szerokosc_od_stopnie')
      disable('szerokosc_od_minuty')
      disable('szerokosc_do_polkula')
      disable('szerokosc_do_stopnie')
      disable('szerokosc_do_minuty')
      disable('dlugosc_do_polkula')
      disable('dlugosc_do_stopnie')
      disable('dlugosc_do_minuty')
      disable('dlugosc_od_polkula')
      disable('dlugosc_od_stopnie')
      disable('dlugosc_od_minuty')
      enable('panstwo')
    }})
    #uruchomienie guzika zatwierdz
    toListen <- reactive({
      list(input$wybor_danych, input$panstwo, input$szerokosc_od_stopnie, input$szerokosc_od_stopnie,
           input$szerokosc_do_minuty, input$szerokosc_do_stopnie, input$dlugosc_od_minuty, input$dlugosc_od_stopnie,
           input$dlugosc_do_minuty, input$dlugosc_do_stopnie, input$zasoby)})
    observeEvent(toListen(),{
      if(input$wybor_danych=="Regiony" && !is.null(input$panstwo) && !is.null(input$zasoby)){
        enable('submit')
      }
      else if(input$wybor_danych=="Współrzędne geograficzne" && 
              !is.null(input$szerokosc_od_minuty) && 
              !is.null(input$szerokosc_od_stopnie) &&
              !is.null(input$szerokosc_do_minuty) &&
              !is.null(input$szerokosc_do_stopnie) &&
              !is.null(input$dlugosc_od_minuty) &&
              !is.null(input$dlugosc_od_stopnie) &&
              !is.null(input$dlugosc_do_minuty) &&
              !is.null(input$dlugosc_do_stopnie)
              #&&
              #input$szerokosc_od_minuty >=0&&
              #input$szerokosc_do_minuty >=0 &&
              #input$dlugosc_od_minuty >= 0 &&
              #input$dlugosc_do_minuty >=0 &&
              #input$szerokosc_od_minuty <60 &&
              #input$szerokosc_do_minuty <60 &&
              #input$dlugosc_od_minuty <60 &&
              #input$dlugosc_do_minuty <60 &&
              #input$szerokosc_od_stopnie >=0 &&
              #input$szerokosc_do_stopnie >=0 &&
              #input$dlugosc_od_stopnie >= 0 &&
              #input$dlugosc_do_stopnie >=0 &&
              #(input$szerokosc_od_stopnie < 90 | (input$szerokosc_od_stopnie==90 && input$szerokosc_od_minuty==0)) &&
              #(input$szerokosc_do_stopnie < 90  | (input$szerokosc_do_stopnie==90 && input$szerokosc_do_minuty==0)) &&
              #(input$dlugosc_od_stopnie < 180 | (input$dlugosc_od_stopnie == 180 && input$dlugosc_od_minuty == 0)) &&
              #(input$dlugosc_do_stopnie <180 | (input$dlugosc_do_stopnie==180 && input$dlugosc_do_minuty == 0))
              #&&!is.null(input$zasoby) 
              ){
        enable('submit')
      }
      else{
        disable('submit')
      }})
      observeEvent(input$submit, {
        output$szukane <- renderText({
          paste0("Sposób wyszukania: ", input$wybor_danych)
      })
    })
    }


shinyApp(ui, server)
