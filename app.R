library(shiny)
ui <- fluidPage(
  titlePanel("Wybór kryteriów "),
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
    h4("Od"),
    column(3,
    radioButtons(inputId = "szerokosc_od_polkula", label = "Półkula",
                 choices = c("W","E"))),
    column(3,numericInput(inputId = "szerokosc_od_stopnie", label = "Szerokość od w stopniach",
                 value = 45, min = 0, max = 90)),
    column(3,numericInput(inputId = "szerokosc_od_minuty", label = "Szerokość od w minutach",
                 value = 30, min = 0, max = 60))),
  fluidRow(
    h4("Do"),
    column(3, radioButtons(inputId = "szerokosc_do_polkula", label = "Półkula",
                 choices = c("W","E"))),
    column(3, numericInput(inputId = "szerokosc_do_stopnie", label = "Szerokość do w stopniach",
                                 value = 45, min = 0, max = 90)),
    column(3, numericInput(inputId = "szerokosc_do_minuty", label = "Szerokość do w minutach",
                                 value = 30, min = 0, max = 60))),
  h3("Długość geograficzna"),
  fluidRow(#wybór długości geograficznej
    h4("Od"),
    column(3, radioButtons(inputId = "dlugosc_od_polkula", label = "Półkula",
                                 choices = c("N","S"))),
      column(3, numericInput(inputId = "dlugosc_od_stopnie", label = "Długość od w stopniach",
                                 value = 90, min = 0, max = 180)),
      column(3, numericInput(inputId = "dlugosc_od_minuty", label = "Długość od w minutach",
                                 value = 90, min = 0, max = 180))),
  fluidRow(
    h4("Do"),
    column(3, radioButtons(inputId = "dlugosc_do_polkula", label = "Półkula",
                           choices = c("N","S"))),
    column(3, numericInput(inputId = "dlugosc_do_stopnie", label = "Długość do w stopniach",
                           value = 90, min = 0, max = 180)),
    column(3, numericInput(inputId = "dlugosc_do_minuty", label = "Długość do w minutach",
                           value = 90, min = 0, max = 180))
  ),
  h3("Bliskość jakich zasobów jest interesująca"),
  fluidRow(selectInput(inputId = "zasoby", label = "",
  choices = list("Rzeki" = "rzeki", "Uprawy" = "uprawy", "Lasy" = "lasy"), selected = NULL, multiple = TRUE)),
  h3("Czy chcesz uwzględnić przynależność do specjalnej strafy ekonomicznej (SSE)?"),
  fluidRow(column(3, radioButtons(inputId = "strefa_ekon", label = "",choices = c("Tak", "Nie"), selected="Nie"))))
                    
  server <- function(input, output) {
}

shinyApp(ui, server)
