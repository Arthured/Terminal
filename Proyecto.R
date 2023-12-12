#Librerias
library(shiny)
library(tidyverse)
library(lubridate)

#Llamar a la base de datos //desde github para no depender de la computadora
pokemones <- read.csv("https://raw.githubusercontent.com/lgreski/pokemonData/master/Pokemon.csv")
ui <- fluidPage(
  titlePanel("Pokedex de la 1ra a la 9ma generacion"),#titulo
  navbarPage(
    tabsetPanel(
      tabPanel("Inicio",
               mainPanel(h3("La Pokédex, en el mundo ficcion de Pokémon, es una enciclopedia electrónica portátil que los entrenadores Pokémon llevan consigo para registrar automáticamente las fichas de todas las diversas especies Pokémon vistas y capturadas durante su viaje como entrenadores"),
                 imageOutput("Figura",width=100,height="400px"))
               ),
      tabPanel("DATOS",DT::dataTableOutput("tabla")),#Imprime todos los pokemons
      tabPanel("RESUMEN",#Hace un resumen de las estadisticas
               sidebarPanel(
                 selectizeInput("Generation",label = h4("Seleccionar generacion"), 
                                choices = pokemones$Generation, multiple=TRUE)),
               verbatimTextOutput("Resumen")),
      tabPanel("TOTAL",#Hace un resumen de las estadisticas
               sidebarPanel(
                 selectInput("tabla",label = h4("Seleccionar la grafica"), 
                                choices = c("Histograma","Boxplot","Puntos","EDF"),
                                multiple=FALSE)),
               mainPanel(plotOutput("totales",width=500,height = 400,))),
      tabPanel("Ataque",#Hace un resumen de las estadisticas
               sidebarPanel(
                 selectInput("tabla",label = h4("Seleccionar la grafica"), 
                             choices = c("Histograma","Boxplot","Puntos","EDF"),
                             multiple=FALSE)),
               mainPanel(plotOutput("attack",width=500,height = 400,))),
      tabPanel("Defensa",#Hace un resumen de las estadisticas
               sidebarPanel(
                 selectInput("tabla",label = h4("Seleccionar la grafica"), 
                             choices = c("Histograma","Boxplot","Puntos","EDF"),
                             multiple=FALSE)),
               mainPanel(plotOutput("def",width=500,height = 400,))),
      tabPanel("Velocidad",#Hace un resumen de las estadisticas
               sidebarPanel(
                 selectInput("tabla",label = h4("Seleccionar la grafica"), 
                             choices = c("Histograma","Boxplot","Puntos","EDF"),
                             multiple=FALSE)),
               mainPanel(plotOutput("vel",width=500,height = 400,))),
      tabPanel("Preguntas",
               sidebarPanel(
                 textInput("pregunta","Ingrese una pregunta"),
                 h4("Por favor en español y todo en minusculas")
               ))
    )
  )
)

server <- function(input, output) {

  PokemonSub <- reactive({ pokemones %>%
      filter(Generation %in% input$Generation)
  })
  #ingresa un gif
  output$Figura<-renderImage(
    return(list(src="C://Users//artur//OneDrive//Documentos//uni//7mo//Terminal//shiny//www//pokedex.gif", align = "left",height='250px',width='500px'))
  )
  #me imprime la tabla de todos los pokemones
  output$tabla <- DT::renderDataTable(DT::datatable({pokemones},
                                                    options=list(lengthMenu=list(c(10,50,100),c("10","50","100"))
                                                    ), filter = "top", selection = "multiple", style = "bootstrap" 
  ))
  #Hace un resumen de los pokemones
  output$Resumen <- renderPrint({summary(PokemonSub())})

  elegir<-function(tabla,estadistica){
    switch(
      tabla,
      Histograma= ggplot(pokemones,  mapping = aes(estadistica)) +   
                    geom_histogram(alpha = 0.5, position = "identity"),
      Boxplot=ggplot(pokemones,  mapping = aes(estadistica)) +   
                geom_boxplot(alpha = 0.5, position = "identity"),
      Puntos=ggplot(pokemones,  mapping = aes(estadistica)) +   
               geom_histogram(alpha = 0.5, position = "identity"),
      EDF=h4("hola")
    )
  }
  total <- pokemones$Total
  output$totales<-renderPlot ({
    elegir(input$tabla,estadistica = total)}
  )
  totalattack <- pokemones$Attack
  output$attack<-renderPlot ({
    elegir(input$tabla,estadistica = totalattack)}
    )
  totaldef <- pokemones$Defense
  output$def<-renderPlot ({
    elegir(input$tabla,estadistica = totaldef)}
  )
  totalvel <- pokemones$Speed
  output$vel<-renderPlot ({
    elegir(input$tabla,estadistica = totalvel)}
  )
}
shinyApp(ui = ui, server = server)