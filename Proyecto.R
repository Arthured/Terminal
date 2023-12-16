#Librerias
library(shiny)
library(shinythemes)
library(bslib)
library(tidyverse)
library(lubridate)
library(tm)
library(PerformanceAnalytics)


#Llamar a la base de datos //desde github para no depender de la computadora
pokemones <- read.csv("https://raw.githubusercontent.com/lgreski/pokemonData/master/Pokemon.csv",encoding="UTF-8")

totalspdef<- pokemones$Sp..Def
totalspatk<-pokemones$Sp..Atk
totaldef <- pokemones$Defense
total <- pokemones$Total
totalvel <- pokemones$Speed
totalattack <- pokemones$Attack

ui <- fluidPage(
  theme = shinytheme("spacelab"),
  #theme=shinytheme("cyborg"),
  #shinythemes::themeSelector(), 
  titlePanel("Pokedex de la 1ra a la 9ma generacion"),#titulo
  navbarPage(
    tabsetPanel(
      tabPanel("Inicio",
               mainPanel(h3("La Pokédex, en el mundo ficcion de Pokémon, es una enciclopedia electrónica portátil que los entrenadores Pokémon llevan consigo para registrar automáticamente las fichas de todas las diversas especies Pokémon vistas y capturadas durante su viaje como entrenadores"),
                         h4("La Pokedex da el nombre del pokemon, el tipo del pokemon, la generacion,el total de sus estadisticas, el ataque, defensa, velocidad, ataque especial, defensa especial y puntos de salud"),
                         imageOutput("Figura",width=100,height="400px"))
               ),
      tabPanel("Datos",DT::dataTableOutput("tabla")),#Imprime todos los pokemons
      tabPanel("Resumen",#Hace un resumen de las estadisticas
               sidebarPanel(
                 selectizeInput("Generation",label = h4("Seleccionar generacion"), 
                                choices = pokemones$Generation, multiple=TRUE)),
               verbatimTextOutput("Resumen")),
      
      tabPanel("Graficas",#Hace un resumen de las estadisticas
               sidebarPanel(
                 selectInput("tabla",label = h4("Seleccionar la grafica"), 
                             choices = c("Histograma","Boxplot","Puntos","ECDF"),
                             multiple=FALSE),
                 selectInput("estadistica",label = h4("Seleccionar estadistica"), 
                             choices = c("Total","Ataque","Defensa","Ataque especial","Defensa especial","Velocidad","Puntos de salud"),
                             multiple=FALSE)),
               
               mainPanel(plotOutput("esta",width=500,height = 400))),
      
      tabPanel("Preguntas",
               sidebarPanel(
                 textInput("pregunta","Ingrese una pregunta",width="100%"),
                 h4("Por favor en español y todo en minusculas"),
                 tags$style(type="text/css", "#string { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}")
               ),
               mainPanel(
                 textOutput("respuesta"),
                 tags$style(type='text/css', "#button { vertical-align: middle; height: 50px; width: 100%; font-size: 30px;}")
               )
               ),
      ###
      tabPanel("Correlacion",
               sidebarPanel(
                 selectInput("estadistica1",label = h4("Seleccionar estadistica 1"), 
                             choices = c("Total","Ataque","Defensa","Ataque especial","Defensa especial","Velocidad","Puntos de salud"),
                             multiple=FALSE),
                 selectInput("estadistica2",label = h4("Seleccionar estadistica 2"), 
                             choices = c("Total","Ataque","Defensa","Ataque especial","Defensa especial","Velocidad","Puntos de salud"),
                             multiple=FALSE),
                 selectInput("metodo",label = h4("Seleccionar tipo de corelacion"), 
                             choices = c("pearson","spearman","kendall"),
                             multiple=FALSE)
                 
                 ),
               mainPanel(plotOutput("correlacion",width=500,height = 400))
               )
      ####         
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
                                                    options=list(lengthMenu=list(c(10,50,100),c("10","50","100"))), 
                                                    filter = "top", selection = "multiple", style = "bootstrap" ))
  #Hace un resumen de los pokemones
  output$Resumen <- renderPrint({summary(PokemonSub()[c("Total","HP","Attack","Defense","Sp..Atk","Sp..Def","Speed","Generation")])})
  
  ################################Grafica###########################################
  #Funciones para la parte de graficar
  elegir<-function(tabla,estadistica){
    switch(
      tabla,
      "Histograma"= ggplot(pokemones,  mapping = aes(estadistica)) +   
        geom_histogram(alpha = 0.5, main="Histograma",position = "identity",col="blue"),
      "Boxplot"=ggplot(pokemones,  mapping = aes(estadistica)) +   
        geom_boxplot(alpha = 0.5, main="Tabla bloxplot",position = "identity",col="blue"),
      "Puntos"=ggplot(pokemones,  mapping = aes(y=estadistica,x=c(1:length(estadistica)))) +   
        geom_point(alpha = 0.5, position = "identity",color="red")+
        geom_smooth(col="black"),
      "ECDF"={par(mfrow=c(1,1))
          M<-ecdf(estadistica)
          plot(M,verticals=TRUE, do.points=FALSE, main=" 95%",
             xlab="Puntos", ylab=" ECDF",
             col="black")
          n<-length(estadistica)
          alpha <-0.05
          en <- sqrt(log(2/alpha)/(2*n))
          L_DKW <- pmax(M(estadistica)-en,0)
          U_DKW <- pmin(M(estadistica)+en,1)
          points(sort(estadistica), L_DKW[order(estadistica)], "l", col="red")
          points(sort(estadistica), U_DKW[order(estadistica)], "l", col="blue")}
    )
  }
  
  #Elige la estadistica que quieres graficar 
  estadis<-function(ala)
  {
    switch(
      ala,
      Total=return(total),
      Ataque=return(totalattack),
      Defensa=return(totaldef),
      Velocidad=return(totalvel),
      "Ataque especial"=return(pokemones$Sp..Atk),
      "Defensa especial"=return(pokemones$Sp..Def),
      "Puntos de salud"=return(pokemones$HP)
    )  
  }
  
  #manda toda la informacion para mostrarla  
  output$esta<-renderPlot ({
    elegir(input$tabla,estadis(input$estadistica))}
    )
  #################################################################
  ######################preguntas###################################
  # Procesar la consulta del usuario
  filtrar_texto<-function(texto){
    consulta_usuario<-texto
    consulta_procesada <- tolower(consulta_usuario)
    consulta_procesada <- removePunctuation(consulta_procesada)
    #consulta_procesada <- removeNumbers(consulta_procesada)
    consulta_procesada <- removeWords(consulta_procesada, stopwords("spanish"))
    consulta_procesada <- stripWhitespace(consulta_procesada)
    lista_palabras<-strsplit(consulta_usuario, " ")[[1]]
    return(lista_palabras)
  }
  lista_estadistica<-c("total","ataque","defensa","ataque_especial","defensa_especial","velocidad","puntos_de_salud")
  lista_funciones<-c("media","varianza","mediana", "rango", "alto","bajo","relacion","mayor", "menor", "desviacion_estandar","promedio")
  
  stadis<-function(alas)
  {
      switch(
        alas,
        "total"=return(total),
        "ataque"=return(totalattack),
        "defensa"=return(totaldef),
        "velocidad"=return(totalvel),
        "ataque_especial"=return(pokemones$Sp..Atk),
        "defensa_especial"=return(pokemones$Sp..Def),
        "puntos_de_salud"=return(pokemones$HP)
      )     
  }
  valor2<-function(a)
  {
    z<-TRUE
    for (x in 1:length(a)){
      z<-z&a[x]
    }
    return(z)
  }
  valor<-function(a)
  {
    z<-FALSE
    for (x in 1:length(a)){
      z<-z|a[x]
    }
    return(z)
  }
  #La funcion manda a filtrar separa el texto y manda lo que le pides
  question<-function(res){
    a<-filtrar_texto(res)
    if(length(a)!=0){
      if(valor(c((a %in% lista_funciones) ) ) & valor(c((a %in% lista_estadistica)))){
        sta<-a[a %in% lista_estadistica]
        func<-a[a %in% lista_funciones]
        if(valor(c("relacion") %in% func))
        {
          return(cor(stadis(sta[1]),stadis(sta[2])))
        }
        else{
          s<-stadis(sta[1])
          #cat(s)
          regre<-respuesta(func,s,sta[1])
          return(regre)
        }
      }
      else{
        return("Lo siento tu pregunta esta incompleta")
      }
  }
  else{
    return("Lo siento no puedo entenderte")
  }
  }
  #la funcion recibe la funcion pedida y a que parametros se lo damos 
  respuesta<-function(questions,estadistica,sta){
    switch(
      questions,
      "promedio"=c("El promedio de",sta,"es",mean(estadistica)),
      "media"=c("La media de",sta,"es",mean(estadistica)),
      "rango"=c("El rango de",sta,"es",range(estadistica)),
      "alto"=c("El",sta,  "mas alto es",c(max(estadistica))),
      "bajo"=c("El",sta,  "mas bajo es",min(estadistica)),
      "mayor"=c("Los pokemones con más ",sta,"son:",unlist(pokemonmimax(sta,max(estadistica))$Name),"con",max(estadistica)),
      "menor"=c("Los pokemones con menos ",sta,"son:",unlist(pokemonmimax(sta,min(estadistica))$Name),"con",min(estadistica)),
      "mediana"=c("La mediana de",sta,"es",median(estadistica)),
      "desviacion_estandar"=c("La desviacion estandar de",sta,"es",sd(estadistica)),
      "varianza"=c("La varianza de",sta,"es",var(estadistica) )
      )  
    
  }
  #
  pokemonmimax<-function(estadistica,minmax)
  {
    switch(
      estadistica,
      "total"=filter(pokemones,Total==minmax),
      "ataque"=filter(pokemones,Attack==minmax),
      "defensa"=filter(pokemones,Defense==minmax),
      "velocidad"=filter(pokemones,Speed==minmax),
      "ataque_especial"=filter(pokemones,Sp..Atk==minmax),
      "defensa_especial"=filter(pokemones,Sp..Def==minmax),
      "puntos_de_salud"=filter(pokemones,HP==minmax)
    )
  }
  # 
  longitud<-reactive(strsplit(input$pregunta, " ")[[1]])
  
  output$respuesta<-renderText(
    question(input$pregunta)
  )
 #########################################################################
 
 ###################correlaciones#########################################
 
 output$correlacion<-renderPlot(
   chart.Correlation(data.frame(estadis(input$estadistica1),estadis(input$estadistica2)),#col.names=c(input$estadistica1,input$estadistica2)),
                     method = input$metodo,
                     main="Correlacion",#c("La correlacion entre",input$estadistica1,"y",input$estadistica2),
                     labels=c("Estadistica 1","Estadistica 2")
                     )
 )

}

shinyApp(ui = ui, server = server)
