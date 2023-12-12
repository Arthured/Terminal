library(shiny)
#preprosesado
mpgData<-mtcars
mpgData$am<-factor(mpgData$am,labels=c("Automatico","Manual"))
b<-100
x<-c(1:b)
#interfaz
ui<-pageWithSidebar(
  #titulo
  headerPanel("Millas por galon"),
  #creamos un side bar panle para las entradas
  sidebarPanel(
    #Seleciona variables en mpg
    selectInput("var","variable",
                choices = c("cilindros"="cyl",
                            "Transmision"="am",
                            "Motores"="gear")),
    #
    checkboxInput("outliers","Mostrar ouliers",TRUE),
    
  ),
  
  #main visualizar el resultado
  mainPanel(
    #texto de la formula
    h3(textOutput("text")),
    #plot
    plotOutput("mpgPlot")
  )
)
server<-function(input,output)
{
  #Calcular el texto de la formula 
  #es una expresion reactiva
  formulaTexto<-reactive({
    paste("mpg ~",input$var)
  })
  
  output$texto<-renderText({
    formulaTexto()
  })
  #generar el plot
  output$mpgPlot<-renderPlot({
    boxplot(as.formula(formulaTexto()),
            data=mpgData,
            outline=input$outliers,
            col="red",pch=19)
  })
}
shinyApp(ui,server)

