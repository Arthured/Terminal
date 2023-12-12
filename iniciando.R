#Arturo Huerta Esguerra

#librerias
library(shiny)
library(tm)
library(shinydashboard)

ui<-dashboardPage(title= "Dashboard",skin= "green",
  dashboardHeader(title='Proyecto',
                  dropdownMenu(type = "message",
                               messageItem(from="Arturo",
                                           "Hola")),
                  dropdownMenu(type = "notifications",
                               notificationItem(text = "Hola como estas"))
  ),
  dashboardSidebar(),
  dashboardBody()
)
server<-function(input,output){
}
shinyApp(ui,server)
