library(wrapr)
library(lubridate)
library(magrittr)
library(shiny)
library(shinyjs)
library(shinyalert)
library(shinyWidgets)
library(data.table)
library(ggplot2)

ui<- fluidPage(
  tags$head(
    tags$link(type="text/css", rel="stylesheet", href="main.css")
    ),
  textInput("random",label = "Enter your word"),
  actionBttn("send",label = "SEND"), br(),br(),
  textOutput("current"),
  actionBttn("exit","Exit App",icon = icon("sign-out-alt"),style = "material-circle",color = "danger")
               )

server<- function(input,output,session){
  newword <- eventReactive(input$send,input$random)
  observe(
    fwrite(data.table(timestamp=now(),phrases=newword()),file = "test.csv",append = T,quote = T,sep = "|")
  )
  contents <- reactiveFileReader(intervalMillis = 200,filePath = "test.csv",readFunc = fread,header=T,session = session)
  output$current <- renderText(contents()$phrases %>% dQuote())
  observeEvent(input$exit,stopApp(1))
}

shinyApp(ui,server)