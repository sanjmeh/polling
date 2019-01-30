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
  textInput("random",label = "Enter your word"),
  actionBttn("send",label = "SEND"), br(),br(),
  textOutput("current",h1)
               )

server<- function(input,output,session){
  newword <- eventReactive(input$send,input$random)
  observe(
    fwrite(data.table(timestamp=now(),phrases=newword()),file = "test.csv",append = T,quote = T,sep = "|")
  )
  contents <- reactiveFileReader(intervalMillis = 200,filePath = "test.csv",readFunc = fread,header=T,session = session)
  output$current <- renderText(contents()$phrases %>% dQuote())
}

shinyApp(ui,server)