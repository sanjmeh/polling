library(wrapr)
library(lubridate)
library(magrittr)
library(shiny)
library(shinyjs)
library(shinyalert)
library(shinyWidgets)
library(data.table)
library(ggplot2)
source("www/func.R")


ui<- fluidPage(
  tags$head(
    tags$link(type="text/css", rel="stylesheet", href="main.css"),
    tags$script(src="getIP.js")
  ),
  textOutput("user"),
  #selectInput("user",label = "Your pen name",choices = c("Jack","Mary","Tom"),multiple = F,selectize = T),
  div(id="page1",
      textInput("random",label = "Enter a phrase"),br(),
      actionBttn("send",label = "SEND"), br(),br(),
      textOutput("current"),
      tableOutput("ip"),
      actionButton("exit","Exit App",icon = icon("sign-out-alt"),width = "200px")
  )
)

server<- function(input,output,session){
  observe({
    updateSelectInput(session,inputId = "user",selected = input$user)
    })
  
  uname <- getname()
  
  output$user <- renderText({
    uname
  })
  
  newword <- eventReactive(input$send,input$random)
  
  observe(
    fwrite(data.table(timestamp=Sys.time(),user=uname,phrases=newword()),file = "test.csv",append = T,quote = T,sep = "|")
  )
  contents <- reactiveFileReader(intervalMillis = 200,filePath = "test.csv",readFunc = fread,header=T,session = session)
  output$current <- renderText(contents()$phrases %>% dQuote())
  
  IP <- reactive({ input$getIP })
  
  output$ip <- renderTable({
    data.frame(IP())
  })
  
  observeEvent(input$getIP,{
    if(file.exists("www/ip.csv")) fwrite(data.frame(IP()),"www/ip.csv",append = T) else
      fwrite(data.frame(IP()),"www/ip.csv")
  })
  
  observeEvent(input$exit,stopApp(1))
}

shinyApp(ui,server)