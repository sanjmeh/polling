library(stringr)
library(jsonlite)
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


ui<- fixedPage(
  useShinyalert(),
  useShinyjs(),
  tags$head(
    tags$link(type="text/css", rel="stylesheet", href="main.css"),
    tags$script(src="getIP.js")
  ),
  div(id="auth",
  textInput("confcode",label = "Conference code or assumed name",value = NULL,placeholder = "Enter conf code/user name"),
  fixedRow(
  column(4,checkboxInput("isadmin",label = "I am an admin",value = F)),
  column(8,hidden(passwordInput("pass",label = "Enter admin passcode:",value = NULL)))
  ),
    actionBttn("go","GO",style = "pill",color = "default"),
    actionBttn("exit1","EXIT",style = "pill",color = "warning")
  ),
  hidden(
      div(id="main",
          textOutput("user"),
          div(id="page1",
              textInput("random",label = "Enter a phrase"),br(),
              actionBttn("send",label = "SEND"), br(),br(),
              textOutput("current"), br(),
              tableOutput("ip"),
              actionButton("exit2","Exit App",icon = icon("sign-out-alt"),width = "200px")
          )
      ),
      div(id="adminsection",
          plotOutput("devices"),
          checkboxInput("broadcast",label = "Broadcast",value = F)
      )
  )
)

server<- function(input,output,session){
  utype<- NA
  cat("\nInitial value of utype is:",utype)
  uname<- character()
  observeEvent(input$isadmin,{
    if(input$isadmin==T) showElement("pass") else hideElement("pass")
  })
  
 
  observeEvent(input$go,ignoreNULL = T,{
    if(input$isadmin == T && input$pass =="abc123")
    {
      cat("\nPassed all criteria for admin..")
      shinyalert("SUCCESS - ADMIN","You are now the admin",type = "success")
      hideElement("auth")
      utype <<- "admin"
      cat("\nutype inside oberve event of SEND")
      print(utype)
      showElement("adminsection")
    } else{
      hideElement("adminsection")
      if(is.na(as.numeric(input$confcode)))  { # check if not numeric
        if(isactive(input$confcode)){ # if not numeric and an active user name
          utype <<- "partic"
          cat("\nutype changed to:",utype)
          uname <<- input$confcode
          #shinyalert("Success",paste("You have rejoined the conference on the assumed name",uname),type = "success")
          output$user <- renderText(uname)
          showElement("main")
          hideElement("auth")
          hideElement("adminsection")
        }
        else
          shinyalert(title = "Incorrect",text = "This assumed name is not used for this conference. Try conference code instead for new assumed name",type = "warning")
      } else # if numeric
      {
        if(as.numeric(input$confcode)==123){
          shinyalert("Welcome","Welcome to the conference facilitated by Sanjay Mehrotra",type = "success")
          uname <<- getname()
          output$user <- renderText(uname)
          showElement("main")
          hideElement("auth")
          hideElement("adminsection")
        } else 
          shinyalert(title = "Incorrect conf code",text = "Check the correct numeric code of 3 digits",type = "error")
      }
    }
    
  })
  
  
  observeEvent(input$broadcast, {
      cat("\nbroadcast checkbox event\n")
    cat("User Type")
    print(utype)
      if(utype=="partic" & input$broadcast==T) showElement("devices")
    })
       
  
  newword <- eventReactive(input$send,input$random)
  contents <- reactiveVal()
  observe({
    param_IP <- toJSON(IP(),auto_unbox = T)
    param_response <- toJSON(list(phrase=newword()),auto_unbox = T)
    logevent(eventcode ="IP",param=param_IP,user = NA)
    logevent(eventcode="ANS",param=param_response,user=uname)
    contents <<- reactiveFileReader(intervalMillis = 200,filePath = "www/log.csv",readFunc = fread,header=T,session = session)
  })
  
  observeEvent(contents(),{
    print(contents())
  })
  
  # output$current <- renderText(contents()$parameters %>% dQuote())
  # output$devices <- renderPlot({
  #   x1 <- contents()
  #   cat("\nhow does x1 appear inside:",x1)
  #   x1$timestamp %<>% ymd_hms(tz = "Asia/Kolkata")
  #   str(x1)
  #   ggplot(x1,aes(timestamp)) + geom_histogram(binwidth = 5)
  # })
  
  IP <- reactive({ input$getIP })
  
  observeEvent(input$getIP,{
    if(file.exists("www/ip.csv")) fwrite(data.frame(IP()),"www/ip.csv",append = T,sep = "|") else
      fwrite(data.frame(IP()),"www/ip.csv")
  })
  
  observeEvent(input$exit1,stopApp(1))
  observeEvent(input$exit2,stopApp(2))
}

shinyApp(ui,server)