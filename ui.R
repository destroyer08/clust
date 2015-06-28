library(shiny)

shinyUI(fluidPage(theme = "bootstrap.css",
                  tagList(
                    tags$head(
                      tags$link(rel="stylesheet", type="text/css",href="style.css"),
                      tags$script(type="text/javascript", src = "busy.js")
                    )
                  ),
                  
                  div(class = "busy",  
                      
                      img(src="ajaxloaderq.gif"),align="center"
                  ),
                  div(class = "busy2",  
                      h2("Give Us Data...By Uploading File"),
                      img(src="giphy.gif"),align="center"
                  ),
                  
                  tags$head(tags$style(type="text/css", 
                                       ".shiny-output-error { visibility: hidden; }",
                                       ".shiny-output-error:before { visibility: hidden; }"
                  )),
                  
                  
                  
                  
                  title = "Cluster Analysis",
                  h3("Cluster Analysis",align="center"),
                  
                  #fluidRow(column(3,h5("File/Study:"),verbatimTextOutput("code"))),h3("Cluster Analysis",align="center")),
                  hr(),
                  #  br(),
                  
                  fluidRow(class = "myRow1",
                           
                         
                           
                           column(3,fileInput('file1', 'Choose CSV File',
                                              accept=c('text/csv','text/comma-separated-values,text/plain', 
                                                       '.csv')),tags$a(href = 'sample.csv', 'Download sample file')),
                           #checkboxInput('header', 'Header', TRUE),
                           #uiOutput("slide1")
                           column(3,uiOutput("slide2")),
                           
                           
                           column(3,uiOutput("slide1")),
                           column(3,uiOutput("slide3"))
                          
                           
                           
                           
                           
                           #br(),
                           #fluidRow(column(5,uiOutput("button2")))
                           
                           
                           
                           
                           
                           
                           
                  ),
                  hr(),
                  fluidRow(class = "myRow2",
                           
                           
                           
                           column(2,uiOutput("choice1"),
                                  uiOutput("subdia"),uiOutput("pcatab"),uiOutput("var1"),
                                  uiOutput("var2")),column(8,offset=1,br(),
                                fluidRow(column(9,plotOutput("plot1")),
                                                                                                                                                              column(2,offset=0,uiOutput("button2")))
                                  )
                  ),
                  #column(2,uiOutput("button2")),
                  
                  fluidRow(class = "myRow3",column(2,uiOutput("button"))),
                  
                  fluidRow(class = "myRow4",
                           column(3),
                           column(6,offset=0.5,verbatimTextOutput("desc1"))
                           
                          
                  ),
                  fluidRow(class = "myRow4",
                           
                           tableOutput("cmean"),
                           tableOutput("stdscr"),
                           
                           #tableOutput("clust"),
                           tableOutput("stdev")
                           #plotOutput('plot1'),
                  ),
                  h6(tags$b("@ copyright 2015. Pratik Dekate. All rights reserved."),align="center")
                  
                  
                  
                  
                  
                  
                  #column(4,oggset=8,plotOutput('plot1')),
                  #column(3,selectInput('selection',h5('Figures'),
                  #c("Radar Plot","Cluster Plot","Comparision Plot","Principal Component Analysis")))
                  
                  
                  #fluidRow(column(4,selectInput('selection2',h5('Statistics'),
                  #                             c("Means","Standard Deviation","Standard Score"))))
                  
                  
                  
                  
                  
                  
))
