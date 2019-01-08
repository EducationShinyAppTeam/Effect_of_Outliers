library(shinydashboard)
library(shiny)
library(shinyBS)
library(shinyjs)
ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Effect of Outliers"),
                    dashboardSidebar(
                      sidebarMenu(
                        id="tabs",
                      #  menuItem("Prerequisite", tabName = "prerequisite3"),
                        menuItem("Overview", tabName = "overview3",icon = icon("dashboard")),
                        menuItem("Explore", tabName = "fivenumber", icon = icon("wpexplorer"))
                      )
                    ),
                    dashboardBody(
                      tags$head(
                        tags$style(HTML(
                          '.popover-title{
                            color: black;
                            background-color: Orange
                          }'
                          
                        )),
                        
                        #Change the location of progress bar
                        tags$style(
                          HTML(".shiny-notification {
                          height: 100px;
                          width: 800px;
                          position:fixed;
                          top: calc(50% - 50px);;
                          left: calc(50% - 400px);;
                          }
                          "
                          )
                        )
                        ),
                      
                      tabItems(
                        # First tab content
                        tabItem(tabName = "overview3",
                                #' fluidRow(
                                  # the color for slider bar
                                  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: orange}")),
                                  tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: orange}")),
                                  tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: orange}")),
                                  tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: orange}")),
                                
                               
                                tags$a(href='http://stat.psu.edu/',   tags$img(src='logo.png', align = "left", width = 180)),
                                br(),
                                br(),
                                br(),
                                h3(strong("About:")),
                                h4("In this App, you will observe the effect of outliers on both histograms and boxplots."),
                                br(),
                                h3(strong("Instructions:")),
                                h4(tags$li("Specify  population mean, Standard Deviation and the sample size n.")),
                                h4(tags$li("Click GO! to generate a dataset.")),
                                h4(tags$li("Change the value of the movable point to see how it affects the Histogram & Boxplot.")),
                                
                                div(style = "text-align:center",
                                    bsButton("go", "G O !", icon("bolt"), size = "large",style = "warning")),
                                br(),
                                h3(strong("Acknowledgements:")),
                                h4("This app was developed and coded by Caihui Xiao and Sitong Liu then further updated by Zhiliang Zhang and Jiajun Gao.")
                                
                                
                        ),
                        
                        # Second tab content
                        tabItem(tabName = "fivenumber",fluidPage(
                          div(style="display: inline-block;vertical-align:top;",
                              tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 15))
                          ),
                          wellPanel(
                      
                            fluidRow(
                              column(4,
                                     
                                     sliderInput("num", 
                                                 "Sample Size:", 
                                                 value = 30,
                                                 max = 100,
                                                 min = 1)),
                              
                              column(4, sliderInput("mean", 
                                                    "Population Mean:", 
                                                    min = -10,
                                                    max = 10, 
                                                    value = 2)),
                              column(4,
                                     sliderInput("sd", 
                                                 "Population Standard Deviation:", 
                                                 min = 0,
                                                 max = 10, 
                                                 value = 2))),
                            fluidRow(
                              column(6,
                                     
                                     #changed from numeric input to a slider bar 

                                     sliderInput("X1", "Move the black dot ", min = -50, 

                                                 max = 50, value = 0),
                                     bsPopover("X1"," ","Drag the slide bar to change the value of black dot on the plot.",trigger = "click",place = "bottom",options = list(container = "body"))
                              )
                            ),
                            fluidRow(
                            div(style = "text-align:center",
                                bsButton("goButton2", "Create",size = "large",icon("retweet"), style = "warning"),
                                bsPopover("goButton2", " ","Click to generate sample statistics summary and create your own plot", trigger = "hover", placement = "right", options = list(container = "body"))
                                
                            )
                            
                          )),
  
                          
                          hr(),
                          wellPanel( 
                            div(style = "text-align: center" , h3("Sample Statistics")),
                            fluidRow(column(2,offset = 1,
                                            
                                            h3(textOutput("hhh"))),
                                     column(2,
                                            
                                            h3(textOutput("hhh1")))),
                            fluidRow( 
                              column(2,offset=1,
                                     h3(textOutput("hhh2"))),
                              column(2,
                                     h3(textOutput("hhh3"))),
                              column(2,
                                     h3(textOutput("hhh4"))),
                              column(2,
                                     h3(textOutput("hhh5"))),
                              column(2,
                                     h3(textOutput("hhh6"))))),
                          
                          
                          
                          fluidRow(
                            column(6,
                                   br(),
                                   plotOutput("distPlot2")),
                            column(6,
                                   br(),
                                   plotOutput("hisPlot2"),
                                   bsPopover("hisPlot2", "Line info","Orange line for median and yellow line for mean.", trigger = "hover", placement = "top", options = list(container = "body")))
                          )
                          
                      
                        
                        )
                      )
                    )
))