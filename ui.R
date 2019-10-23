library(shinydashboard)
library(shiny)
library(shinyBS)
library(shinyjs)
library(formattable)
sliderInput2 <- function(inputId, label, min, max, value, step=NULL, from_min, from_max){
  x <- sliderInput(inputId, label, min, max, value, step)
  x$children[[2]]$attribs <- c(x$children[[2]]$attribs, 
             "data-from-min" = from_min,
             "data-from-max" = from_max,
             "data-from-shadow" = TRUE)
  x
}
ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Effect of Outlier",
                                    tags$li(class="dropdown",
                                            actionLink("info", icon("info"), class="myClass")),
                                    tags$li(class='dropdown',
                                            tags$a(href="https://shinyapps.science.psu.edu/",
                                                   icon('home', lib='font-awesome')))),
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
                          background-color: orange
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
                                tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css"),
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
                                #h4(tags$li("Click GO! to generate a dataset.")),
                                h4(tags$li("Change the value of the movable point to see how it affects the Histogram & Boxplot.")),
                                
                                div(style = "text-align:center",
                                    bsButton("go", "G O !", icon("bolt"), size = "large", style = "warning", class="circle grow")),
                                br(),
                                h3(strong("Acknowledgements:")),
                                h4("This app was developed and coded by Caihui Xiao and Sitong Liu then further updated by Zhiliang Zhang, Jiajun Gao and Ruisi Wang.")
                                
                                
                                ),
                        
                        # Second tab content
                        tabItem(tabName = "fivenumber",fluidPage(
                        
                          wellPanel(
                            
                            fluidRow(
                              column(4,
                                     sliderInput2("num", "Sample Size:",
                                                  min = 0, 
                                                  max = 100, 
                                                  value = 50, 
                                                  step = 1, 
                                                  from_min = 2, 
                                                  from_max = 100
                                     )),
                              
                              column(4,
                                     sliderInput("mean",
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
                                     
                                     sliderInput("X1", "Move the outlier (black dot) ",
                                                 min = -50, 
                                                 max = 50, 
                                                 value = 0),
                                     bsPopover("X1"," ","Drag the slide bar to change the value of black dot on the plot.",
                                               trigger = "hover", 
                                               place = "bottom", 
                                               options = list(container = "body"))
                              )
                            )
                            
                            
                          ),
                          
                           
                          wellPanel( 
                            # div(style = "text-align: center", 
                            #     h3(strong("Sample Statistics "))),
                            # h3(tableOutput("values")),
                            # h3(tableOutput("values2"))),
                          
                          
                          fluidRow(
                            column(6,
                                   #br(),
                                   plotOutput("distPlot2")),
                            column(6,
                                   #br(),
                                   plotOutput("hisPlot2"),
                                   bsPopover("hisPlot2", "Line info", 
                                             "Blue line for median and red line for mean.", 
                                             trigger = "hover", placement = "top"))
                            
                          ),
                          div(style = "text-align: center", 
                              h3(strong("Sample Statistics "))),
                          h3(tableOutput("values")),
                          h3(tableOutput("values2")))
                          
                          
                        )
                        )
                        )
                      ))
