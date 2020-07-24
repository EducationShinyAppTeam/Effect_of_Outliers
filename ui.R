library(shinydashboard)
library(shiny)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)

sliderInput2 <- function(inputId, label, min, max, value, step=NULL, from_min, from_max){
  x <- sliderInput(inputId, label, min, max, value, step)
  x$children[[2]]$attribs <- c(x$children[[2]]$attribs, 
             "data-from-min" = from_min,
             "data-from-max" = from_max,
             "data-from-shadow" = TRUE)
  x
}
# UI starts ---
shinyUI(
  dashboardPage(
    skin = "yellow",
    dashboardHeader(
      titleWidth = 250,
      title = "Effect of Outlier",
      tags$li(class="dropdown",
              actionLink("info", icon("info"), class="myClass")),
      tags$li(class='dropdown',
              tags$a(href="https://shinyapps.science.psu.edu/",
                     icon('home', lib='font-awesome')))),
    dashboardSidebar(
      width = 250,
      sidebarMenu(id="tabs",
        menuItem("Overview", tabName = "overview",icon = icon("dashboard")),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        menuItem("Reference", tabName = "References", icon = icon("leanpub"))
      ),
      tags$div(class = "sidebar-logo",
               boastUtils::psu_eberly_logo("reversed"))
    ),
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css",
                  href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
        ),
      tabItems(
        # First tab content
        tabItem(
          tabName = "overview",
          h1("Effects of Outliers"),
          #Title
          p("In this App, you will observe the effect of outliers
            on both histograms and boxplots."),
          br(),
          h2("Instructions"),
          tags$ol(
            tags$li("Specify  Population Mean, Standard Deviation,
                    and the Sample Size n."),
            tags$li("Change the value of the movable point 
                    to see how it affects the Histogram & Boxplot.")
          ),
          div(
            style = "text-align:center",
            bsButton(
              inputId = "go",
              label = "GO!",
              icon("bolt"),
              size = "large",
            )
          ),
          #Acknowledgements
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This app was originally developed and coded by Caihui Xiao
            and Sitong Liu in June 2017. The was further updated by 
            Zhiliang Zhang and Jiajun Gao in June 2018, 
            and Ruisi Wang in June 2019, and Daehoon Gwak in July 2020.",
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 7/24/2020 by DG.")
          )
        ),
        # Second tab content
        tabItem(
          tabName = "explore",
          h2('Explore the Effects of Outlier'),
          fluidRow(
            column(
              width = 4,
              sliderInput2(
                inputId = "sampleSize",
                label = "Sample Size:",
                min = 0,
                max = 100,
                value = 50,
                step = 1,
                from_min = 2,
                from_max = 100
              )
            ),
            column(
              width = 4,
              sliderInput(
                inputId = "mean",
                label = "Population Mean:",
                min = -10,
                max = 10,
                value = 0
              )
            ),
            
            column(
              width = 4,
              sliderInput(
                inputId = "sd",
                label = "Population Standard Deviation:",
                min = 0,
                max = 10,
                value = 2
              )
            )
          ),
          fluidRow(
            column(
              width = 6,
              sliderInput(
                inputId = "outlier",
                label = "Move the outlier (black dot)",
                min = -50,
                max = 50,
                value = 0,
                animate = animationOptions(interval = 200, loop = FALSE)
              )
            )
          ),
          fluidRow(
            column(
              width = 6,
              plotOutput(outputId = "boxPlot"),
              # Alt text
              tags$script(HTML(
                "$(document).ready(function() 
                       { document.getElementById('boxPlot').
                       setAttribute('aria-label',
                       `Shows Boxplot interacting with the sliderInput`)
                       })"
              ))
              ),
            column(
              width = 6,
              plotOutput(outputId = "histplot"),
              bsPopover(
                id = "histplot",
                title = "Line info",
                content = "Blue line for median and red line for mean.",
                trigger = "hover",
                placement = "top"
                ),
              # Alt text
              tags$script(HTML(
                "$(document).ready(function() 
                       { document.getElementById('histplot').
                       setAttribute('aria-label',
                       `Shows Histogram interacting with the sliderInput`)
                       })"
              ))
              )
            ), 
          p(strong("Sample Statistics "), align = 'center'),
          p(tableOutput(outputId = "values")), # mean and sd
          p(tableOutput(outputId = "values2")) # Five numbers
        ),
        tabItem(
          tabName = "References",
          h2("References"),
          p(     #shinyBS
            class = "hangingindent",
            "Bailey, E. (2015), shinyBS: Twitter bootstrap components for shiny.
            (v0.61), [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(     #Boast Utilities
            class = "hangingindent",
            "Carey, R. (2019), boastUtils: BOAST Utilities. (v0.1.0),
            [R Package]. Available from
            https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(     #shinydashboard
            class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2018), shinydashboard: Create
            dashboards with 'Shiny'. (v0.7.1), [R Package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(     #shiny
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J.
            (2019), shiny: Web application framework for R. (v1.4.0),
            [R Package]. Available from https://CRAN.R-project.org/package=shiny"
          ),
          p(     #shinyWidgets
            class = "hangingindent",
            "Perrier, V., Meyer, F., Granjon, D., Fellows, I., and Davis, W.
            (2020), shinyWidgets: Custom Inputs Widgets for Shiny
            (v0.5.2), [R package]. Available from
            https://cran.r-project.org/web/packages/shinyWidgets/index.html"
          ),
          p(     #reference for ideas
            class = "hangingindent",
            " Statistical Applets - Mean and Median (n.d.), Available from
          http://digitalfirst.bfwpub.com/stats_applet/generic_stats_applet_6_meanmed.html"
          )
        )
      )
    )
  )
)
