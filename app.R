# Library Calls----
library(shinydashboard)
library(shiny)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(ggplot2)
library(DT)

## App Meta Data----------------------------------------------------------------
APP_TITLE <<- "Effect of Outliers"
APP_DESCP  <<- paste(
  "This app provides an opportunity to examine the impact of an outlier on",
  "the values of various descriptive statistics, a boxplot, and a histogram."
)
## End App Meta Data------------------------------------------------------------

# Define global constants and functions ----

# Define the UI ----
ui <- list(
  dashboardPage(
    skin = "yellow",
    dashboardHeader(
      titleWidth = 250,
      title = "Effects of an Outliers",
      tags$li(class="dropdown",
              actionLink("info", icon("info"), class="myClass")),
      tags$li(
        class = "dropdown",
        tags$a(target = "_blank", icon("comments"),
          href = "https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=Effects_of_Outliers"
        )
      ),
      tags$li(class='dropdown',
              tags$a(href="https://shinyapps.science.psu.edu/",
                     icon('home', lib='font-awesome')))),
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id="tabs",
        menuItem("Overview", tabName = "overview",icon = icon("tachometer-alt")),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        menuItem("References", tabName = "References", icon = icon("leanpub"))
      ),
      tags$div(class = "sidebar-logo",
               boastUtils::psu_eberly_logo("reversed"))
    ),
    dashboardBody(
      tabItems(
        # First tab content ----
        tabItem(
          tabName = "overview",
          #Title
          h1("Effects of Outliers"),
          p("In this app, you will observe the effects of an outlier
             on histograms, boxplots, and summary statistics."),
          br(),
          h2("Instructions"),
          tags$ol(
            tags$li("Specify the values for the sample size, ", tags$em("n"),
                    "as well as the the population mean and standard deviation."),
            tags$li("Change the value of a desginated outlier by moving the
                    outlier slider (or pressing the associated play button to
                    animate the slider) to see how the potential outlier's value
                    affects a boxplot, a histogram, and the values of summary
                    statistics.")
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
            div(class = "updated", "Last Update: 9/21/2020 by NJH.")
          )
        ),
        # Second tab content ----
        tabItem(
          tabName = "explore",
          h2('Explore the Effects of an Outlier'),
          fluidRow(
            column(
              width = 4,
              sliderInput(
                inputId = "sampleSize",
                label = "Sample Size",
                min = 0,
                max = 100,
                value = 50,
                step = 1
              )
            ),
            column(
              width = 4,
              sliderInput(
                inputId = "mean",
                label = "Population Mean",
                min = -10,
                max = 10,
                value = 0
              )
            ),
            column(
              width = 4,
              sliderInput(
                inputId = "sd",
                label = "Population Standard Deviation",
                min = 0,
                max = 10,
                value = 2
              )
            )
          ),
          sliderInput(
            inputId = "outlier",
            label = "Move the outlier (large black dot)",
            min = -50,
            max = 50,
            value = 0,
            animate = animationOptions(interval = 1000, loop = FALSE)
          ),
          uiOutput("sizeWarning", class = "redtext"),
          div(
            style = "margin: auto;",
            plotOutput(outputId = "boxPlot", height = "175px"),
            plotOutput(outputId = "histplot", height = "300px")
          ),
          tags$script(HTML(
            "$(document).ready(function()
                       { document.getElementById('boxPlot').
                       setAttribute('aria-label',
                       `Shows Boxplot interacting with the sliderInput`)
                       })"
          )),
          tags$script(HTML(
            "$(document).ready(function()
                       { document.getElementById('histplot').
                       setAttribute('aria-label',
                       `Shows Histogram interacting with the sliderInput`)
                       })"
          )),
          br(),
          h3("Summary Statistics for the Sample", align = 'center'),
          DT::DTOutput(outputId = "values") # mean, sd, and five numbers
        ),
        ## References ----
        tabItem(
          tabName = "References",
          h2("References"),
          p(     #shinyBS
            class = "hangingindent",
            "Bailey, E. (2015), shinyBS: Twitter bootstrap components for shiny,
            R package. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(     #Boast Utilities
            class = "hangingindent",
            "Carey, R. (2019), boastUtils: BOAST Utilities, R Package.
            Available from
            https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(     #shinydashboard
            class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2018), shinydashboard: Create
            dashboards with 'Shiny', R Package. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(     #shiny
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J.
            (2019), shiny: Web application framework for R, R Package.
            Available from https://CRAN.R-project.org/package=shiny"
          ),
          p(     #shinyWidgets
            class = "hangingindent",
            "Perrier, V., Meyer, F., Granjon, D., Fellows, I., and Davis, W.
            (2020), shinyWidgets: Custom Inputs Widgets for Shiny, R package.
            Available from
            https://cran.r-project.org/web/packages/shinyWidgets/index.html"
          ),
          p(     #reference for ideas
            class = "hangingindent",
            "Statistical Applets - Mean and Median (n.d.), Available from
          http://digitalfirst.bfwpub.com/stats_applet/generic_stats_applet_6_meanmed.html"
          ),
          p( # ggplot2
            class = "hangingindent",
            "Wickham, H., Chang, W., Henry, L., Pedersen, T.L., Takahashi, K.,
            Wilke, C, Woo, K., Yutani, H., and Dunnington, D. (2020),
            ggplot2: Create Elegant Data Visualisations Using the
            Grammar of Graphics, R Package. Available from
            https://cran.r-project.org/web/packages/ggplot2/index.html"
          ),
          p( # DT
            class = "hangingindent",
            "Xie, Y., Cheng, J., Tan, X., Allaire, J., Girlich, M., Ellis, G.F.,
            and Rauh, J. (2020), DT: A Wrapper of the JavaScript Library
            'DataTables', R Package. Available from
            https://cran.r-project.org/web/packages/DT/index.html"
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Define the server ----
server <- function(session, input,output){
  # navigate to explore page
  observeEvent(input$go,{
    updateTabItems(session, "tabs", "explore")
  })
  # info button on the top right corner
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions",
      text = "Specify the values for the sample size as well as the population
      mean and standard deviation. Move the outlier slider (or press the play
      button to animate it) to change the value of a potential outlier.",
      type = "info"
    )
  })

  dataSet <- reactiveVal()

  observeEvent({input$sampleSize | input$mean | input$sd}, {
    if (input$sampleSize >= 2) {
      dataSet(c(input$outlier,
                round(rnorm(n = input$sampleSize - 1,
                            mean = input$mean,
                            sd = input$sd),
                      digits = 2))
      )
    } else {
      output$sizeWarning <- renderUI(
        "Please set sample size to at least 2; plots will not update until you do."
      )
    }
  })

  # You will need to first add whichever palette line from above to your code
  # combine observeEvent since they all react to the sliderInput
  observeEvent(input$outlier, {
    dataSet(c(input$outlier, dataSet()[-1]))
    # Boxplot
    output$boxPlot <- renderPlot({
      ggplot(data = data.frame(data0 = dataSet()),
             mapping = aes(y = 0, x = data0)) +
        geom_boxplot(width = 0.3, col = "black",
                     fill = boastUtils::boastPalette[6]) +
        geom_vline(aes(xintercept = mean(data0), color = "mean"), size = 1) +
        geom_vline(aes(xintercept = median(data0), color = "median"), size = 1) +
        labs(title = "Boxplot", y = NULL, x = 'Value') +
        geom_point(mapping = aes(y = 0, x = data0[1]), size = 4) +
        theme(
          plot.title = element_text(hjust = 0.5),  # move title to center
          panel.background = element_blank(),  # remove background
          axis.line = element_line(colour = "black"),  # make axis line black
          plot.caption = element_text(size = 18),  # change the text size
          text = element_text(size = 18), # change the text size
          axis.text.x = element_text(size = 18),
          legend.position = "bottom",
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
        ) +
        scale_color_manual(name = "statistics",
                           values = c(mean = "red", median = "blue"))
    })
    # Histogram
    output$histplot <- renderPlot({
      ggplot(data = data.frame(x = dataSet()), mapping = aes(x = x)) +
        geom_histogram(binwidth = 1, boundary = 0, col = "black",
                       fill = boastUtils::boastPalette[6]) +
        labs(title = "Histogram", x = 'Value', y = 'Frequency') +
        # mean value line
        geom_vline(aes(xintercept = mean(x), color = "mean"), size = 1) +
        # median value line
        geom_vline(aes(xintercept = median(x), color = "median"), size = 1) +
        # outliers
        geom_point(mapping = aes(x = x[1], y = 0.25), size = 4) +
        # legend
        scale_color_manual(name = "statistics",
                           values = c(mean = "red", median = "blue")) +
        theme(
          plot.title = element_text(hjust = 0.5), # move title to center
          panel.background = element_blank(), # remove background
          axis.line = element_line(colour = "black"), # make axis line black
          plot.caption = element_text(size = 18), # change the text size
          text = element_text(size = 18), # change the text size
          axis.text = element_text(size = 18),
          legend.position = "none"
        ) +
        scale_y_continuous(expand = expansion(mult = 0, add = c(0, 1)))
    })
    # build dataframe for the values - mean, sd, and five numbers
    output$values <- DT::renderDT({
      df <- data.frame(
        Mean = round(mean(dataSet()), digits = 1),
        SD = round(sd(dataSet()), digits = 1),
        Min = round(min(dataSet()), digits = 1),
        Q1 = round(quantile(dataSet(), 0.25), digits = 1),
        Median = round(median(dataSet()), digits = 1),
        Q3 = round(quantile(dataSet(), 0.75), digits = 1),
        Max = round(max(dataSet()), digits = 1)
      )
    },
    style = "bootstrap4",  # You must use this style
    rownames = FALSE,
    options = list(
      responsive = TRUE,
      scrollX = TRUE,
      paging = FALSE,  # Set to False for small tables
      searching = FALSE,  # Set to False to turn of the search bar
      ordering = FALSE,
      info = FALSE,
      columnDefs = list(
        list(className = "dt-center", targets = -1:6)
      )
    )
    )
  })
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
