# Load Packages ----
library(shinydashboard)
library(shiny)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(ggplot2)
library(DT)

# Load additional dependencies and setup functions ----
# source("global.R")

# Define the UI ----
ui <- list(
  dashboardPage(
    skin = "yellow",
    ## Header ----
    dashboardHeader(
      titleWidth = 250,
      title = "Effect of Outliers",
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Effect_of_Outliers")
      ),
      tags$li(
        class = "dropdown",
        tags$a(
          href = 'https://shinyapps.science.psu.edu/',
          icon("house")
        )
      )
    ),
    ## Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview",icon = icon("gauge-high")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        menuItem("References", tabName = "References", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ## Body ----
    dashboardBody(
      tabItems(
        ### Overview Page ----
        tabItem(
          tabName = "overview",
          h1("Effect of Outliers"),
          p("In this app, you will observe the effects of an outlier
             on histograms, box plots, and summary statistics."),
          br(),
          h2("Instructions"),
          tags$ol(
            tags$li("Press the 'Prerequisites' button to review concepts on the 
                    Prerequisites page."),
            tags$li("Once you have properly reviewed the prerequisites, head to 
                    the Explore page to see the concepts in action."),
            tags$li("Specify the values for the sample size, ", tags$em("n"),
                    ", as well as the the population mean and standard deviation 
                    using the three input sliders."),
            tags$li("Change the value of diamond by moving the designated slider 
                    (or pressing the associated play button to animate the slider)."),
            tags$li("Watch how the diamond becomes a potential outlier
                    and how its value affects a box plot, a histogram, and the 
                    values of summary statistics.")
          ),
          #### Prerequisites Button 
          div(
            style = "text-align:center;",
            bsButton(
              inputId = "goToPrereq",
              label = "Prerequisites",
              icon = icon("book"),
              size = "large"
            )
          ),
          #### Acknowledgements
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This app was originally developed and coded by Caihui Xiao and 
            Sitong Liu in June 2017. The app was further updated by Zhiliang Zhang
            and Jiajun Gao in June 2018, Ruisi Wang in June 2019, Daehoon Gwak in
            July 2020, and Sean Burke in June 2023.",
            br(),
            br(),
            br(),
            br(),
            "Cite this app as:",
            br(),
            citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 6/15/2023 by SB.")
          )
        ),
        ### Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          p("Here are some concepts you may want to review before heading to the 
            Explore Page."),
          br(),
          h3("Plots"),
          br(),
          fluidRow(
            box(
              width = 6,
              title = tags$strong("Box Plots"),
              collapsible = TRUE,
              collapsed = TRUE,
              p("There are several kinds of box plots that we encounter, including
                standard box plots and modified/outlier box plots. All box plots
                reveal information about the data collection but do not display
                [all of] the individual cases that make up the collection. They
                are generally composed of a central box and two whiskers on either
                side, which is why they are sometimes referred to as box-and-whisker
                plots."),
              p("A standard box plot highlights the values of the sample minimum
                (Min), the first quartile (Q1), the sample median, the third 
                quartile (Q3), and the sample maximum (Max). (Refer to the
                'Five Number Summary' section for more information on these 
                statistics.) We can also see other aspects such as the spread of
                the data (via the sample range and inter-quartile range (IQR)) as
                well as some aspects of case density."),
              br(),
              tags$figure(
                align = "center",
                tags$img(
                  src = "stanBoxPlotEx.jpeg",
                  width = "100%",
                  alt = "This is an example of a standard box plot that highlights 
                  the five number summary."
                )
              ),
              p("A modified or outlier box plot is a variation of the box plot
                where we impose a rule to flag cases as being potential outliers.
                These potential outliers will appear as dots in our plot. Instead
                of the whiskers extending to the values of the sample minimum and
                maximum, the whiskers will extend to 'hinge' points based upon 
                our chosen flag rule. The flag rule is expressed as some
                multiple (typically, 1.5) of the IQR above the value of the third
                quartile \\((Q3 + 1.5*IQR)\\) and below the value of the first quartile \\((Q1 - 1.5*IQR)\\). If the hinge is less than the sample minimum, the whisker will only 
                extend to the value of the sample minimum. The same is true for 
                the upper hinge relative to the sample maximum."),
              br(),
              tags$figure(
                align = "center",
                tags$img(
                  src = "modBoxPlotEx.jpeg",
                  width = "100%",
                  alt = "This is an example of a modified of outlier box plot that 
                  highlights the median, quartiles, hinges, and potential outliers."
                )
              )
            ),
            box(
              width = 6,
              title = tags$strong("Histogram"),
              collapsible = TRUE,
              collapsed = TRUE,
              p("This plot displays the frequency, relative frequency, or density 
                of data using bars that are typically adjacent to one another and 
                represent intervals or ranges of values (in this app, we'll display 
                frequency histograms)."),
              tags$figure(
                align = "center",
                tags$img(
                  src = "hisEx.jpeg",
                  width = "100%", #add percentage
                  alt = "This is an example histogram that displays a slightly 
                  left-skewed distribution of the data and locates the median as 
                  a line that lies slightly right from the center of the plot."
                )
              )
            )
          ),
          br(),
          h3("Summary Statistics for a Sample"),
          br(),
          fluidRow(
            box(
              width = 6,
              title = tags$strong("Five Number Summary"),
              collapsible = TRUE,
              collapsed = TRUE,
              tags$ol(
                tags$li(tags$strong("Minimum: "), "This is the smallest observed
                        value in the data collection."),
                tags$li(tags$strong("Lower Quartile (Q1): "), "This is the number 
                        in the 25th percentile, in which at least 25% of the data
                        has values less than this number."),
                tags$li(tags$strong("Median: "), "This is the number in the 50th 
                      percentile, where at least 50% of the data has values less 
                      than or equal to this number, and at least 50% has values 
                      greater than or equal to this number."),
                tags$li(tags$strong("Upper Quartile (Q3): "), "This is the number 
                      in the 75th percentile, in which at least 75% of the data 
                      has values less than this number."),
                tags$li(tags$strong("Maximum: "), "This is the largest observed
                        value in the data collection."),
              )
            ),
            box(
              width = 6,
              title = tags$strong("Descriptive Statistics"),
              collapsible = TRUE,
              collapsed = TRUE,
              tags$strong("Mean:"),
              tags$ul(
                tags$li("This number represents the arithmetic mean of all the values 
              within the dataset."),
              tags$li("\\(\\bar{x} = \\sum_{i=1}^{n} \\frac{x_i}{n}\\)") 
              ),
              br(),
              tags$strong("Standard Deviation:"),
              tags$ul(
                tags$li("The standard deviation is a measure of the dispersion or spread 
              of the data relative to the mean."),
              tags$li("\\(\\sigma = \\sqrt{\\frac{\\sum_{i=1}^{n} (x_i - \\bar{x})^2}{n}}\\)")
              ),
              br(),
              tags$strong("Interquartile Range (IQR):"),
              tags$ul(
                tags$li("This number is the range of the middle half of values 
                        from the lower quartile to the upper quartile."),
                tags$li("\\(\\text{IQR} = Q_3 - Q_1\\)")
              )
            )
          ),
          br(),
          div(
            style = "text-align: center",
            bsButton(
              inputId = "goToExplore", 
              label = "Explore", 
              icon = icon("bolt"),
              size = "large", 
              class = "circle grow"
            )
          )
        ),
        #### Explore Page ----
        tabItem(
          tabName = "explore",
          h2('Explore the Effects of an Outlier'),
          p("Watch closely as the diamond becomes a potential outlier when turned 
            red. Pay close attention how the change in the value of the diamond 
            affects the histogram, outlier box plot, and summary statistics. Which 
            values of the summary statistics change as the value of the diamond 
            changes? Which values stay the same?"),
          ##### Slider Inputs Panel -----
          wellPanel(
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
            fluidRow(
              column(
                width = 6,
                offset = 3,
                sliderInput(
                  inputId = "outlier",
                  label = "Move the Diamond",
                  min = -50,
                  max = 50,
                  value = 0,
                  animate = animationOptions(interval = 1700, loop = FALSE)
                )
              )
            )
          ),
          br(),
          uiOutput("sizeWarning", class = "redtext"),
          ##### Plot Outputs ----
          div(
            style = "margin: auto;",
            plotOutput(outputId = "boxPlot", height = "175px"),
            plotOutput(outputId = "histplot", height = "300px")
          ),
          br(),
          ##### Data Table Outputs----
          h3("Summary Statistics for the Sample", align = 'center'),
          DT::DTOutput(outputId = "descStat",width = "50%"), # mean, sd
          DT::DTOutput(outputId = "fiveNumSum") #five numbers
        ),
        #### References ----
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
server <- function(session, input, output) {
  
  ## Prereq Button ----
  observeEvent(
    eventExpr = input$goToPrereq,
    handlerExpr = {
      updateTabItems(
        session = session, 
        inputId = "pages",
        selected = "prerequisites"
      )
    }
  )
  ## Explore Button ----
  observeEvent(
    eventExpr = input$goToExplore,
    handlerExpr = {
      updateTabItems(
        session = session, 
        inputId = "pages",
        selected = "explore"
      )
    }
  )
  
  ## Info Button  ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        title = "Information",
        text = "This application will allow you to visually explore the effect 
        of an outlier in different ways.",
        type = "info"
      )
    }
  )
  
  # initialize dataset
  dataSet <- reactiveVal()
  
  ## Alt argument function ----
  altArg <- function(plotType) {
    paste0(
      "This ",
      plotType,
      " interacting with the slider input currently displays a",
      if (round(mean(dataSet()), digits = 1) > round(median(dataSet()), digits = 1)) {
          " right-skewed distribution of the data."
      } else if (round(mean(dataSet()), digits = 1) < round(median(dataSet()), digits = 1))  {
        " left-skewed distribution of the data."
      } else if (round(mean(dataSet()), digits = 1) == round(median(dataSet()), digits = 1))  {
        " symmetric distribution of the data."
      }
    )
  }
  
  ## Diamond color function ----
  
  changeColor <- function() {
    #if diamond's value is greater than the upper hinge, turn red 
    if (input$outlier > (
      (round(quantile(dataSet(), 0.75), digits = 1)) + 
      (1.5 * ((round(quantile(dataSet(), 0.75), digits = 1)) -
             (round(quantile(dataSet(), 0.25), digits = 1))))
    ) |
    #if diamond's value is less than the lower hinge, turn red 
    input$outlier < (
      (round(quantile(dataSet(), 0.25), digits = 1)) - 
      (1.5 * ((round(quantile(dataSet(), 0.75), digits = 1)) -
             (round(quantile(dataSet(), 0.25), digits = 1)))
    ))) {
      boastUtils::psuPalette[2]
    } else {
      boastUtils::boastPalette[5]
    }
  }
  
  
  ## Slider Inputs ----
  observeEvent(
    eventExpr = {input$sampleSize | input$mean | input$sd},
    handlerExpr = {
      if (input$sampleSize >= 2) {
        dataSet(
          c(
            input$outlier,
            round(
              rnorm(
                n = input$sampleSize - 1,
                mean = input$mean,
                sd = input$sd
              ),
              digits = 2
            )
          )
        )
        output$sizeWarning <- renderUI(NULL)
      } else {
        output$sizeWarning <- renderUI(
          "Please set sample size to at least 2; the plots will not update until 
          you do."
        )
        sendSweetAlert(
          session = session,
          title = "Warning",
          text = "Please set sample size to at least 2; the plots will not update 
          until you do!",
        type = "warning"
        )
      }
    }
  )
  
  # You will need to first add whichever palette line from above to your code
  # combine observeEvent since they all react to the sliderInput
  
  ## Plots and Data Tables----
  observeEvent(
    eventExpr = input$outlier, 
    handlerExpr = {
      dataSet(c(input$outlier, dataSet()[-1]))
      
      ### Render Box Plot ----
      output$boxPlot <- renderPlot(
        expr = {
          ggplot(
            data = data.frame(data0 = dataSet()),
            mapping = aes(y = 0, x = data0)
          ) +
            geom_boxplot(
              width = 0.3,
              col = boastUtils::boastPalette[5],
              fill = boastUtils::boastPalette[6],
              outlier.size = 5.5
            ) +
            geom_vline(
              mapping = aes(xintercept = mean(data0), color = "mean"), 
              linewidth = 1
            ) +
            geom_vline(
              mapping = aes(xintercept = median(data0), color = "median"), 
              linewidth = 1
            ) +
            labs(
              title = "Box Plot",
              y = NULL, 
              x = 'Value'
            ) +
            geom_point(
              mapping = aes(y = 0, x = data0[1]),
              shape = "diamond",
              color = changeColor(),
              size = 9
            ) + 
            theme(
              plot.title = element_text(hjust = 0.5),  # move title to center
              panel.background = element_blank(),  # remove background
              axis.line = element_line(colour = boastUtils::boastPalette[5]),  # make axis line black
              plot.caption = element_text(size = 18),  # change the text size
              text = element_text(size = 18), # change the text size
              axis.text.x = element_text(size = 18),
              legend.position = "bottom",
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank()
            ) +
            scale_color_manual(
              name = "statistics",
              values = c(
                mean = boastUtils::psuPalette[2], 
                median = boastUtils::psuPalette[1]
              )
            )
        },
        alt = altArg("boxplot")
      )
      
      ## Render Histogram ----
      output$histplot <- renderPlot(
        expr = {
          ggplot(
            data = data.frame(x = dataSet()), 
            mapping = aes(x = x)
          ) +
            geom_histogram(
              binwidth = 1, 
              boundary = 0, 
              col = boastUtils::boastPalette[5],
              fill = boastUtils::boastPalette[6]
            ) +
            labs(
              title = "Histogram",
              x = 'Value',
              y = 'Frequency'
            ) +
            # mean value line
            geom_vline(
              mapping = aes(xintercept = mean(x), color = "mean"),
              linewidth = 1
            ) +
            # median value line
            geom_vline(
              mapping = aes(xintercept = median(x), color = "median"), 
              linewidth = 1
            ) +
            # outliers
            geom_point(
              mapping = aes(x = x[1], y = 0.25), 
              shape = "diamond",
              color = changeColor(),
              size = 9
            ) +
            # legend
            scale_color_manual(
              name = "statistics",
              values = c(
                mean = boastUtils::psuPalette[2], 
                median = boastUtils::psuPalette[1]
              )
            ) +
            theme(
              plot.title = element_text(hjust = 0.5), # move title to center
              panel.background = element_blank(), # remove background
              axis.line = element_line(colour = boastUtils::boastPalette[5]), # make axis line black
              plot.caption = element_text(size = 18), # change the text size
              text = element_text(size = 18), # change the text size
              axis.text = element_text(size = 18),
              legend.position = "none"
            ) +
            scale_y_continuous(expand = expansion(mult = 0, add = c(0, 1)))
        },
        alt = altArg("histogram")
      )
      ### Mean, SD, and IQR Data Table----
      output$descStat <- DT::renderDT(
        expr = {
          df1 <- data.frame(
            Mean = round(mean(dataSet()), digits = 1),
            SD = round(sd(dataSet()), digits = 1),
            IQR = (round(quantile(dataSet(), 0.75), digits = 1) - round(quantile(dataSet(), 0.25), digits = 1))
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
            list(className = "dt-center", targets = "_all")
          )
        )
      )
      
      ### 5 Number Summary Data Table ----
      output$fiveNumSum <- DT::renderDT(
        expr = {
          df2 <- data.frame(
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
            list(className = "dt-center", targets = "_all")
          )
        )
      )
    }
  )
}



# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
