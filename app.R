# Load Packages ----
library(shinydashboard)
library(shiny)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(ggplot2)
library(DT)

# Load additional dependencies and setup functions ----
# None in this app

# Define the UI ----
ui <- list(
  dashboardPage(
    skin = "yellow",
    ## Header ----
    dashboardHeader(
      titleWidth = 250,
      title = "Effect of Outliers",
      tags$li(
        class = "dropdown",
        actionLink(
          inputId = "info",
          label = tags$span(class = "sr-only", "info"),
          icon("info")
        )
      ),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Effect_of_Outliers")
      ),
      tags$li(
        class = "dropdown",
        tags$a(
          href = 'https://shinyapps.science.psu.edu/',
          icon("house"), tags$span(class = "sr-only", "BOAST Site"),
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
          div(
            style = "text-align:center;",
            bsButton(
              inputId = "goToPrereq",
              label = "Prerequisites",
              icon = icon("book"),
              size = "large"
            )
          ),
          #### Acknowledgements ----
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
                standard box plots and modified/outlier box plots. They
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
              title = tags$strong("Histograms"),
              collapsible = TRUE,
              collapsed = TRUE,
              p("This plot displays the (absolute) frequency, relative frequency,
                or density of data using bars that are typically adjacent to one
                another. Each bar covers an interval (a set) of values marked by
                the bar's width. The height of each bar tells us how many observed
                values are in that interval, either as an direct count (absolute
                frequency), a proportion of the total (relative frequency), or as
                a density. In this app, we'll display (absolute) frequency
                histograms)."),
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
              p("[Tukey's] Five Number Summary consists of the values of five
                sample statistics. The underlying attribute needs to have sense 
                of ordering; that is, we can think of a case as having more or 
                less of that attribute. We generally present the Five Number 
                Summary from smallest value to largest value."),
              tags$ol(
                tags$li(tags$strong("Sample Minimum: "), "The value of this 
                        statistic provides a measure of the lower extremum. We 
                        get this value by looking for the smallest observed value
                        in the data collection."),
                tags$li(tags$strong("Lower Quartile (Q1): "), "This statistic 
                        measures the point at which we can break the ordered data
                        collection into two pieces--one piece containing the 
                        smallest 25% of the observed data and the other containing
                        the largest 75%. We find this value by looking for the 
                        value (does not need to be observed) that lets us make 
                        such a break. This statistic is also known as the First 
                        Quartile and 25th Percentile."),
                tags$li(tags$strong("Sample Median: "), "This statistic measures
                        the middle of an ordered data collection. That is to say,
                        we can break the ordered collection into two (nearly) 
                        equally-sized sub-collections. The value of this statistic
                        should be half-way through the ordered collection. Thus,
                        50% of the observed values should be smaller than it; the
                        other half should be at least as large as this value. The
                        Sample Median is also known as the Second Quartile and
                        50th Percentile."),
                tags$li(tags$strong("Upper Quartile (Q3): "), "This statistic 
                        measures the point at which we can break the ordered data
                        collection into two pieces--one piece containing the 
                        smallest 75% of the observed data and the other containing
                        the largest 25%. We find this value by looking for the 
                        value (does not need to be observed) that lets us make 
                        such a break. This statistic is also known as the Third 
                        Quartile and 75th Percentile."),
                tags$li(tags$strong("Sample Maximum: "), "The value of this 
                        statistic provides a measure of the upper extremum. We 
                        get this value by looking for the largest observed value
                        in the data collection."),
              )
            ),
            box(
              width = 6,
              title = tags$strong("Additional Descriptive Statistics"),
              collapsible = TRUE,
              collapsed = TRUE,
              tags$strong("Sample (Arithmetic) Mean"),
              tags$ul(
                tags$li("This statistic provides a measure of how well the data
                        collection performed at collecting values relative to the
                        size of the data collection."),
                tags$li("We can calculate this value by adding up all of the 
                        observed values (including any zeros) and then dividing
                        that total by how many cases are in the data collection.",
                        "\\[\\bar{x} = \\sum\\limits_{i=1}^{n} x_i \\bigg/ n\\]"
                )        
              ),
              br(),
              tags$strong("Sample (Arithmetic) Standard Deviation"),
              tags$ul(
                tags$li("This statistic measures how much pairs of cases differ
                        from each other in value, relative to the sample size. 
                        This statistic comes from the Sample (Arithmetic) Variance
                        and ajusts the unit of measurement by applying the square
                        root."),
                tags$li("We can calculate the value of this statistic with the
                        following formula:",
                        "\\[s =\\sqrt{\\frac{\\sum\\limits_{i=1}^{n}
                        \\left(x_i - \\bar{x}\\right)^2}{n-1}}\\]"
                )
              ),
              br(),
              tags$strong("Interquartile Range (IQR)"),
              tags$ul(
                tags$li("This statistic provides a measure of the spread for the
                        middle half of the ordered data collection."),
                tags$li("We can calculate the value of this statistic by finding
                        the difference between the values of the Upper and Lower
                        Quartiles",
                        "\\[\\text{IQR} = Q_3 - Q_1\\]"
                )
              )
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
          plotOutput(outputId = "boxPlot", height = "175px"),
          plotOutput(outputId = "histplot", height = "300px"),
          br(),
          ##### Data Table Outputs----
          h3("Summary Statistics for the Sample"),
          DT::DTOutput(outputId = "descStat",width = "50%"), # mean, sd
          DT::DTOutput(outputId = "fiveNumSum") #five numbers
        ),
        #### References ----
        tabItem(
          tabName = "References",
          h2("References"),
          p(     #shinyBS
            class = "hangingindent",
            "Bailey, E. (2022). shinyBS: Twitter bootstrap components for shiny.
            (v 0.61.1). [R package]. Available from 
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(     #Boast Utilities
            class = "hangingindent",
            "Carey, R. and Hatfield, N. (2023). boastUtils: BOAST Utilities. 
            (v 0.1.11.3). [R Package]. Available from
            https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(     #shinydashboard
            class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2021). shinydashboard: Create
            dashboards with 'Shiny'. (v 0.7.2). [R Package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(     #shiny
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Sievert, C., Schloerke, B.,
            Xie, Y., Allen, J., McPherson, J., Dipert, A., and Borges, B. (2023).
            shiny: Web application framework for R, R Package. (v 1.7.5). [R Package].
            Available from https://CRAN.R-project.org/package=shiny"
          ),
          p(     #shinyWidgets
            class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2023), shinyWidgets: Custom
            Inputs Widgets for Shiny. (v 0.7.6). [R package]. Available from
            https://cran.r-project.org/web/packages/shinyWidgets/index.html"
          ),
          p(     #reference for ideas
            class = "hangingindent",
            "Statistical Applets - Mean and Median (n.d.), Available from
          http://digitalfirst.bfwpub.com/stats_applet/generic_stats_applet_6_meanmed.html"
          ),
          p( # ggplot2
            class = "hangingindent",
            "Wickham, H. (2016). ggplot2: Elegant graphics for data analysis.
            (v3.4.3). [R Package]. New York:Springer-Verlag. Available from
            https://ggplot2.tidyverse.org"
          ),
          p( # DT
            class = "hangingindent",
            "Xie, Y., Cheng, J., and Tan, X. (2023). DT: A Wrapper of the 
            JavaScript Library 'DataTables'. (v 0.28). [R Package]. Available from
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
              panel.background = element_blank(),  # remove background
              plot.title = element_text(hjust = 0.5),  # move title to center
              plot.caption = element_text(size = 18),  # change the text size
              plot.margin = margin(l = 75, unit = "pt"),
              text = element_text(size = 18), # change the text size
              axis.text.x = element_text(size = 18),
              axis.line.x = element_line(colour = "black"),  # make axis line black
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.line.y = element_blank(),
              legend.position = "bottom"
            ) +
            scale_color_manual(
              name = "Statistics",
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
              closed = "left",
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
              panel.background = element_blank(), # remove background
              plot.title = element_text(hjust = 0.5), # move title to center
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
          data.frame(
            Mean = round(mean(dataSet()), digits = 1),
            SD = round(sd(dataSet()), digits = 1),
            IQR = (round(quantile(dataSet(), 0.75), digits = 1) - round(quantile(dataSet(), 0.25), digits = 1))
          )
        },
        style = "bootstrap4", 
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
          data.frame(
            Min = round(min(dataSet()), digits = 1),
            Q1 = round(quantile(dataSet(), 0.25), digits = 1),
            Median = round(median(dataSet()), digits = 1),
            Q3 = round(quantile(dataSet(), 0.75), digits = 1),
            Max = round(max(dataSet()), digits = 1)
          )
        },
        style = "bootstrap4",
        caption = "Five Number Summary",
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
