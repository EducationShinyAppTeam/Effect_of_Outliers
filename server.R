library(boastUtils)
# Server starts ---
shinyServer(function(session, input,output){
  # navigate to explore page
  observeEvent(input$go,{
    updateTabItems(session, "tabs", "explore")
  })
  # info button on the top right corner
  observeEvent(input$info,{
    sendSweetAlert(
      session = session, 
      title = "Instructions:", 
      text = "Specify  Population Mean, Standard Deviation, 
      and the Sample Size n", 
      type = "info"
    )
  })
  # You will need to first add whichever palette line from above to your code
  boastPalette <- c("#0072B2","#D55E00","#009E73","#CE77A8",
                    "#000000","#E69F00","#999999","#56B4E9","#CC79A7")
  # combine observeEvent since they all react to the sliderInput
  observeEvent({input$sampleSize | input$mean | input$sd}, 
  {
    # rnorm: number of size = n, mean = mean, sd = sd
    dataset <- round(rnorm(n = input$sampleSize-1, mean = input$mean, 
                          sd = input$sd), digits = 2)
    # Boxplot     
    output$boxPlot <- renderPlot({
      # dataset2 is the combination of the outlier from sliderInput & 'dataset'
      dataset2 <- c(input$outlier, dataset)
      ggplot(data = data.frame(data0 = dataset2), 
             mapping = aes(x = 0, y = data0)) + 
        geom_boxplot(width = 0.3, col = "black", fill = "#E69F00") + 
        labs(title = "Boxplot", x = 'x', y = 'outlier value') +
        geom_point(mapping = aes(x = 0, y = data0[1]), size = 4) + 
        theme(
          plot.title = element_text(hjust = 0.5), # move title to center
          panel.background = element_blank(), # remove background
          axis.line = element_line(colour = "black"), # make axis line black
          plot.caption = element_text(size = 18), # change the text size 
          text = element_text(size = 18) # change the text size 
        )
    })
    # Histogram 
    output$histplot <- renderPlot({
      dataset2 <- c(input$outlier, dataset)
      ggplot(data = data.frame(x = dataset2), mapping = aes(x = x)) + 
        geom_histogram(binwidth = 1, boundary = 0, col = "black",
                       fill = "#E69F00") + 
        labs(title = "Histogram", x = 'outlier value', y = 'frenquency') +
        # mean value line
        geom_vline(aes(xintercept = mean(x), color = "mean"), size = 3) + 
        # median value line
        geom_vline(aes(xintercept = median(x), color = "median"), size = 3) +
        # outliers
        geom_point(mapping = aes(x = x[1], y = 0), size = 4) + 
        # legend
        scale_color_manual(name = "statistics", 
                           values = c(mean = "red", median = "blue")) +
        theme(
          plot.title = element_text(hjust = 0.5), # move title to center
          panel.background = element_blank(), # remove background
          axis.line = element_line(colour = "black"), # make axis line black
          plot.caption = element_text(size = 18), # change the text size 
          text = element_text(size = 18) # change the text size 
          #legend.position = "bottom" # change the legend position
        )
    })
    # build dataframe for the values - mean, sd, and five numbers
    output$values <- DT::renderDT({
      dataset2 <- c(input$outlier, dataset)
      df <- data.frame(
        Mean = (as.character(round(mean(dataset2), digits = 1))), 
        SD = (as.character(round(sd(dataset2), digits = 1))), 
        Min = (as.character(round(min(dataset2), digits = 1))), 
        Q1 = (as.character(round(quantile(dataset2,1/4), digits = 1))), 
        Median = (as.character(round(median(dataset2), digits = 1))), 
        Q3 = (as.character(round(quantile(dataset2,3/4), digits = 1))), 
        max = (as.character(round(max(dataset2), digits = 1))), 
        stringsAsFactors = FALSE)
    },
    caption = "Statistic Summary", # Add a caption to your table
    style = "bootstrap4", # You must use this style
    rownames = FALSE,
    options = list(
      responsive = TRUE,
      scrollX = TRUE,
      paging = FALSE, # Set to False for small tables
      searching = FALSE, # Set to False to turn of the search bar
      ordering = FALSE
    )
   )
  })
})

