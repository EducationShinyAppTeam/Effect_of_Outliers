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
  # combine observeEvent since they all react to the sliderInput
  observeEvent({input$sampleSize | input$mean | input$sd},
  {
    # rnorm: number of size = n, mean = mean, sd = sd
    dataset <- round(rnorm(n = input$sampleSize-1, mean = input$mean,
                          sd=input$sd), digits = 2)
    # Boxplot     
    output$boxPlot <- renderPlot({
      # dataset2 is the combination of the outlier from sliderInput & 'dataset'
      dataset2 <- c(input$outlier, dataset)
      boxplot(dataset2, xlab = 'x', ylab = 'value', font.lab = 2,
              col = "#ffdaad", border = '#1c4063', main = "Boxplot")
      points.default(input$outlier, pch = 16, cex = 2)
    })
    # Histogram 
    output$histplot <- renderPlot({
      dataset2 <- c(input$outlier, dataset)
      hist(dataset2, xlab = 'value', ylab = 'frenquency',
           col = 'lightblue', border = "#1c4063", main = "Histogram")
      points.default(input$outlier, 0, pch = 16, cex = 2)
      # mean value line
      abline(v = (round(mean(dataset2), digits = 1)),
             col = "red", lwd = 2.5)
      # median value line
      abline(v = (round(median(dataset2), digits = 1)),
             col = "#1c4063", lwd =2.5)
    })
    # build dataframe for the values - mean, sd
    sliderValues <- reactive({
      dataset2 <- c(input$outlier, dataset)
      data.frame(
        mean = (as.character(round(mean(dataset2), digits = 1))),
        sd = (as.character(round(sd(dataset2), digits = 1))),
        stringsAsFactors = FALSE)
    })
    output$values <- renderTable(
      {sliderValues()},
      width = '100%', align = 'c', 
      striped = TRUE, bordered = TRUE
    )
    # build dataframe for the values - five numbers
    sliderValues2 <- reactive({
      dataset2 <- c(input$outlier, dataset)
      data.frame(
        min = (as.character(round(min(dataset2), digits = 1))),
        Q1 = (as.character(round(quantile(dataset2,1/4), digits = 1))),
        median = (as.character(round(median(dataset2), digits = 1))),
        Q3 = (as.character(round(quantile(dataset2,3/4), digits = 1))),
        max = (as.character(round(max(dataset2), digits = 1))),
        stringsAsFactors = FALSE)
    })
    output$values2 <- renderTable(
      {sliderValues2()}, 
      width = "100%", align = 'c',
      striped = TRUE, bordered = TRUE
    )
  })
})

