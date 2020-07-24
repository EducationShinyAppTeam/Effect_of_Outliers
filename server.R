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
  observeEvent({input$sampleSize | input$mean | input$sd},
  {
    round2<-round(rnorm(input$sampleSize-1, mean = input$mean, sd=input$sd), 
                  digits =2)
    #distribution plot     
    output$boxPlot <- renderPlot({
      table1 <- c(input$outlier, round2)
      boxplot(table1,  ylab = 'value', xlab = 'x',font.lab = 2,
              col = "#ffdaad", border = '#1c4063', main="Boxplot")
      points.default(input$outlier,pch = 16,cex = 2.2)
    })
    #histogram plot
    output$hisPlot <- renderPlot({
      table1 <- c(input$outlier, round2)
      hist(table1, ylab = 'frenquency', xlab = 'value', 
           col = 'lightblue', border = "#1c4063", main="Histogram")
      points.default(input$outlier,0,pch = 16,cex = 2.2)
      abline(v = (round(mean(table1), digits = 1)), col = "red", lwd = 2.5)
      abline(v = (round(median(table1), digits = 1)), col = "#1c4063", lwd =2.5)
    })
    # values
    hhh <- renderText({
      dataset <- c(input$outlier, round2)
      paste("mean:", round(mean(dataset), digits = 1))
    })
    output$hhh1 <- renderText({
      dataset <- c(input$outlier, round2)
      paste("SD:", round(sd(dataset), digits = 1))
    })
    output$hhh2 <- renderText({
      dataset <-c(input$outlier, round2)
      paste("min:", round(min(dataset), digits = 1))
    })
    output$hhh3 <- renderText({
      dataset <- c(input$outlier, round2)
      paste("Q1:", round(quantile(dataset, 1/4), digits = 1))
    })
    output$hhh4 <- renderText({
      dataset <- c(input$outlier, round2)
      paste(" median:", round(median(dataset), digits = 1))
    })
    output$hhh5 <- renderText({
      dataset <- c(input$outlier, round2)
      paste(" Q3:", round(quantile(dataset, 3/4), digits = 1))
    })
    output$hhh6 <- renderText({
      dataset <- c(input$outlier, round2)
      paste("max:", round(max(dataset), digits = 1))
    })
    sliderValues <- reactive({
      dataset <- c(input$outlier, round2)
      data.frame(
        mean = (as.character(round(mean(dataset), digits = 1))),
        sd = (as.character(round(sd(dataset), digits = 1))),
        stringsAsFactors = FALSE)
    })
    output$values <- renderTable(
      {sliderValues()},
      width = '33%', spacing = 'xs', align = 'c', 
      striped = TRUE, bordered = TRUE
    )
    sliderValues2 <- reactive({
      dataset <- c(input$outlier, round2)
      data.frame(
        min = (as.character(round(min(dataset), digits = 1))),
        Q1 = (as.character(round(quantile(dataset,1/4), digits = 1))),
        median = (as.character(round(median(dataset), digits = 1))),
        Q3 = (as.character(round(quantile(dataset,3/4), digits = 1))),
        max = (as.character(round(max(dataset), digits = 1))),
        stringsAsFactors = FALSE)
    })
    output$values2 <- renderTable(
      {sliderValues2()}, 
      width = "100%", spacing = 'xs', align = 'c',
      striped = TRUE, bordered = TRUE
    )
  })
})

