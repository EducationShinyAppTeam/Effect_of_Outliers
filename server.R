server <- function(input, output,session) { 
  rand2 <- eventReactive(
    input$goButton2, { round(rnorm(input$num, mean = input$mean, sd=input$sd),digits =2)}  
  )
  
  observeEvent(input$go, {
    
    updateTabItems(session, "tabs", "fivenumber")
  })
  
  output$distPlot2 <- renderPlot({      
    
    table1 <- c(input$X1, rand2())
    boxplot(table1,  main="boxplot",
            ylab = 'range', xlab = 'x',col = "#A93226", border = 'orange', background = 'black')
    points.default(input$X1,pch=16,cex=2.2)
    withProgress(session, min=1, max=15, {
      setProgress(message = 'Generating Result',
                  detail = 'Generating Sample Statistics, boxplot and histogram')
      for (i in 1:10) {
        setProgress(value = i)
        Sys.sleep(0.1)
      }
    })
  })
  
  
  output$hisPlot2 <- renderPlot({
    
    
    table1 <- c(input$X1, rand2())
    hist(table1, ylab = 'Y', xlab = 'values', col = '#5499C7',
         border = "black",main="histogram")
    points.default(input$X1,0,pch=16,cex=2.2)
    abline(v = (round(mean(table1), digits = 1)), col = "yellow", lwd = 2.5)
    abline(v = (round(median(table1), digits = 1)), col = "orange", lwd =2.5)
    })
  
  
  
  output$hhh <- renderText({
    dataset <- c(input$X1, rand2())
    paste("mean:",round(mean(dataset), digits = 1))
    
  })
  
  output$hhh1 <- renderText({
    dataset <- c(input$X1, rand2())
    paste(" SD:",round(sd(dataset), digits = 1))
    
  })
  output$hhh2 <- renderText({
    dataset <-c(input$X1, rand2())
    paste("min:",round(min(dataset), digits = 1))
    
  })
  output$hhh3 <- renderText({
    dataset <- c(input$X1, rand2())
    paste("Q1:",round(quantile(dataset, 1/4), digits = 1))
    
  })
  output$hhh4 <- renderText({
    dataset <- c(input$X1, rand2())
    paste(" median:",round(median(dataset), digits = 1))
    
  })
  output$hhh5 <- renderText({
    dataset <- c(input$X1, rand2())
    paste(" Q3:",round(quantile(dataset, 3/4), digits = 1))
    
  })
  output$hhh6 <- renderText({
    dataset <- c(input$X1, rand2())
    paste("max:",round(max(dataset), digits = 1))
    
  })
}