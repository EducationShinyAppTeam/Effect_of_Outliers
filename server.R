shinyServer(function(session, input,output){
  observeEvent(input$go,{
    updateTabItems(session, "tabs", "fivenumber")
  })
  

  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "Click on desired square, answer the question, then hit submit and go to next question.",
      type = "info"
    )
  })
  
  observeEvent({input$num
    input$mean
    input$sd
  },
  {
    round2<-round(rnorm(input$num-1, mean = input$mean, sd=input$sd),digits =2)
    #distribution plot     
    output$distPlot2 <- renderPlot({
      table1 <- c(input$X1, round2)
      boxplot(table1,  ylab = 'value', xlab = 'x',font.lab = 2,
              col = "#ffdaad", border = '#1c4063', background = 'black',main="boxplot")
      points.default(input$X1,pch = 16,cex = 2.2)
    })
    
    
    #histogram plot
    output$hisPlot2 <- renderPlot({
      table1 <- c(input$X1, round2)
      hist(table1, ylab = 'frenquency', xlab = 'value', 
           col = 'lightblue', border = "#1c4063",background = 'black', main="histogram")
      points.default(input$X1,0,pch = 16,cex = 2.2)
      abline(v = (round(mean(table1), digits = 1)), col = "red", lwd = 2.5)
      abline(v = (round(median(table1), digits = 1)), col = "#1c4063", lwd =2.5)
      # legend("right", legend="median", col="#1c4063", lty=1:2, cex=1, box.lty=0)
      #legend("right", legend="mean", col="red", lty=1:2, cex=1, box.lty=0)
      #legend("topright", legend="median", col="#1c4063", lty=1:2, cex=1, box.lty=0)
    })
    
    
    hhh <- renderText({
      dataset <- c(input$X1, round2)
      paste("mean:",round(mean(dataset), digits = 1))
      
    })
    
    output$hhh1 <- renderText({
      dataset <- c(input$X1, round2)
      paste("SD:",round(sd(dataset), digits = 1))
      
    })
    output$hhh2 <- renderText({
      dataset <-c(input$X1, round2)
      paste("min:",round(min(dataset), digits = 1))
      
    })
    output$hhh3 <- renderText({
      dataset <- c(input$X1, round2)
      paste("Q1:",round(quantile(dataset, 1/4), digits = 1))
      
    })
    output$hhh4 <- renderText({
      dataset <- c(input$X1, round2)
      paste(" median:",round(median(dataset), digits = 1))
      
    })
    output$hhh5 <- renderText({
      dataset <- c(input$X1, round2)
      paste(" Q3:",round(quantile(dataset, 3/4), digits = 1))
      
    })
    output$hhh6 <- renderText({
      dataset <- c(input$X1, round2)
      paste("max:",round(max(dataset), digits = 1))
      
    })
    
    sliderValues <- reactive({
      dataset <- c(input$X1, round2)
      data.frame(
        mean = (as.character(round(mean(dataset), digits = 1))),
        sd = (as.character(round(sd(dataset), digits = 1))),
        #min = (as.character(round(min(dataset), digits = 1))),
        #Q1 = (as.character(round(quantile(dataset,1/4), digits = 1))),
        #median = (as.character(round(median(dataset), digits = 1))),
        #Q3 = (as.character(round(quantile(dataset,3/4), digits = 1))),
        #max = (as.character(round(max(dataset), digits = 1))),
        stringsAsFactors = FALSE)
      
    })
    output$values <- renderTable({
      sliderValues()
    }, width = '33%', spacing = 'xs', align = 'c', striped = TRUE, bordered = TRUE)
      
    
    sliderValues2 <- reactive({
      dataset <- c(input$X1, round2)
      data.frame(
        #mean = (as.character(round(mean(dataset), digits = 1))),
        #sd = (as.character(round(sd(dataset), digits = 1))),
        min = (as.character(round(min(dataset), digits = 1))),
        Q1 = (as.character(round(quantile(dataset,1/4), digits = 1))),
        median = (as.character(round(median(dataset), digits = 1))),
        Q3 = (as.character(round(quantile(dataset,3/4), digits = 1))),
        max = (as.character(round(max(dataset), digits = 1))),
        stringsAsFactors = FALSE)
      
    })
    
    output$values2 <- renderTable({
      sliderValues2()
    },  width = "100%", spacing = 'xs', align = 'c', striped = TRUE, bordered = TRUE)
    
    
    # sliderInput2 <- function(inputId, label, min, max, value, step=NULL, from_min, from_max){
    #   x <- sliderInput(inputId, label, min, max, value, step)
    #   x$num <- c(x$num-1,
    #              "data-from-min" = from_min,
    #              "data-from-max" = from_max,
    #              "data-from-shadow" = TRUE)
    #   x
    # }
  })
})

