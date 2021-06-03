library(shiny)

shinyServer(function(input, output) {
    
    data <- abs(data.frame(state.x77))
    data$Frost <- 1+data$Frost
    data <- log(data)
    x <- reactive({input$var})
    
    brushed_data <- reactive({brushedPoints(data, input$brush1, xvar = x(),
                                            yvar = "Income")})
    
    model <- reactive({
        if(nrow(brushed_data()) < 2){
            return(NULL)
        }
        
        lm <- switch (x(),
           'Population' = lm(Income ~ Population, data=brushed_data()),
           'Illiteracy' = lm(Income ~ Illiteracy, data=brushed_data()),
           'Life.Exp' = lm(Income ~ Life.Exp, data=brushed_data()),
           'Murder' = lm(Income ~ Murder, data=brushed_data()),
           'HS.Grad' = lm(Income ~ HS.Grad, data=brushed_data()),
           'Frost' = lm(Income ~ Frost, data=brushed_data()),
           'Area' = lm(Income ~ Area, data=brushed_data())
        )
        lm
    })
    
    output$slopeOut <- renderText({
        if(is.null(model())){
            "No Model Found"
        } else {
            model()[[1]][2]
        }
    })
    output$intOut <- renderText({
        if(is.null(model())){
            "No Model Found"
        } else {
            model()[[1]][1]
        }
    })
    output$plot1 <- renderPlot({
        plot(data[,x()], data$Income, xlab = x(),
             ylab = "Income", main = paste("Income by",x()),
             cex = 1.5, pch = 16, bty = "n", col='#00BB00EE')
        
        if(!is.null(model())){
            
            # predicts + interval
            newx <- seq(min(data[,x()]), max(data[,x()]),
                        length.out=100)
            
            newData <- switch (x(),
                'Population' = data.frame(Population=newx),
                'Illiteracy' = data.frame(Illiteracy=newx),
                'Life.Exp' = data.frame(Life.Exp=newx),
                'Murder' = data.frame(Murder=newx),
                'HS.Grad' = data.frame(HS.Grad=newx),
                'Frost' = data.frame(Frost=newx),
                'Area' = data.frame(Area=newx)
            )
            
            preds <- predict(model(), newdata = newData, interval = 'confidence',
                             level=input$con/100)
            
            # add fill
            polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]),
                    col = '#15f4ee88', border = NA)
            
            # model
            abline(model(), col = "blue2", lwd = 2.4)
            
            # intervals
            lines(newx, preds[ ,3], lty = 'dashed', col = 'red2', lwd=1.8)
            lines(newx, preds[ ,2], lty = 'dashed', col = 'red2', lwd=1.8)
        }
    })
})