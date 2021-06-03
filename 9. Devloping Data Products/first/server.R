
library(shiny)

shinyServer(
    function(input, output) {
        mu <- reactive({input$mu})
        sd <- reactive({input$sd})
        line <- reactive({input$line})
        col <- reactive({input$col})
        s2 <- reactive({input$s2})
        output$plot = renderPlot({
            x <- seq(-10,10,length.out=500/sd())
            y <- dnorm(x,mu(),sd())
            plot(x,y,xlim = c(-10,10),type = 'l',lwd=2,col=col())
            abline(v=line()[2],col=col()+1,lwd=2)
            if (s2()) {
                abline(v=line()[1],col=col()+2,lwd=2)
            }
        })
        p <- reactive({round(pnorm(line(),mu(),sd(),lower.tail = FALSE),3)*100})
        output$prob1 <-renderText({
            if (s2()) {
                paste0(c(100-p()[1],p()[2]),'%')
            } else {
                paste0(100-p()[1],'%')
            }
        },sep = ', ')
        output$prob2 <- renderText({
            ifelse(s2(),paste0(sum(100-p()[1],p()[2]),'%'),'-')
        })
    }
)
