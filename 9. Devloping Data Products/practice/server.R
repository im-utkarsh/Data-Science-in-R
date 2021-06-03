library(shiny)

shinyServer(function(input, output) {
    model <- reactive({
        brushed_data <- brushedPoints(mtcars, input$brush1,
                                      xvar = "mpg", yvar = "disp")
        if(nrow(brushed_data) < 2){
            return(NULL)
        }
        lm(disp ~ mpg, data = brushed_data)
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
        plot(mtcars$mpg, mtcars$disp, xlab = "mpg",
             ylab = "disp", main = "Cars Measurements",
             cex = 1.5, pch = 16, bty = "n")
        if(!is.null(model())){
            abline(model(), col = "blue", lwd = 2)
        }
    })
})