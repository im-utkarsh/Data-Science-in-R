
library(shiny)

shinyUI(fluidPage(
        pageWithSidebar(
        headerPanel("Example plot"),
        sidebarPanel(
            sliderInput('mu', 'Mean',value = 0, min = -5, max = +5, step = 0.5,),
            sliderInput('sd', 'Standard Deviation',value = 1, min = 0.5, max = 2, step = 0.05,),
            sliderInput('line', 'Line',value = c(-1.96,1.96), min = -9, max = 9, step = .05,),
            numericInput('col', 'Color',value = 1, min = 1, max = 8, step = 1,),
            checkboxInput("s2", "2 sided", value = TRUE)
        ),
        mainPanel(
            plotOutput('plot'),
            h3('Probability of a point as extreme as shown by line(s)'),
            h4('1 sided'),
            strong(textOutput('prob1')),
            h4('2 sided'),
            strong(textOutput('prob2'))
        )
    ))
)