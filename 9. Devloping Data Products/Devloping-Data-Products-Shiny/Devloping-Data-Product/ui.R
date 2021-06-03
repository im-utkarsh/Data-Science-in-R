library(shiny)
library(markdown)

shinyUI(fluidPage(
    titlePanel("Visualize Models"),
    sidebarLayout(
        sidebarPanel(
            sliderInput('con', 'Confidence Interval',value = 95, min = 0, max = 100,
                        step = 2.5,),
            selectInput("var", "Variable (against Income) :",
                        c("Population" = "Population", "Illiteracy" = "Illiteracy",
                          "Life Expectancy" = "Life.Exp", 'Murder'='Murder',
                          'High-School Grade'='HS.Grad','Frost'='Frost',
                          'Area'='Area')),
            h3("Slope"),
            textOutput("slopeOut"),
            h3("Intercept"),
            textOutput("intOut")
        ),
        mainPanel(
            tabsetPanel(type = "tabs",
                tabPanel("Plot",
                         plotOutput("plot1", brush = brushOpts(id = "brush1",
                                                               fill = "hotpink"))),
                tabPanel("About", includeMarkdown("about.md"))
            )
        )
    )
))