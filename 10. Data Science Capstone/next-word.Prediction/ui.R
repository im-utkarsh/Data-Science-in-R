library(shiny)
shinyUI(fluidPage(
        titlePanel("Text Prediction Model"),
        p("This app that takes an input phrase (multiple words) in a text box and outputs a prediction of the next word."),
        
        tabsetPanel(
            tabPanel('Predict',
                     sidebarLayout(
                         sidebarPanel(
                             h2("Instructions:"), 
                             h5("1. Enter a word or words in the text box."),
                             h5("2. The predicted next word prints below it in blue."),
                             h5("3. A question mark means no prediction, typically due to mis-spelling"),
                             br(),
                         ),
                         mainPanel(h2("Predict"),
                                      textInput("user_input_pred", h3("Your Input:")),
                                      h3("Predicted Next Word:"),
                                      h4(em(span(textOutput("ngram_output_pred"), style="color:blue")))
                             )   
                         )
                     ),
            tabPanel('Generate',
                     sidebarLayout(
                         sidebarPanel(
                             h2("Instructions:"), 
                             h5("1. Enter a word or words in the text box."),
                             h5("2. The generated sentence prints below it in blue."),
                             h5("3. A question mark means no prediction, typically due to mis-spelling"),
                             h5("4. The sentence will be generated upto next 6 words."),
                             br(),
                         ),
                         mainPanel(h2("Generate"),
                                   textInput("user_input_gen", h3("Your Input:")),
                                   h3("Generated Sentece:"),
                                   h4(em(span(textOutput("ngram_output_gen"), style="color:blue")))
                         )   
                     )
            )
        )
))
