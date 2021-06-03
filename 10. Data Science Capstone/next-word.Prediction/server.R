suppressWarnings(library(shiny))
suppressWarnings(library(dplyr))
suppressWarnings(library(stringr))


bi_words <- readRDS("biwords.RData")
tri_words <- readRDS("triwords.RData")
quad_words <- readRDS("quadwords.RData")

bigram <- function(input_words){
    num <- length(input_words)
    filter(bi_words, word1==input_words[num])[1,] %>%
        select(num_range("word", 2)) %>%
        as.character() -> out
    ifelse(is.na(out), "?", return(out))
}

trigram <- function(input_words){
    num <- length(input_words)
    filter(tri_words, 
           word1==input_words[num-1], 
           word2==input_words[num])[1,]  %>%
        select(num_range("word", 3)) %>%
        as.character() -> out
    ifelse(is.na(out), bigram(input_words), return(out))
}

quadgram <- function(input_words){
    num <- length(input_words)
    filter(quad_words, 
           word1==input_words[num-2], 
           word2==input_words[num-1], 
           word3==input_words[num])[1,]  %>%
        select(num_range("word", 4)) %>%
        as.character() -> out
    ifelse(is.na(out), trigram(input_words), return(out))
}



ngrams <- function(input){
    input <- data.frame(text = input)
    replace_reg <- "[^[:alpha:][:space:]]*"
    input <- input %>% mutate(text = str_replace_all(text, replace_reg, ""))
    input_count <- str_count(input, boundary("word"))
    input_words <- unlist(str_split(input, boundary("word")))
    input_words <- tolower(input_words)
    out <- ifelse(input_count == 1, bigram(input_words), 
                  ifelse (input_count == 2, trigram(input_words), quadgram(input_words)))
    return(out)
}

shinyServer(function(input, output) {
        
        output$ngram_output_pred <- renderText({
            if (input$user_input_pred=='') {
                return()
            }
            ngrams(input$user_input_pred)
        })
        
        output$ngram_output_gen <- renderText({
            if (input$user_input_gen=='') {
                return()
            }
            s <- input$user_input_gen
            for (i in seq(6)) {
                n <- ngrams(s)
                s <- paste(s,n)
            }
            s
        })
        
})





