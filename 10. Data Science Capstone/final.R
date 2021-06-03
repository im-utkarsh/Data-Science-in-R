library(stringr)
library(tidytext)
library(dplyr)
library(readr)
library(tidyr)


blogs <- read_lines("final/en_US/en_US.blogs.txt", skip_empty_rows = TRUE)
news <- read_lines("final/en_US/en_US.news.txt", skip_empty_rows = TRUE)
twitter <- read_lines("final/en_US/en_US.twitter.txt", skip_empty_rows = TRUE)

detach("package:readr", unload = TRUE)

blogs   <- tibble(text = blogs)
news    <- tibble(text = news)
twitter <- tibble(text = twitter)



set.seed(3339)

blogs_sample <- blogs %>%
    sample_n(., nrow(blogs)*0.1)
news_sample <- news %>%
    sample_n(., nrow(news)*0.1)
twitter_sample <- twitter %>%
    sample_n(., nrow(twitter)*0.1)




repo_sample <- bind_rows(mutate(blogs_sample, source = "blogs"),
                         mutate(news_sample,  source = "news"),
                         mutate(twitter_sample, source = "twitter"))



repo_sample$source <- as.factor(repo_sample$source)




rm(blogs, blogs_sample,news, news_sample, twitter, twitter_sample)




clean_sample <-  repo_sample %>%
    mutate(text = str_replace_all(text, '[^[:alpha:][:space:]]*', "")) %>%
    mutate(text = str_replace_all(text, 'http[^[:space:]]*', "")) %>%
    mutate(text = str_replace_all(text, '\\b(?=\\w*(\\w)\\1)\\w+\\b', "")) %>% 
    mutate(text = iconv(text, from = 'UTF-8',to = "ASCII//TRANSLIT"))


rm(repo_sample)

##########################################################################

clean_sample <- readRDS('CleanSample.RData')

bigram_repo <- clean_sample  %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2)

trigram_repo <- clean_sample  %>%
    unnest_tokens(trigram, text, token = "ngrams", n = 3)

quadgram_repo <- clean_sample  %>%
    unnest_tokens(quadgram, text, token = "ngrams", n = 4)

quintgram_repo <- clean_sample  %>%
    unnest_tokens(quintgram, text, token = "ngrams", n = 5)

sextgram_repo <- clean_sample  %>%
    unnest_tokens(sextgram, text, token = "ngrams", n = 6)

rm(clean_sample)



bigram_cover <- bigram_repo %>%
    count(bigram) %>%  
    filter(n > 10) %>%
    arrange(desc(n))  
rm(bigram_repo)


trigram_cover <- trigram_repo %>%
    count(trigram) %>%  
    filter(n > 10) %>%
    arrange(desc(n))  
rm(trigram_repo)


quadgram_cover <- quadgram_repo %>%
    count(quadgram) %>%  
    filter(n > 10) %>%
    arrange(desc(n))  
rm(quadgram_repo)


quintgram_cover <- quintgram_repo %>%
    count(quintgram) %>%  
    filter(n > 10) %>%
    arrange(desc(n))  
rm(quintgram_repo)


sextgram_cover <- sextgram_repo %>%
    count(sextgram) %>%  
    filter(n > 10) %>%
    arrange(desc(n))  
rm(sextgram_repo)





bi_words <- bigram_cover %>%
    separate(bigram, c("word1", "word2"), sep = " ")
rm(bigram_cover)

tri_words <- trigram_cover %>%
    separate(trigram, c("word1", "word2", "word3"), sep = " ")
rm(trigram_cover)

quad_words <- quadgram_cover %>%
    separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ")
rm(quadgram_cover)

quint_words <- quintgram_cover %>%
    separate(quintgram, c("word1", "word2", "word3", "word4", "word5"), sep = " ")
rm(quintgram_cover)

sext_words <- sextgram_cover %>%
    separate(sextgram, c("word1", "word2", "word3", "word4", "word5", "word6"), sep = " ")
rm(sextgram_cover)





saveRDS(bi_words, file="biWords.RData")
saveRDS(tri_words, file="triWords.RData")
saveRDS(quad_words,file="quadWords.RData")

saveRDS(quint_words,file="quintWords.RData")
saveRDS(sext_words,file="sextWords.RData")










bi_words <- readRDS("biWords.RData")
tri_words <- readRDS("triWords.RData")
quad_words <- readRDS("quadWords.RData")




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




