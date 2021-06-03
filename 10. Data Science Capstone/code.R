library(tm)
library(RWeka)
library(readr)
library(ggplot2)

if ( !file.exists("Coursera-SwiftKey.zip") ) {
    download.file('https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip',
                  'Coursera-SwiftKey.zip')
}

unzip('Coursera-SwiftKey.zip', list = TRUE)

if (!file.exists('bad-words.txt')) {
    download.file('https://www.cs.cmu.edu/~biglou/resources/bad-words.txt','bad-words.txt')
}

blogs <- read_lines("final/en_US/en_US.blogs.txt", skip_empty_rows = TRUE)
news <- read_lines("final/en_US/en_US.news.txt", skip_empty_rows = TRUE)
twitter <- read_lines("final/en_US/en_US.twitter.txt", skip_empty_rows = TRUE)

detach("package:readr", unload = TRUE)

sb <- file.size("final/en_US/en_US.blogs.txt")
sn <- file.size("final/en_US/en_US.news.txt")
st <- file.size("final/en_US/en_US.twitter.txt")

data.frame(source = c("blogs", "news", "twitter"),
           size.MB = c(sb/1024, sn/1024, st/1024),
           num.lines = c(NROW(blogs),NROW(news),NROW(twitter)),
           num.char = c(sum(nchar(blogs)),sum(nchar(news)),sum(nchar(twitter))))

set.seed(3339)
blogs <- blogs[sample(length(blogs),25000)]
news <- news[sample(length(news),25000)]
twitter <- twitter[sample(length(twitter),25000)]

## Words for profanity filtering are taken from *Luis von Ahn's*[https://www.cs.cmu.edu/~biglou] Research Group.
badwords <- readLines('bad-words.txt')

corp <- VCorpus(VectorSource(c(blogs,news,twitter)))
corp <- tm_map(corp, content_transformer(tolower))
## for removing pattern of choice
remov <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
corp <- tm_map(corp, remov, "(f|ht)tp(s?)://(.*)[.][a-z]+")
corp <- tm_map(corp, remov, "@[^\\s]+")
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp,removePunctuation)
corp <- tm_map(corp,removeWords, badwords)
corp <- tm_map(corp,removeWords, stopwords())
corp <- tm_map(corp, stripWhitespace)

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

TDM1 <- TermDocumentMatrix(corp, control = list(stemming=TRUE))
TDM2 <- TermDocumentMatrix(corp, control = list(tokenize = BigramTokenizer, stemming=TRUE))
TDM3 <- TermDocumentMatrix(corp, control = list(tokenize = TrigramTokenizer, stemming=TRUE))

inspect(TDM1)
inspect(TDM2)
inspect(TDM3)

f1 <- findFreqTerms(TDM1,lowfreq =2500)
f2 <- findFreqTerms(TDM2, lowfreq = 100)
f3 <- findFreqTerms(TDM3, lowfreq = 10)
Unigramfreq <- sort(rowSums(as.matrix(TDM1[f1,])),decreasing = TRUE)
Unigramfreq <- data.frame(word=names(Unigramfreq),frequency=Unigramfreq)
Bigramfreq <- sort(rowSums(as.matrix(TDM2[f2,])),decreasing = TRUE)
Bigramfreq <- data.frame(word=names(Bigramfreq),frequency=Bigramfreq)
Trigramfreq <- sort(rowSums(as.matrix(TDM3[f3,])),decreasing = TRUE)
Trigramfreq <- data.frame(word=names(Trigramfreq),frequency=Trigramfreq)


g <- ggplot(Unigramfreq[1:27,], aes(x = factor(word,levels = Unigramfreq$word[1:27],
                                               ordered=TRUE), y = frequency)) + 
    geom_bar(stat = "identity", fill='aquamarine3')
g + ggtitle("Most common Unigrams") + xlab("Unigrams") + ylab("Frequency") +
    geom_text(stat="count", y=30, hjust=0, size = 5.4, angle = 90,color='white',
              label=Unigramfreq[1:27,]$word) + 
    theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())

g <- ggplot(Bigramfreq[1:27,], aes(x = factor(word,levels = Bigramfreq$word[1:27],
                                               ordered=TRUE), y = frequency)) + 
    geom_bar(stat = "identity", fill='aquamarine3')
g + ggtitle("Most common Bigrams") + xlab("Bigrams") + ylab("Frequency") +
    geom_text(stat="count", y=0.9, hjust=0, size = 4.2, angle = 90,color='white',
              label=Bigramfreq[1:27,]$word) + 
    theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())

g <- ggplot(Trigramfreq[1:27,], aes(x = factor(word,levels = Trigramfreq$word[1:27],
                                               ordered=TRUE), y = frequency)) + 
    geom_bar(stat = "identity", fill='aquamarine3')
g + ggtitle("Most common Trigrams") + xlab("Trigrams") + ylab("Frequency") +
    geom_text(stat="count", y=0.3, hjust=0, size = 3.6, angle = 90,color='black',
              label=Trigramfreq[1:27,]$word) + 
    theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())

