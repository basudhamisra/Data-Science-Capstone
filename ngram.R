## This file generates unigram, bigram, trigram and quadgram combinations from coursera 
## swiftkey data source for word prediction and saves them in 4 different .Rda files
## Once these .Rda files are produced we do not have to run this R code again and again.
## We will copy these bigram.Rda, trigram.Rda, quadgram.Rda files in shinyapp directory.
## shinyapp directory has newmodel.R, ui.R, server.R and three .Rda files.
## Though ngram.R runtime takes little bit more than 4 minutes, shinyapp runtime is
## very fast. It predicts word within 1-2 seconds.


## necessary libraries
library(stringi)
library(dplyr)
library(tm)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_152')
library(RWeka)
library(data.table)

## this function calculates how much time it takes to run the whole job by calculating the
## difference between starting and ending time
sleep_for_a_minute <- function() { Sys.sleep(60) }
start_time <- Sys.time()



# uploading data
blogs <- readLines("final/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
news <- readLines("final/en_US/en_US.news.txt", encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
twitter <- readLines("final/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE, warn = FALSE)


# sampling the data
set.seed(111)
sample.size <- 5000
sample.blogs <- blogs[sample(1:length(blogs),sample.size)]
sample.news <- news[sample(1:length(news),sample.size)] 
sample.twitter <- twitter[sample(1:length(twitter),sample.size)]

sample.data <- c(sample.blogs,sample.news,sample.twitter) %>%
  sapply(function(row) iconv(row, "latin1", "ASCII", sub=""))#removes non-ASCII characters 

# creating cleaning function
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
preprocessCorpus <- function(corpus){
  corpus <- tm_map(corpus, toSpace, "/|@|\\|") %>% # removes special characters
    tm_map(content_transformer(tolower)) %>% # transform everything in lower case
    tm_map(removeNumbers) %>% # remove numbers
    tm_map(removePunctuation) %>% # remove punctuations
    tm_map(stripWhitespace) # remove extra white space
  return(corpus)
}

# cleaning sample data
sample.data <- VCorpus(VectorSource(sample.data))
sample.data <- preprocessCorpus(sample.data)


# tokenizer functions
UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))
QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=4, max=4))


# calculating frequencies for various n-gram models

unitdm <- sample.data %>%
          TermDocumentMatrix(control=list(tokenize=UnigramTokenizer)) 
unifreqterm <- findFreqTerms(unitdm, lowfreq = 5) # keeps unigrams upto 5 frequencies
unifreq <- unitdm[unifreqterm,] %>%
           as.matrix() %>%
           rowSums() %>%
           sort(decreasing=TRUE)
unidf <- data.frame(unigram = names(unifreq), frequency = unifreq)
unigramlist <- setDT(unidf)
save(unigramlist, file = "unigram.Rda")


bitdm <- sample.data %>%
         TermDocumentMatrix(control=list(tokenize=BigramTokenizer)) 
bifreqterm <- findFreqTerms(bitdm, lowfreq = 3) 
bifreq <- bitdm[bifreqterm,] %>%
          as.matrix() %>%
          rowSums() %>%
          sort(decreasing=TRUE)
bidf <- data.frame(bigram = names(bifreq), frequency = bifreq)
## splits bigram words into two words and save them separately in data frame 
## in the following three lines
bi.out <- strsplit(as.character(bidf$bigram),' ')
bidf <- data.frame(bidf$bigram, do.call(rbind, bi.out), bidf$frequency)
names(bidf) <- c("bigram", "w1", "w2", "frequency")
bigramlist <- setDT(bidf)
save(bigramlist, file = "bigram.Rda") 

tritdm <- sample.data %>%
          TermDocumentMatrix(control=list(tokenize=TrigramTokenizer)) 
trifreqterm <- findFreqTerms(tritdm, lowfreq = 2) 
trifreq <- tritdm[trifreqterm,] %>%
           as.matrix() %>%
           rowSums() %>%
           sort(decreasing=TRUE)
tridf <- data.frame(trigram = names(trifreq), frequency = trifreq)
tri.out <- strsplit(as.character(tridf$trigram),' ')
tridf <- data.frame(tridf$trigram, do.call(rbind, tri.out), tridf$frequency)
names(tridf) <- c("trigram", "w1", "w2", "w3", "frequency")
trigramlist <- setDT(tridf)
save(trigramlist, file = "trigram.Rda") 

quadtdm <- sample.data %>%
           TermDocumentMatrix(control=list(tokenize=QuadgramTokenizer)) 
quadfreqterm <- findFreqTerms(quadtdm, lowfreq = 2) 
quadfreq <- quadtdm[quadfreqterm,] %>%
            as.matrix() %>%
            rowSums() %>%
            sort(decreasing=TRUE)
quaddf <- data.frame(quadgram = names(quadfreq), frequency = quadfreq)
quad.out <- strsplit(as.character(quaddf$quadgram),' ')
quaddf <- data.frame(quaddf$quadgram, do.call(rbind, quad.out), quaddf$frequency)
names(quaddf) <- c("quadgram", "w1", "w2", "w3", "w4", "frequency")
quadgramlist <- setDT(quaddf)
save(quadgramlist, file = "quadgram.Rda")

sleep_for_a_minute()
end_time <- Sys.time()
total_time <- end_time - start_time
print(total_time)

## some sample print commands
#print(dim(unigramlist))
#print(dim(tridf))
#print(class(tridf))
#print(bigramlist[1:2,])
#print(trigramlist[1:2,])
#print(tridf[1:2,])

