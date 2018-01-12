## This is the model file for word prediction for Capstone Project of Data Science.
## bigram.Rda, trigram.Rda, quadgram.Rda files are produced from ngram.R file.


## necessary libraries
library(stringi)
library(dplyr)
library(tm)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_152')
library(RWeka)
library(data.table)



## loading .Rda files.
load(file = "bigram.Rda")
load(file = "trigram.Rda")
load(file = "quadgram.Rda")


## predfun is the function which predicts the next word
## user.sen is like user sentence, whatever user types
predfun <- function(user.sen){
  
  value = "Next word is" #initialising
  sentence <- user.sen %>%
              tolower() %>%
              strsplit(' ') %>% 
              unlist() #striping the user sentence into different words 

## If user types more than 3 words, it takes only last three words and predicts
## value from quadgram function  
        if(length(sentence) >= 3){
          value <- quadgram(sentence[(length(sentence) - 2) : length(sentence)])
        }

## if quadgram does not match or user types only two words, it checks last two words and
## predicts value from trigram function
        if(is.null(value) || length(sentence) == 2){
          value <- trigram(sentence[(length(sentence) - 1) : length(sentence)])
        }
  
## if bigram does not match or user types only one word, it checks last word and
## predicts value from bigram function     
        if(is.null(value) || length(sentence) == 1){
          value = bigram(sentence[length(sentence)])
        }
  
## if nothing matches, it just returns "the"      
        if(is.null(value)){
          value = "the" # if it can't predict from ngrams, it randomly suggests "the"
        }
## at the end it returns the predicted word      
        return(value)
  
} # predfun function ends


## quadgram is the function to look for predicted word from quadgramlist
quadgram <- function(quad.sen){
  
## initialising a data frame keeping quadgramlist of quadgram.Rda in mind  
  quad.df <- data.frame(quadgram ="test", w1 = "word1", w2 = "word2",
                       w3 = "word3", w4 = "word4", frequency=0) 

## for loop runs from the whole length of quadgramlist    
    for(i in 1:length(quadgramlist)){
      
## in quad.match it checks whether typed three words match with quadgram 1st three words
## or not
      quad.match <- quadgramlist[quadgramlist$w1 == quad.sen[1] & 
                                 quadgramlist$w2 == quad.sen[2] &
                                 quadgramlist$w3 == quad.sen[3]] 
      
      n <- as.numeric(quad.match$frequency)
    
## if match is found, it includes in quad.df data frame              
      if(length(n) != 0){
        quad.df <- rbind(quad.df, quad.match)
      }
      
    } ##for loop ends
  
## after running the whole loop, if quad.df has only one row that means no match
## is found, then it returns NULL
      if(nrow(quad.df) == 1) return(NULL)

##  otherwise
      quad.df <- quad.df[order(-frequency)]
      #quad.result <- droplevels(quad.df$w4[1])
      #return(quad.result) # with this a level message is appearing
      quad.result <- unlist(strsplit(as.String(quad.df[1, quadgram]), ' '))
      return (quad.result[length(quad.result)])
  
} # quadgram function ends


## trigram is the function to look for predicted word from trigramlist
trigram <- function(tri.sen){
    
  tri.df <- data.frame(trigram ="test", w1 = "word1", w2 = "word2",
                           w3 = "word3", frequency=0)
       
    for(i in 1:length(trigramlist)){
       tri.match <- trigramlist[trigramlist$w1 == tri.sen[1] & 
                                trigramlist$w2 == tri.sen[2]]
       
       n <- as.numeric(tri.match$frequency)
             
       if(length(n) != 0){
          tri.df <- rbind(tri.df, tri.match)
        }
    }

      if(nrow(tri.df) == 1) return(NULL)

      tri.df <- tri.df[order(-frequency)]
      #tri.result <- droplevels(tri.df$w3[1])
      #return(tri.result)
      tri.result <- unlist(strsplit(as.String(tri.df[1, trigram]), ' '))
      return (tri.result[length(tri.result)])
}


## bigram is the function to look for predicted word from bigram list
bigram <- function(bi.sen){
  
   bi.df <- data.frame(bigram ="test", w1 = "word1", w2 = "word2", frequency=0)
    
    for(i in 1:length(bigramlist)){
      bi.match <- bigramlist[bigramlist$w1 == bi.sen[1]]
      
      n <- as.numeric(bi.match$frequency)
                   
      if(length(n) != 0){
        bi.df <- rbind(bi.df, bi.match)
      }
    } 
    
    if(nrow(bi.df) == 1) return(NULL)
    
    bi.df <- bi.df[order(-frequency)]
    #bi.result <- droplevels(bi.df$w2[1])
    #return(bi.result)
    bi.result <- unlist(strsplit(as.String(bi.df[1, bigram]), ' '))
    return (bi.result[length(bi.result)])
    
}
  
  
  
  
  