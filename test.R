library(tidyverse)
library(tidytext)
library(pryr)

setwd("~/R/Capestone")
corpus <- readRDS("corpus10.rds")
unigram <- readRDS("unigram10.rds") 
bigram <- readRDS("bigram10.rds") %>% filter(n>quantile(.$n, 0.75))
trigram <- readRDS("trigram10.rds") %>% filter(n>quantile(.$n, 0.75))
tetragram <- readRDS("tetragram10.rds") %>% filter(n>quantile(.$n, 0.75))
pentagram <- readRDS("pentagram10.rds") %>% filter(n>quantile(.$n, 0.75))
hexagram <- readRDS("hexagram10.rds") %>% filter(n>quantile(.$n, 0.75))


ngrams <- list("one" = unigram, "two" = bigram, "three" =trigram,
               "four" = tetragram, "five" = pentagram, "six" = hexagram)
saveRDS(ngrams, "ngrams.rds")
voc <- tibble(word = unigram$ngram)

maxn <- length(ngrams)
dist <- tibble(word=voc)

input <- tibble(text = "this is just the test") %>% unnest_tokens(word,text, token = "ngrams", n=1)
input[!input$word %in% voc$word, ] <- "unk"
maxinput <- nrow(input)
maxuse <- min(maxinput+1,maxn)
idchg <- 0
repeat{
        ngram.tbl <- ngrams[[maxuse]]
        for (i in 1:(maxuse-1)) {
                ngram.tbl <- ngram.tbl %>% filter(.[,i] == input$word[i+idchg])
                
        }
        if(nrow(ngram.tbl)>0){
                print(ngram.tbl %>% arrange(desc(n)))
                break
        }
        else{
                print("Backed-down")
                maxuse <- maxuse - 1
                idchg <- idchg + 1

        }
        if(maxuse==0){
                print("There is no match")
                break
        }
}

