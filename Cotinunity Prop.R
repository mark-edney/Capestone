library(tidyverse)
library(tidytext)
library(pryr)

setwd("~/R/Capestone")
corpus <- readRDS("corpus10.rds")
unigram <- readRDS("unigram10.rds") 
bigram <- readRDS("bigram10.rds") 
trigram <- readRDS("trigram10.rds") %>% filter(n>quantile(.$n, 0.5))
tetragram <- readRDS("tetragram10.rds") %>% filter(n>quantile(.$n, 0.55))
pentagram <- readRDS("pentagram10.rds") %>% filter(n>quantile(.$n, 0.6))
hexagram <- readRDS("hexagram10.rds") %>% filter(n>quantile(.$n, 0.65))

voc <- tibble(word = unigram$ngram)
ngrams <- list("one" = unigram, "two" = bigram, "three" =trigram,
               "four" = tetragram, "five" = pentagram, "six" = hexagram)
saveRDS(ngrams, "ngrams.rds")

#Cont probability for KNS
cont.prop.func <- function(word, ngrams){
        out <- ngrams %>% 
                filter(.[,ncol(ngrams)-1] == word) %>%
                nrow() 
        out / nrow(ngrams)
}
cont.prop <- list()
cont.prop$one <- tibble(word=voc$word, prop = ngrams$one$n/sum(ngrams$one$n))
cont.prop$two <- tibble(word=voc$word, prop = map_dbl(word, cont.prop.func, ngrams=ngrams$two))
cont.prop$three <- tibble(word=voc$word, prop = map_dbl(word, cont.prop.func, ngrams=ngrams$three))
cont.prop$four <- tibble(word=voc$word, prop = map_dbl(word, cont.prop.func, ngrams=ngrams$four))
cont.prop$five <- tibble(word=voc$word, prop = map_dbl(word, cont.prop.func, ngrams=ngrams$five))
cont.prop$six <- tibble(word=voc$word, prop = map_dbl(word, cont.prop.func, ngrams=ngrams$six))
saveRDS(cont.prop, "cont.prop.rds")

