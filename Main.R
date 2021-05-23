library(tidyverse)
library(tidytext)
library(pryr)
setwd("~/R/Capestone")
blog <- read_lines("~/R/Capestone/data/final/en_US/en_US.blogs.txt")
news <- read_lines("~/R/Capestone/data/final/en_US/en_US.news.txt")
twitter <- read_lines("~/R/Capestone/data/final/en_US/en_US.twitter.txt")
blog <- tibble(text = blog) 
news <- tibble(text = news)
twitter <- tibble(text = twitter)

set.seed(90210)
corpus <- bind_rows(blog,twitter,news) %>% 
        slice_sample(prop = 0.10) %>%
        mutate(line = row_number())

corpus <- readRDS("corpus10.rds")
set.seed(90210)
corpus <- corpus %>%
        slice_sample(prop = 0.10) %>%
        mutate(line = row_number())
testset <- bind_rows(blog,twitter,news) %>% 
        slice_sample(n=500) 
testset <- testset[!testset$text %in% corpus$text,] %>%
        mutate(line=row_number())
saveRDS(testset, "testset.rds")

prof <- read_lines("~/R/Capestone/data/facebook-bad-words-list_comma-separated-text-file_2021_01_18.txt")[15]
prof <- prof %>% str_split(", ") %>% flatten %>% unlist
prof <- tibble("word" = prof)

english <- read_lines("~/R/Capestone/data/diction.txt")
english <- tibble("word" = english[!english==""])

contract <- read_lines("~/R/Capestone/data/contractions.txt")
contract <- tibble("word" = contract) %>% unnest_tokens(word,word)

#clean up ram
voc <- bind_rows(english, contract) %>% anti_join(prof)

unigram <- corpus %>% unnest_tokens(ngram, text, token = "ngrams", n = 1) %>%
        semi_join(voc, by = c("ngram"="word")) 
#decreases the voc size
voc <- tibble(word = unique(unigram$ngram))
#OOV 1% of the least likely unigrams
unigramcount <- unigram %>% count(ngram)
unk <- unigramcount %>%
        filter(n==1) %>%
        slice_sample(n = mean(unigramcount$n)) 
unigram[unigram$ngram %in% unk$ngram,]$ngram <- "<unk>"
corpus <- unigram %>%
        group_by(line) %>%
        summarise(line = paste(ngram, collapse = " ")) %>%
        rename("text" = "line") %>%
        mutate("line" = row_number())
saveRDS(corpus, "corpus10.rds")
rm(unigramcount)  


#
#
#
setwd("~/R/Capestone")
corpus <- readRDS("corpus10.rds")

bigram <- corpus %>% 
        unnest_tokens(ngram, text, token = "ngrams", n = 2) %>%
        separate(ngram, c("word1", "word2"), sep = " ") %>%
        count(word1, word2) %>%
        na.omit()
saveRDS(bigram, "bigram10.rds")

trigram <- corpus %>% 
        unnest_tokens(ngram, text, token = "ngrams", n = 3) %>%
        separate(ngram, c("word1", "word2", "word3"), sep = " ") %>%
        count(word1, word2, word3) %>%
        na.omit()
saveRDS(trigram, "trigram10.rds")

tetragram <- corpus %>% 
        unnest_tokens(ngram, text, token = "ngrams", n = 4) %>%
        separate(ngram, c("word1", "word2", "word3", "word4"), sep = " ") %>%
        count(word1, word2, word3, word4) %>%
        na.omit()
saveRDS(tetragram, "tetragram10.rds")

pentagram <- corpus %>% 
        unnest_tokens(ngram, text, token = "ngrams", n = 5) %>%
        separate(ngram, c("word1", "word2", "word3", "word4", "word5"), sep = " ") %>%
        count(word1, word2, word3, word4, word5) %>%
        na.omit()
saveRDS(pentagram, "pentagram10.rds")

hexagram <- corpus %>% 
        unnest_tokens(ngram, text, token = "ngrams", n = 6) %>%
        separate(ngram, c("word1", "word2", "word3", "word4", "word5", "word6"), sep = " ") %>%
        count(word1, word2, word3, word4, word5, word6) %>%
        na.omit()
saveRDS(hexagram, "hexagram10.rds")

