library(tidyverse)
library(tidytext)
library(stopwords)


#downloads the corpus files, profanity filter and English dictionary

url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
url2 <- "https://www.freewebheaders.com/download/files/facebook-bad-words-list_comma-separated-text-file_2021_01_18.zip"
url3 <- "https://raw.githubusercontent.com/dwyl/english-words/master/words_alpha.txt"
if(dir.exists("~/R/Capestone/data/") == FALSE){
       dir.create("~/R/Capestone/data/")}

if(file.exists("~/R/Capestone/data/data.zip") == FALSE|
   file.exists("~/R/Capestone/data/prof.zip")==FALSE|
   file.exists("~/R/Capestone/data/diction.txt")==FALSE){
        download.file(url,destfile = "~/R/Capestone/data/data.zip")
        download.file(url2,destfile = "~/R/Capestone/data/prof.zip")
        download.file(url3,destfile = "~/R/Capestone/data/diction.txt")
        setwd("~/R/Capestone/data/")
        unzip("~/R/Capestone/data/prof.zip")
        unzip("~/R/Capestone/data/data.zip")
        setwd("~/R/Capestone")
}

#Opening the text files to store as a tibble

blog <- read_lines("~/R/Capestone/data/final/en_US/en_US.blogs.txt")
news <- read_lines("~/R/Capestone/data/final/en_US/en_US.news.txt")
twitter <- read_lines("~/R/Capestone/data/final/en_US/en_US.twitter.txt")
blog <- tibble(text = blog) 
news <- tibble(text = news)
twitter <- tibble(text = twitter)

prof <- read_lines("~/R/Capestone/data/facebook-bad-words-list_comma-separated-text-file_2021_01_18.txt")[15]
prof <- prof %>% str_split(", ") %>% flatten %>% unlist
prof <- tibble("word" = prof)

english <- read_lines("~/R/Capestone/data/diction.txt")
english <- tibble("word" = english[!english==""])

set.seed(90210)
corpus <- full_join(blog,twitter) %>% full_join(news) %>% 
        slice_sample(prop = 0.20) %>%
        mutate(line = row_number(), .before = "text")
#clean up ram
rm(blog,news,twitter)
voc <- english %>% anti_join(prof)

unigram <- corpus %>% unnest_tokens(ngram, text, token = "ngrams", n = 1) %>%
        semi_join(voc, by = c("ngram"="word")) %>% count(ngram)
#decreases the voc size
voc <- tibble(word = unigram$ngram)

#OOV 1% of the least likely unigrams
unks <- unigram[unigram$n==1,] %>% slice_sample(prop = 0.01)
unigram[unigram$ngram %in% unks$ngram,]$ngram <- "<unk>"
unigram <- count(unigram, ngram)

#creates hhe bigrams
bigram <- corpus %>% unnest_tokens(ngram, text, token = "ngrams", n = 2) %>%
        separate(ngram, c("word1", "word2")) %>%
        filter(word1 %in% voc$word, word2 %in% voc$word) 

bigram$word1[bigram$word1 %in% unks$ngram] <- "<unk>"
bigram$word2[bigram$word2 %in% unks$ngram] <- "<unk>"
bigram <- count(bigram, word1, word2)

#creates the trigrams
trigram <- corpus %>% unnest_tokens(ngram, text, token = "ngrams", n = 3) %>%
        separate(ngram, c("word1", "word2", "word3")) %>% 
        filter(word1 %in% voc$word, word2 %in% voc$word, word3 %in% voc$word) 
trigram$word1[trigram$word1 %in% unks$ngram] <- "<unk>"
trigram$word2[trigram$word2 %in% unks$ngram] <- "<unk>"
trigram$word3[trigram$word3 %in% unks$ngram] <- "<unk>"
trigram <- count(trigram, word1, word2, word3)

#Start of the model distributions
dist <- tibble(word = voc$word)
input <- tibble(text = c("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd",
                         "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his",
                         "I'd give anything to see arctic monkeys this",
                         "Talking to your mom has the same effect as a hug and helps reduce your",
                         "When you were in Holland you were like 1 inch away from me but you hadn't time to take a",
                         "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the",
                         "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each",
                         "Every inch of you is perfect from the bottom to the",
                         "I'm thankful my childhood was filled with imagination and bruises from playing",
                         "I like how the same people are in almost all of Adam Sandler's")) %>%
        mutate(line = row_number())
input <- unnest_tokens(input, word, text, token = "words")


w <- vector(length = 3)
w[1] <- input[length(input)-1]
w[2] <- input[length(input)]

#MLE for trigrams

temp <- trigram %>% filter(word1 == w[1]) %>% filter(word2 == w[2])
MLEdf <- tibble(word=temp$word3, MLE=temp$n/sum(temp$n))
dist <- left_join(dist, MLEdf)
dist[is.na(dist$MLE),]$MLE <- 0

#add one smoothing
ADDdf <- tibble(word=temp$word3, ADD=temp$n)
dist <- left_join(dist, ADDdf)
dist[is.na(dist$ADD),]$ADD <- 0
dist$ADD <- dist$ADD + 1
dist$ADD <- dist$ADD / sum(dist$ADD)

#good turning
Nr <- count(temp, n) %>% add_row(n = 0, nn = 0)  %>% 
        arrange(n)
Nr %<>% mutate(c= 0) %>% mutate(Gt = 0)
total <- sum(Nr$nn*Nr$n)
Nr$Gt[Nr$n==0] <- Nr$nn[Nr$n==1] / total
Nr$nn[Nr$n==0] <- nrow(dist) - total
for (i in 2:nrow(Nr)) {
        Nr$c[i] <-  (Nr$n[i]+1)*Nr$nn[i+1]/Nr$nn[i]
        Nr$c[i][is.na(Nr$c[i])] <- 0
        Nr$Gt[i] <-  Nr$c[i] / total
}
Nr$Gt[Nr$n==max(Nr$n)] <- 1 - sum(Nr$Gt)
Nr$Gt <- Nr$Gt/ Nr$nn
Gt <- tibble(word=voc$word)
Gt <- left_join(Gt, select(temp, word3, n), by = c("word" = "word3"))
Gt$n[is.na(Gt$n)] <- 0 
Gt <- left_join(Gt, select(Nr, n, Gt))
dist <- left_join(dist, select(Gt, word, Gt))

#Absolute Discounting
ADdf <- voc %>% left_join(select(temp, word3, n), by = c("word" = "word3")) 
ADdf$n[is.na(ADdf$n)] <- 0
d <- 0.5
temp2 <- trigram %>% filter(word1 == w[1]) %>% 
        count(word3) %>% 
        mutate(p3 = n / sum(n))

ADdf <- mutate(ADdf, p = n /sum(n), p1 = ((n-d) > 0) * (n-d) / sum(n),
               p2 = d/sum(n)* nrow(temp))
ADdf <- left_join(ADdf, select(temp2, word3, p3), by = c("word" = "word3" ))
ADdf$p3[is.na(ADdf$p3)] <- 0
ADdf <- mutate(ADdf, AD = p1+p2*p3)

#KN for bigrams
d <- 0.5
KN <- tibble(word = dist$word, P=0)
matched <- bigram[bigram$word1==w[2],] %>% select(-word1) %>% rename(word=word2)
KN <- KN %>% left_join(matched)
KN$n <- replace_na(KN$n, 0)
KN$P <- KN$n*(KN$n-d>0) / sum(KN$n)

#The best results by algorithm
dist %>% summarise_at(c("MLE", "ADD", "Gt"), max)

