library(tidyverse)
library(tidytext)

#This code is used to test the models in the shiny application
setwd("~/R/Capestone")
ngrams <- readRDS("ngrams.rds")
cont.prop <- readRDS("cont.prop.rds")
maxn <- length(ngrams)-1
voc <- tibble(word = ngrams$one$ngram)
testset <- readRDS("testset.rds") %>%
        unnest_tokens(word, text, token="ngrams", n=1)
testset[!testset$word %in% voc$word,]$word <- "unk"


#maximum allowed ngram
maxuse <- function(testset){
        min(nrow(testset) + 1 ,maxn)
}

#Model Stupid-backoff
create.dist <- function(text, ngrams){
        n <- maxuse(text)
        text[!text$word %in% voc$word,] <- "unk"
        ngrams.tbl <- ngrams[1:n]
        for (i in 2:n){
                if(i>5){
                        ngrams.tbl[[i]] <- ngrams.tbl[[i]] %>%
                                filter(.[,i-5] == text$word[nrow(text)-4])}
                if(i>4){
                        ngrams.tbl[[i]] <- ngrams.tbl[[i]] %>%
                                filter(.[,i-4] == text$word[nrow(text)-3])}
                if(i>3){
                        ngrams.tbl[[i]] <- ngrams.tbl[[i]] %>%
                                filter(.[,i-3] == text$word[nrow(text)-2])}
                if(i>2){
                        ngrams.tbl[[i]] <- ngrams.tbl[[i]] %>%
                                filter(.[,i-2] == text$word[nrow(text)-1])}
                ngrams.tbl[[i]] <- ngrams.tbl[[i]] %>%
                        filter(.[,i-1] == text$word[nrow(text)]) %>%
                        arrange(desc(n))
        }
        nlen <- sum(map(ngrams.tbl, nrow)>1)
        ngrams.tbl[nlen]
        
        ###
        ##Model testing
        dist <- tibble(word=voc$word)
        counts <- ngrams.tbl[nlen][[1]] %>%
                select(ncol(.)-1,ncol(.)) %>%
                rename(word = colnames(.)[1])
        dist <- left_join(dist,counts, by = "word")
        dist$n[is.na(dist$n)] <- 0
        
        #The simplest model is the MLE based on the counts
        dist$MLE <- dist$n/sum(dist$n)
        
        #Add one smoothing strictly adds a count to every value, not
        #much better than MLE
        dist$ADD <- dist$n
        dist$ADD[dist$ADD==0] <- 1
        dist$ADD <- dist$ADD/sum(dist$ADD)
        
        #Good turing
        Nr <- count(counts, n, name = "nn") %>%
                add_row(n = 0, nn = 0) %>% 
                arrange(n) %>% 
                mutate(c= 0) %>%
                mutate(sc = 0) %>%
                mutate(GT = 0)
        total <- sum(Nr$nn*Nr$n)
        
        #the probability for unseen matches is set to the next value probability
        Nr$GT[Nr$n==0] <- Nr$nn[2]*Nr$n[2]/total
        Nr$nn[Nr$n==0] <- sum(dist$n==0)
        
        Zn <- Nr[-1,] %>% add_row(n=Nr$n[nrow(Nr)]+1)
        Zr <- Nr[-1,] %>% lm(log(nn)~log(n), data=.) %>% predict(newdata=Zn)
        Zr <- exp(Zr)
        #creates the new adjusted counts
        j <- 0 
        for (i in 2:nrow(Nr)) {
                Nr$c[i] <-  (Nr$n[i]+1)*Nr$nn[i+1]/Nr$nn[i]
                Nr$c[i][is.na(Nr$c[i])] <- 0
                Nr$sc[i] <-  (Nr$n[i]+1)*Zr[i]/Zr[i-1]
                if(Nr$n[i+1]-Nr$n[i] > 1 | i == nrow(Nr)){
                        j <- 1}
                Nr$GT[i] <-  Nr$c[i]*(1-j) + Nr$sc[i]*j
        }
        
        #the specific prop from words with the same count
        #Nr$GT[Nr$GT < 0] <- Nr$nn[2]/total
        Nr$GT <- Nr$GT/sum(Nr$GT)
        Nr$GT2 <- Nr$GT/Nr$nn

        
        dist <- dist %>%
                left_join(select(Nr,n,GT2), by = "n")
        
        #ADI
        discount <- 0.75
        ADI <- dist %>% select(word, n) %>% mutate(ADI = (n - discount)/sum(n))
        ADI$ADI[ADI$ADI < 0 ] <- 0
        unigram.prop <- ngrams$one %>% mutate(prop = n / sum(n))
        uni.wt <- 1 - sum(ADI$ADI)
        ADI <- ADI %>% add_column(uni = unigram.prop$prop*uni.wt) %>% 
                mutate(ADI = ADI + uni, .keep = "unused")
        dist <- dist %>% add_column(ADI =ADI$ADI)
        
        #KNS
        KNS <- dist %>% select(word, n) %>% mutate(KNS = (n - discount)/sum(n))
        KNS$KNS[KNS$KNS < 0 ] <- 0
        cont.wt <- 1 - sum(KNS$KNS)
        KNS <- KNS %>% add_column(cont_prop = cont.prop[nlen][[1]]$prop*cont.wt)
        #if the continuity probability is 0, use the lower continuity probability
        #use the same weight to decrease it even more
        #will require to normalize
        for (i in 1: (nlen - 1)) {
                index <- rev((nlen - 1): 1)
                KNS$cont_prop[KNS$cont_prop == 0] <- cont.prop[index][[1]]$prop[KNS$cont_prop == 0]*cont.wt^(i)}

        KNS$cont_prop <- KNS$cont_prop/sum(KNS$cont_prop)
        KNS <- KNS %>% mutate(KNS = KNS + cont_prop, .keep = "unused")
        
        dist <- dist %>% add_column(KNS = KNS$KNS)
        
        
        maxes <- dist %>% 
                summarise_at(-1, which.max) %>%
                flatten() %>%
                unlist
        
        dist <- dist %>% rename(Add_One = ADD, Good_Turing = GT2)
}

#The equation for perplexity
perplexity <- function(dist){
        MLE <- sum(log(dist$MLE))*(-1/nrow(dist))
        Add_One <- sum(log(dist$Add_One))*(-1/nrow(dist))
        Good_Turing <- sum(log(dist$Good_Turing))*(-1/nrow(dist))
        ADI <- sum(log(dist$ADI))*(-1/nrow(dist))
        KNS <- sum(log(dist$KNS))*(-1/nrow(dist))
        tibble(MLE = 2^MLE, Add_One = 2^Add_One, Good_Turing = 2^Good_Turing, ADI = 2^ADI, KNS = 2^KNS)
}


##for testing only

mdl.perplex <- tibble(MLE = numeric(), Add_One = numeric(), Good_Turing = numeric(),
              ADI = numeric(), KNS = numeric())
mdl.acc <- tibble(MLE_acc = numeric(), ADD_acc = numeric(), GT_acc = numeric(),
        ADI_acc = numeric(), KNS_acc = numeric())
temp <- NULL
##max(testset$line)
for (j in 1:max(testset$line)) {
        temp <- testset[testset$line==j,]
        for (i in 1 : (nrow(temp)-1)) 
                {
                temp2 <- temp[1:i,] %>%
                        create.dist(ngrams)
                word.correct <- tibble(
                        MLE_acc = as.numeric(temp2$word[which.max(temp2$MLE)]==temp[i+1,2]),
                        ADD_acc = as.numeric(temp2$word[which.max(temp2$Add_One)]==temp[i+1,2]),
                        GT_acc = as.numeric(temp2$word[which.max(temp2$Good_Turing)]==temp[i+1,2]),
                        ADI_acc = as.numeric(temp2$word[which.max(temp2$ADI)]==temp[i+1,2]),
                        KNS_acc = as.numeric(temp2$word[which.max(temp2$KNS)]==temp[i+1,2]))
                
                mdl.perplex <- mdl.perplex %>% add_row(temp2 %>% perplexity)
                mdl.acc <- mdl.acc %>% add_row(word.correct) }
}

saveRDS(mdl.perplex, "mdl.perplex.rds")
saveRDS(mdl.acc, "mdl.acc.rds")
