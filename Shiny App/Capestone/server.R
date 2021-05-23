library(shiny)
library(tidyverse)
library(tidytext)

shinyServer(function(input, output) {
        #Initialization
        ngrams <- readRDS("ngrams.rds")
        cont.prop <- readRDS("cont.prop.rds")
        maxn <- length(ngrams)-1
        voc <- tibble(word = ngrams$one$ngram)
        #cleans up input
        truetext <- reactive({
                truetext <- input$text %>%
                        tibble(text=.) %>%
                        unnest_tokens(word, text, token="ngrams", n=1)
                truetext[!truetext$word %in% voc$word,] <- "unk"
                truetext
        })
        
        maxuse <- reactive({
                min(nrow(truetext()) + 1,maxn)
                })

        
        MLE <- reactive({
                text <- truetext()
                n <- maxuse() 
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
                ngrams.tbl
        })
        nlen <- reactive({sum(map(MLE(), nrow)>0)})
        output$nlen <- nlen
        output$predict <- renderText({
                ans <- MLE()[nlen()][[1]]
                ans[1,length(ans)-1] %>% as.character()
                })

        output$MLE <- renderPrint({MLE()[nlen()]})
        
        dist <- reactive({
                #The distribution tb is setup from the previous counts and the bag
                #of words in the vocabulary
                dist <- tibble(word=voc$word)
                counts <- MLE()[nlen()][[1]] %>%
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
                KNS <- KNS %>% add_column(cont_prop = cont.prop[nlen()][[1]]$prop*cont.wt)
                #if the continuity probability is 0, use the lower continuity probability
                #use the same weight to decrease it even more
                #will require to normalize
                for (i in 1: (nlen() - 1)) {
                        index <- rev((nlen() - 1): 1)
                        KNS$cont_prop[KNS$cont_prop == 0] <- cont.prop[index][[1]]$prop[KNS$cont_prop == 0]*cont.wt^(i)}
                
                KNS$cont_prop <- KNS$cont_prop/sum(KNS$cont_prop)
                KNS <- KNS %>% mutate(KNS = KNS + cont_prop, .keep = "unused")
                
                dist <- dist %>% add_column(KNS = KNS$KNS)
                
                #Max values from models
                maxes <- dist %>% 
                       summarise_at(-1, which.max) %>%
                       flatten() %>%
                       unlist
                dist %>% rename(Add_One = ADD, Good_Turing = GT2) %>%
                        relocate(c(Add_One, ADI, Good_Turing, KNS, MLE), .after = n)
        })
        output$dist <- reactive({
                dist()
                })
        
        #Draws the data table that summarize the probabilities
        output$maxes <- renderTable({
                maxes <- dist() %>%
                        filter(!word == "<unk>") %>%
                        summarise_at(c(-1,-2), which.max) %>%
                        flatten() %>%
                        unlist
                dist()[maxes,]
        },
        digits = 4, striped = TRUE, align = "c", hover = TRUE)
        output$plotpredict <- renderPlot({
                tb <- dist()
                tb2 <- tibble(word = rep.int(tb$word,(length(tb)-2)), 
                              model = c(rep_len("MLE", nrow(tb)),
                                        rep_len("Add_one", nrow(tb)),
                                        rep_len("Good_Turing", nrow(tb)),
                                        rep_len("ADI", nrow(tb)),
                                        rep_len("KNS", nrow(tb))),
                              prob = c(tb$MLE, tb$Add_One, tb$Good_Turing,
                                       tb$ADI, tb$KNS))
                tb2 %>% filter(!word=="<unk>") %>%
                        mutate(bin = substr(word,1,1)) %>% 
                        group_by(bin) %>%
                        ggplot(aes(x=bin, y= prob)) +
                        facet_wrap(~model)+
                        geom_col(fill = "steelblue") + 
                        coord_flip() + 
                        theme_grey() +
                        scale_x_discrete(limits=rev)
        })

})

