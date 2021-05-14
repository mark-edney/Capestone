library(shiny)
library(tidyverse)
library(tidytext)
library(pryr)

shinyServer(function(input, output) {
        #Initialization
        setwd("~/R/Capestone")
        ngrams <- readRDS("ngrams.rds")
        #ngrams$two$bin <- substr(ngrams$two$word2,1,1)
        #ngrams$twp$bin[ngrams$one$bin=="<"] <- "<unk>"
        voc <- tibble(word = ngrams$one$ngram)
        maxn <- length(ngrams)-1
        dist <- tibble(word=voc)

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
        output$predict <- renderPrint({
                ngram <- MLE()[nlen()]
        })

        output$MLE <- renderPrint({MLE()})

                #draw the distribution
        output$distPlot <- renderPlot({
                tb <- MLE()[[nlen()]]
                c.names <- colnames(tb)
                prediction<- c.names[length(c.names)-1]
                tb %>% ggplot(aes(y = n, x= !!ensym(prediction))) +
                       geom_col(fill = "steelblue") + 
                        coord_flip() + 
                        theme_grey() +
                       scale_x_discrete(limits=rev)

    })

})

