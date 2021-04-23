blogs <- readLines("~/R/Capestone/Coursera-SwiftKey/final/en_US/en_US.blogs.txt")
news <- readLines("~/R/Capestone/Coursera-SwiftKey/final/en_US/en_US.news.txt")
twitter <- readLines("~/R/Capestone/Coursera-SwiftKey/final/en_US/en_US.twitter.txt")
maxblogs <- max(nchar(blogs))
maxnews <- max(nchar(news))
maxtwitter <- max(nchar(twitter))
lovehate <- sum(grepl("love",twitter))/sum(grepl("hate", twitter))
bio <- twitter[grepl("biostats",twitter)]
tweet <- sum(grepl("A computer once beat me at chess, but it was no match for me at kickboxing", twitter))
