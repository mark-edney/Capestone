#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Text prediction Modeling"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            textInput("text", "Enter Text",""),
            h5("This predicition is based on an n-gram off length:"),
            textOutput("nlen")
        ),

        # Show a plot of the generated distribution
        mainPanel(
                h1("Predicted text:"),
                h5("The predicted word for each algorithm is summarized in the following
                   data table: "),
                tableOutput("maxes"),
                h5("The following graph represnts the distribution of predicted words
                   based on a bag of words created by a corpus created from the swiftkey
                   data. The counts (n) represent the likeliness of that word being correct."),
                plotOutput("plotpredict"),
                h1("App Documentation:"),
                h5("This model is based on the stupid back-off model. The highest
                   n-gram is used based on the maximum length of the text provided
                   and the maximum ngram supplied. The model automatically backs-off
                   to a lower ngram if no match is found. For this Application, the
                   highest n-gram is of length 7. The ngrams are however more heavly 
                   weighted to lower length ngrams. Lower length n-grams require 
                   less storage space and have higher application."),
        )
    )
))
