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
            h5("The next predicted word is :"),
            #textOutput("predict"),
            h5("This predicition is based on an n-gram off length:"),
            textOutput("nlen"),
            verbatimTextOutput("MLE")
        ),

        # Show a plot of the generated distribution
        mainPanel(
                h1("Predicted text:"),
                textOutput("predict"),
                h5("The following graph represnts the distribution of predicted words
                   based on a bag of words created by a corpus created from the swiftkey
                   data. The counts (n) represent the likleyness of that word being correct."),
                plotOutput("distPlot"),
                h1("App Documentation:"),
                h5("This model is based on the stupid back-off model. The highest
                   n-gram is used based on the maximum length of the text provided
                   and the maximum ngram supplied. The model automatically backs-off
                   to a lower ngram if no match is found. ")
        )
    )
))
