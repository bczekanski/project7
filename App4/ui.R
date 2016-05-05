library(project7)
library(shiny)
shinyUI(fluidPage(

  titlePanel("Filtering Options"),

  sidebarLayout(
    sidebarPanel(

    sliderInput("m", "Previous Period (Months)",
                min = 3, max = 36, value = 3, step = 3),

    sliderInput("n", "Future Period (Months)",
                min = 3, max = 36, value = 3, step = 3),

    sliderInput("range", "Price Range:",
                min = 1, max = 10000, value = c(0,10000)),

    sliderInput("size", "Market Cap Range:",
                min = 1, max = 10000000000, value = c(0,10000000000)),

    selectInput(
      "e0", "Ticker", choices = c("All", uniques), multiple = TRUE, selectize = TRUE)
    ),


    mainPanel(plotOutput("scatterPlot", height = 700, width = 1000)
               )
        )
))

