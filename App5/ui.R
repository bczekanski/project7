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

      sliderInput("prev.ret.rank", "previous  ret rank",
                  min = 1, max = 10, value = 2),

      sliderInput("range", "Price Range:",
                  min = 1, max = 100000, value = c(0,100000)),

      sliderInput("size", "Market Cap Range:",
                  min = 1, max = 100000000000, value = c(0,1000000000000)),

      sliderInput("volume", "Volume Range:",
                  min = 0.0000, max = 3, value = c(0, 3)),

      selectInput(
        "ticker", "Ticker", choices = c("All", unique.tickers), multiple = TRUE, selectize = TRUE)
      ,

      selectInput(
        "sector", "Sector", choices = c("All", unique.sectors), multiple = TRUE, selectize = TRUE)
      ,

      selectInput(
        "industry", "Industry", choices = c("All", unique.industries), multiple = TRUE, selectize = TRUE)

    ),


    mainPanel(plotOutput("scatterPlot", height = 700, width = 800)
    )
  )
))

