library(project7)
library(shiny)
shinyUI(fluidPage(
  verticalLayout(
  titlePanel("Price Momentum and Trading Volume"),

      sliderInput("n", "Previous Measurement Period (Months)",
                  min = 1, max = 12, value = 3, step = 1)
       ),

    mainPanel(plotOutput("scatterPlot", height = 600, width = 1000)
    )
  )
)

