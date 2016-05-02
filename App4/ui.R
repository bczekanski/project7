shinyUI(fluidPage(

  titlePanel("Conditional panels"),

  column(2, wellPanel(
    sliderInput("m", "Previous Period",
                min = 3, max = 36, value = 3, step = 3)
  )),

  column(2, wellPanel(
    sliderInput("n", "Future Period",
                min = 3, max = 36, value = 3, step = 3)
  )),

  column(5,
         "Words Here",

         # With the conditionalPanel, the condition is a JavaScript
         # expression. In these expressions, input values like
         # input$n are accessed with dots, as in input.n
         conditionalPanel("input.n >= 0",
                          plotOutput("scatterPlot", height = 800, width = 800)
         )
  )
))
