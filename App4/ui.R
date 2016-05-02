shinyUI(fluidPage(

  titlePanel("Conditional panels"),

  column(3, wellPanel(
    sliderInput("m", "Mumber of points:",
                min = 0, max = 36, value = 3, step = 3)
  )),

  column(4, wellPanel(
    sliderInput("n", "Number of points:",
                min = 0, max = 36, value = 3, step = 3)
  )),

  column(5,
         "Words Here",

         # With the conditionalPanel, the condition is a JavaScript
         # expression. In these expressions, input values like
         # input$n are accessed with dots, as in input.n
         conditionalPanel("input.n >= 0",
                          plotOutput("scatterPlot", height = 300)
         )
  )
))
