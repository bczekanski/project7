library(ggplot2)
library(project7)
library(dplyr)
shinyServer(function(input, output) {
  output$scatterPlot <- renderPlot({
 t <- w %>%
  filter(j.ret == input$n, k.ret == input$m, symbol != "BRCMB", future.returns <= 600)
    t %>%
    ggplot(aes(prev.returns, future.returns)) +
      geom_point (aes(color = prev.vol)) +
      scale_colour_gradientn(colours=c("#372EFF","#F9FF2E","#FD3206")) +
      geom_smooth (method = lm)
  })

})