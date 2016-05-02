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
      scale_colour_gradientn(colours=c("#372EFF","#F9FF2E","#FD3206"), trans = "log") +
      geom_smooth (method = lm) #+
      #scale_x_log10() +
      #scale_y_log10()
      #lock scales
      # filter by cap, price, sector
  # Display the slope???
    # shinyAppDir("App4")
    })

})
