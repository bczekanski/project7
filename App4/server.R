library(ggplot2)
library(project7)
library(dplyr)
shinyServer(function(input, output) {
  output$scatterPlot <- renderPlot({
 t <- w %>%
  filter(
    j.ret == input$n,
    k.ret == input$m,
    price >= min(input$range),
    price <= max(input$range),
    cap.usd >= min(input$size),
    cap.usd <= max(input$size)
    )

 if (input$e0 == "All"){}
 else{
   t <- t %>%
   filter(symbol == input$e0)
 }

uniques <- unique(w$symbol)
   t %>%
    ggplot(aes(prev.returns, future.returns)) +
      geom_point (aes(color = prev.vol)) +
      scale_colour_gradientn(colours=c("#372EFF","#F9FF2E","#FD3206"), trans = "log") +
      ggtitle("Price Momentum and Trading Volume") +
      geom_smooth (method = lm, show.legend = TRUE) +
      xlim(-75, 100) +
      ylim(-100, 600)
      #scale_x_log10() +
      #scale_y_log10()
      #lock scales
      # filter by cap, price, sector
  # Display the slope???
    # shinyAppDir("App4")
    #output$out1 <- renderPrint(input$in1)
    })

})
