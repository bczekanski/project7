library(ggplot2)
library(project7)
library(dplyr)
library(ggpmisc)
shinyServer(function(input, output) {
  output$scatterPlot <- renderPlot({

    t <- w %>%
      filter(
        j.ret == input$n,
       #k.ret == input$m,
       price >= min(input$range),
       price <= max(input$range),
       cap.usd >= min(input$size),
       cap.usd <= max(input$size),
       prev.vol >= min(input$volume),
       prev.vol <= max(input$volume),
        prev.ret.rank == input$prev.ret.rank
      )

    if (input$ticker == "All"){}
    else{
      t <- t %>%
        filter(symbol == input$ticker)
    }

    if (input$industry == "All"){}
    else{
      t <- t %>%
        filter(m.ind == input$industry)
    }

    if (input$sector == "All"){}
    else{
      t <- t %>%
        filter(m.sec == input$sector)
    }




    t %>%
      ggplot(aes(k.ret, future.returns)) +
      geom_abline(color = "white") +
      geom_jitter (aes(color = future.vol)) +
      scale_colour_gradientn(limits = c(0.00001, .1), colours=c("#372EFF","#FFFFFF","#FD3206"), trans = "log", values = c(0, .75, 1)) +
      ggtitle("Price Momentum and Trading Volume") +
      geom_smooth (method = lm, show.legend = TRUE, formula  = y ~ x) +
      # stat_poly_eq(formula = y~x,
      #              eq.with.lhs = "italic(h)~`=`~",
      #              eq.x.rhs = "~italic(z)",
      #              aes(label = ..eq.label..),
      #              parse = TRUE) +
      xlim(0, 13) +
      ylim(-100, 100) # +
    #scale_x_log10() #+
    #scale_y_log10()
    #372EFF",
    #lock scales
    # filter by cap, price, sector
    # Display the slope???
    # shinyAppDir("App4")
    #output$out1 <- renderPrint(input$in1)

    ## filter by previous decile
  })

})
