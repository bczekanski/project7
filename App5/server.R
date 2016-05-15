library(ggplot2)
library(project7)
library(dplyr)
library(ggpmisc)
library(gganimate)
shinyServer(function(input, output) {
  output$scatterPlot <- renderPlot({

    t <- r %>%
     filter(j.ret == input$n)

    c <- t %>%
      filter(prev.ret.rank == 10, prev.vol.rank == 1) %>%
      ggplot(aes(k.ret, future.returns)) +
      geom_abline(color = "white") +
      geom_jitter (aes(color = future.vol)) +
      scale_colour_gradientn(limits = c(0.0000001, .5), colours=c("#372EFF","#FFFFFF","#FD3206"), trans = "log", values = c(0, .75, 1)) +
      ggtitle("Low Volume Winners") +
      xlab("Time Since Measurement (Months)") +
      ylab("Average Forward Return (Percent)") +
      labs(color = "Average FForward Volume") +
      xlim(0, 13) +
      ylim(-100, 100) -> p1

    #c(0.00001, .1)

    d <- t %>%
      filter(prev.ret.rank == 1, prev.vol.rank == 3) %>%
      ggplot(aes(k.ret, future.returns)) +
      geom_jitter (aes(color = future.vol)) +
      scale_colour_gradientn(limits = c(0.0000001, .5), colours=c("#372EFF","#FFFFFF","#FD3206"), trans = "log", values = c(0, .75, 1)) +
      ggtitle("High Volume Losers") +
      xlab("Time Since Measurement (Months)") +
      ylab("Average Forward Return (Percent)") +
      labs(color = "Average Forward Volume") +
      xlim(0, 13) +
      ylim(-100, 100) -> p2


     a <- t %>%
       filter(prev.ret.rank == 1, prev.vol.rank == 1) %>%
      ggplot(aes(k.ret, future.returns)) +
      geom_abline(color = "white") +
      geom_jitter (aes(color = future.vol)) +
      scale_colour_gradientn(limits = c(0.0000001, .5), colours=c("#372EFF","#FFFFFF","#FD3206"), trans = "log", values = c(0, .75, 1)) +
      ggtitle("Low Volume Losers") +
      xlab("Time Since Measurement (Months)") +
      ylab("Average Forward Return (Percent)") +
      labs(color = "Average Forward Volume") +
      xlim(0, 13) +
      ylim(-100, 100) -> p3

     b <- t %>%
      filter(prev.ret.rank == 10, prev.vol.rank == 3) %>%
      ggplot(aes(k.ret, future.returns, frame = j.ret)) +
      geom_abline(color = "white") +
      geom_jitter (aes(color = future.vol)) +
      scale_colour_gradientn(limits = c(0.0000001, .5), colours=c("#372EFF","#FFFFFF","#FD3206"), trans = "log", values = c(0, .75, 1)) +
      ggtitle("High Volume Winners") +
       xlab("Time Since Measurement (Months)") +
      ylab("Average Forward Return (Percent)") +
      labs(color = "Average Forward Volume") +
      xlim(0, 13) +
      ylim(-100, 100) -> p4



       multiplot(p4, p1, p2, p3, cols=2)

       #gg_animate(p4)


    # shinyAppDir("App5")

  })

})
