#install.packages(c("plotly","magrittr"))
library(magrittr)
plotSpecSum <- function(logdata){
  for(i in seq(logdata$property$run_num)){
    w <- logdata[[i+1]]$CONDITION[[1]]$C14_window
    p <- plotly::plot_ly(data = logdata[[i+1]]$MASTER_SPECTRUM,
                          x = ~smp_point, y = ~sum,
                          type = "scatter",
                          mode = "lines",
                          name = "detector") %>%
          plotly::layout(title = paste(names(logdata)[i+1], "14C spectrum"),
                         xaxis = list(
                           title = "sample point"
                         ),
                         yaxis = list(
                           title = "detector count"
                         )) %>%
          plotly::add_segments(x = w[1], xend = w[1], y = 0, yend = max(logdata[[i+1]]$MASTER_SPECTRUM$sum),
                               color = I("black"),
                               line = list(width = 1),
                               name = "14C window lower limit") %>%
          plotly::add_segments(x = w[2], xend = w[2], y = 0, yend = max(logdata[[i+1]]$MASTER_SPECTRUM$sum),
                               color = I("black"),
                               line = list(width = 1),
                               name = "14C window upper limit")
    print(p)
  }
}



