#' Plot 14C Spectrum
#'
#' \code{plotSpecSum} returns html plot of 14C spectrum.
#' A plot for the number of measurements is output.
#'
#' @param logdata A list generated with \code{readRawdata}.
#'
#' @importFrom magrittr %>%
#' @importFrom plotly plot_ly
#' @importFrom plotly layout
#' @importFrom plotly add_segments
#'
#' @export
#'

plotSpecSum <- function(logdata) {
  for (i in seq(logdata$property$run_num)) {
    w <- logdata[[i + 1]]$CONDITION[[1]]$C14_window
    s <- apply(logdata[[i + 1]]$MASTER_SPECTRUM, 1, sum)
    plotly::plot_ly(x = 0:2047, y = s,
                          type = "scatter",
                          mode = "lines",
                          color = I("grey0"),
                          name = "detector count") %>%
      plotly::layout(title = paste(names(logdata)[i + 1], "14C spectrum"),
                     xaxis = list(title = "sample point"),
                     yaxis = list(title = "detector count")) %>%
      plotly::add_segments(x = w[1], xend = w[1], y = 0, yend = max(s),
                           color = I("dodgerblue2"),
                           line = list(width = 1),
                           name = "14C window lower limit") %>%
      plotly::add_segments(x = w[2], xend = w[2], y = 0, yend = max(s),
                           color = I("brown1"),
                           line = list(width = 1),
                           name = "14C window upper limit") %>%
    print(p)
  }
}
