#' Plot Technical Analysis Tools
#'
#' Read data and plot candlesticks
#' Calculate Simple Moving Average, Bollinger Bands, and Relative Strength Index and
#' plot them
#'
#' @param data A compulsory input, only allows CSV file or xts variable, columns must be arranged in the order of: Date, Open, High, Low, Close, Volume.
#' @param RSIperiod A number (optional, defaults to 14) indicating period of Relative Strength Index.
#' @param MAperiod A number (optional, defaults to 20) indicating period of Moving Average.
#' @param bbandperiod A number (optional, defaults to 20) indicating period of Bollinger Bands.
#' @param bbandsd A number (optional, defaults to 2) indicating standard deviation of Bollinger Bands.
#' @param bbandcolor Color (optional) that you would like to draw Bollinger Bands.
#' @param macolor Color (optional) that you would like to draw Moving Average.
#' @param rsicolor Color (optional) that you would like to draw Relative Strength Index.
#' @param hovercolor Color (optional) that you would like to show when hovering over candlesticks.
#'
#' There will be warnings while plotting with plotly, however the plotting still works nicely without any problem
#'

plotTA <- function(data, RSIperiod = 14, MAperiod = 20,
                   bbandperiod = 20, bbandsd = 2,
                   bbandcolor="#737373", macolor="#33bbff",
                   rsicolor="#000000", hovercolor="#00F5FF"){
  library(TTR)
  library(quantmod)
  library(plotly)
  if ((is.character(data)) && (substr(data,nchar(data)-4+1,nchar(data))==".csv"))
  {
    prices <- read.csv(data,header=TRUE)
    prices$Index <- as.Date(prices$Index)
  }
  else if (is.xts(data))
  {
    prices <-data.frame(Index = index(data),
                        Open = as.numeric(data[,1]),
                        High = as.numeric(data[,2]),
                        Low = as.numeric(data[,3]),
                        Close = as.numeric(data[,4]),
                        Volume = as.numeric(data[,5]))
  }
  else
  {
    warning("Please input the correct data structure: .csv or xts")
  }

  names(prices) <- c("Date","Open","High","Low","Close","Volume")

  bbands_line <- BBands(prices[,c("High","Low","Close")],n=bbandperiod,sd=bbandsd)
  rsi_line <- RSI(prices$Close,n=RSIperiod)
  sma_line <- SMA(prices$Close,n=MAperiod)

  prices <- cbind(prices,data.frame(bbands_line[,c(1,3)],rsi_line,sma_line))

  rangeselectorlist <- list(
    x = 0, y = 0.9,
    bgcolor = "#0099cc",
    font = list(color = "white"),

    buttons = list(
      list(count = 1, label = "reset", step = "all"),
      list(count = 1, label = "1yr", step = "year", stepmode = "backward"),
      list(count = 3, label = "3 mo", step = "month", stepmode = "backward"),
      list(count = 1, label = "1 mo", step = "month", stepmode = "backward"),
      list(step = "all")
    )
  )

  hovertxt <- paste("Date: ", prices$Date, "
                    ",
                    "High: ", prices$High,"
                    ",
                    "Low: ", prices$Low,"
                    ",
                    "Open: ", prices$Open,"
                    ",
                    "Close: ", prices$Close)

  p <- prices %>%
    plot_ly(x = ~Date,
            open = ~Open, close = ~Close,
            high = ~High, low = ~Low, type="candlestick",
            name = "prices", hoverinfo = "none") %>%
    add_lines(y = ~sma_line , name = "Simple Moving Average (n=20)",
              line = list(width = 3, dash = "5px", color = macolor),
              legendgroup = "SMA",
              showlegend = TRUE, hoverinfo = "none") %>%
    add_lines(y = ~up , name = "Bollinger Bands (n=20, sd=2)",
              line = list(width = 1, dash = "5px", color = bbandcolor),
              fill = "tonexty", fillcolor = "rgba(194, 240, 240, 0.2)",
              legendgroup = "Bollinger Bands",
              showlegend = TRUE, hoverinfo = "none") %>%
    add_lines(y = ~dn, name = "Bollinger Bands (n=20, sd=2)",
              line = list(width = 1, dash = "5px", color = bbandcolor),
              fill = "tonexty", fillcolor = "rgba(194, 240, 240, 0.2)",
              legendgroup = "Bollinger Bands",
              showlegend = FALSE, hoverinfo = "none") %>%
    add_lines(y = ~High, opacity = 0, hoverinfo = "text",
              text = hovertxt, showlegend = F,
              line = list(color = hovercolor,width = 1)) %>%
    layout(xaxis = list(rangeslider = list(visible = FALSE),
                        rangeselector = rangeselectorlist),
           yaxis = list(title = "Prices"))

  pp <- prices %>%
    plot_ly(x=~Date, y=~rsi_line, type='scatter', mode='lines',
            name = "Relative Strength Index (n=14)",
            line = list(color = rsicolor, width = 2)) %>%
    layout(yaxis = list(title = "RSI"))

  subplot(p, pp, heights = c(0.7,0.2), nrows=2,
          shareX = TRUE, shareY = FALSE, titleY = TRUE) %>%
    layout(title = paste("Duration: ",prices$Date[1],"-",prices$Date[dim(prices)[1]]),
           legend = list(orientation = 'h', x = 0.5, y = 1,
                         xanchor = 'center', yref = 'paper',
                         font = list(size = 10),
                         bgcolor = 'transparent'))
}
