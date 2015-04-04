library(ggplot2)
library(dplyr)

load(trade.plus)

export4region <- function(region, comms = topTrades(region)) {
    reg = tradeplus %>% 
        filter(exp == region & comm %in% comms) %>% 
        group_by(exp,comm,year) %>% 
        summarise(weight = sum(weight), market = sum(market))
    qplot(year, weight, col=comm, size=market, data=reg) +
      geom_line() +
      ggtitle(paste("export for ", region))
}

export4region("usa")
export4region("chn")
export4region("aus")
