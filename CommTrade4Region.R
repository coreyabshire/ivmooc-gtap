library(ggplot2)
library(dplyr)

load("trade.plus")

export4region <- function(region, comms = topTrades(region)) {
    reg = tradeplus %>% 
        filter(exp == region & comm %in% comms) %>% 
        group_by(exp,comm,year) %>% 
        summarise(weight = sum(weight), value = sum(value))
    qplot(year, weight, col=comm, size=value, data=reg) +
      geom_line() +
      ggtitle(paste("export for ", region))
}

export4region("usa")
export4region("chn")
export4region("aus")
