library(ggplot2)
library(dplyr)

load("trade.plus")

export4region <- function(region, comms = topTrades(region)) {
    tradeplus$value = tradeplus$value / 1000
    reg = tradeplus %>% 
        filter(exp == region & comm %in% comms) %>% 
        group_by(exp,comm,year) %>% 
        summarise(weight = sum(weight), value = sum(value))
    qplot(year, weight, col=comm, size=value, data=reg) +
      geom_line() +
      ggtitle(paste("export for ", region)) +
      theme(plot.title=element_text(face="bold", size=35), 
            axis.text = element_text(size=25),
            axis.title = element_text(size=25),
            legend.text = element_text(size=15))
}

export4region("usa")
export4region("chn")
export4region("aus")
