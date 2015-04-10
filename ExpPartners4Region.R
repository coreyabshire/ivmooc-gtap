library(ggplot2)
library(dplyr)

load("trade.plus")

exportPartners4region <- function(region, partners = topPartners(region)) {
  reg = tradeplus %>% 
    filter(exp == region & imp %in% partners) %>% 
    group_by(exp,imp,year) %>% 
    summarise(weight = sum(weight), value = sum(value))
  qplot(year, weight, col=imp, size=value, data=reg) +
    geom_line() +
    ggtitle(paste("export partners for ", region))
}

exportPartners4region("usa")
