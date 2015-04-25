library(ggplot2)
library(dplyr)

load("trade.plus")

exportPartners4region <- function(region, partners = topPartners(region)) {
  tradeplus$value = tradeplus$value / 1000
  reg = tradeplus %>% 
    filter(exp == region & imp %in% partners) %>% 
    group_by(exp,imp,year) %>% 
    summarise(weight = sum(weight), value = sum(value))
  qplot(year, weight, col=imp, size=value, data=reg) +
    geom_line() +
    ggtitle(paste("export partners for", region)) +
    theme(plot.title=element_text(face="bold", size=35), 
          axis.text = element_text(size=25),
          axis.title = element_text(size=25),
          legend.text = element_text(size=15))
}

exportPartners4region("chn")
exportPartners4region("usa")

