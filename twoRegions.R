library(ggplot2)
library(dplyr)

load("trade.plus")

## @TODO top5 commodities for each region
twoRegions <- function(region1, region2) {
  bilat = tradeplus %>% 
    filter((exp == region1) | (exp == region2)) %>% 
    group_by(exp,year,comm) %>% 
    summarise(weight = sum(weight), value = sum(value))
  qplot(year, value, data=bilat, col=comm, geom="line") + facet_grid(. ~ exp) +
    ggtitle("Compare 2 exporting regions")
}

twoRegions("chn", "usa")