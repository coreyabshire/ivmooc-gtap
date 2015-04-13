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

# Export for 2 region2 and main commodities (example: China (chn) and usa)
twoRegionsMainCommodities <- function(region1, region2, comms) {
  byExp <- trade %>%
    filter(exp == region1 | exp == region2) %>%
    filter(comm %in% comms) %>%
    group_by(comm, year, exp) %>%
    summarise(commval = sum(value))
  byExp$comm <- factor(byExp$comm, levels = comms) # order by top worldtraded commodities
  qplot(year, commval, data = byExp, facets = . ~ comm, col = exp, geom = "line") + 
    facet_wrap(~ comm, ncol=5) +
    ggtitle(paste("Export of World Top Traded Commodities by", region1, "and", region2))
}

topComms = Sum_x_Comm[1:15,][[1]]
twoRegionsMainCommodities("chn", "usa", topComms)
