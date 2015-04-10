library(ggplot2)
library(dplyr)

load("trade.plus")

bilatTrade <- function(region1, region2) {
  bilat = tradeplus %>% 
    filter((exp == region1 & imp == region2) | (exp == region2 & imp == region1)) %>% 
    group_by(exp,imp,year,comm) %>% 
    summarise(weight = sum(weight), value = sum(value))
  qplot(year, value, data=bilat, col=comm, geom="line") + facet_grid(. ~ exp) +
    ggtitle("Bilateral Trade")
}

bilatTrade("usa", "deu")

tradeBalance <- function(region1, region2) {
  bilat = tradeplus %>% 
    filter((exp == region1 & imp == region2) | (exp == region2 & imp == region1)) %>% 
    group_by(exp,imp,year) %>% 
    summarise(weight = sum(weight), value = sum(value))
  qplot(year, value, col=exp, data=bilat, geom="line") + ggtitle("Bilateral Trade")
}

tradeBalance("usa", "deu")
