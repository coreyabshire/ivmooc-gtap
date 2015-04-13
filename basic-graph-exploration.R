library(dplyr)
library(ggplot2)

load("trade.clean")

# By commodity

byComm <- trade %>%
  group_by(comm, year) %>%
  summarise(commval = sum(value)) %>%
  filter(commval > 100000)

qplot(year, commval, data = byComm, color = comm) + geom_line()

qplot(year, commval, data = byComm, facets = . ~ comm) + geom_line()

# By exporting region

byExp <- trade %>%
  group_by(exp, year) %>%
  summarise(commval = sum(value)) %>%
  arrange(commval) %>%
  filter(commval > 1e5)

qplot(year, commval, data = byExp, color = exp) + geom_line()

qplot(year, commval, data = byExp, facets = . ~ exp) + geom_line()

# Export for a specific region for main commodities (example: China (chn))
byExpX <- trade %>%
  filter(exp == "chn" | exp == "usa") %>%
  filter(comm %in% topTrades("chn", 10)) %>%
  group_by(comm, year, exp) %>%
  summarise(commval = sum(value)) %>%
  arrange(commval)


qplot(year, commval, data = byExpX, facets = . ~ comm, col = exp, geom = "line") + ggtitle("Export by China")
  