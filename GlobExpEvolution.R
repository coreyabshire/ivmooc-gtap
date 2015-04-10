#
#
#   Plot evolution of global export over full period  
#
#

library(ggplot2)
library(dplyr)

load("trade.plus")

# 30 biggest exporting countries = 83.5% total global export
topRegions = Sum_x_Exp[1:30,][[1]]
df = Sum_x_ExpYear %>% filter(exp %in% topRegions)
qplot(year, commval, data = df, color = exp, geom = "line") +
  ggtitle("30 biggest exporting countries = 83.5% total global export")

# 20 biggest commodities exported = 93.5% total global export
topComms = Sum_x_Comm[1:20,][[1]]
df = Sum_x_CommYear %>% filter(comm %in% topComms)
qplot(year, commval, data = df, color = comm, geom = "line") +
  ggtitle("20 biggest commodities exported = 93.5% total global export")
