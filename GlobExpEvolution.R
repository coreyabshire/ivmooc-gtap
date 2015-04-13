#
#
#   Plot evolution of global export + trading balance over full period  
#         - for 30 biggest exporting regions
#         - for 20 biggest exported commodities

library(ggplot2)
library(dplyr)

load("trade.plus")

# 30 biggest exporting countries = 83.5% total global export
topRegions = Sum_x_Exp[1:30,][[1]]
df = Sum_x_ExpYear %>% filter(exp %in% topRegions)
colnames(df)[3] <- "expVal"
qplot(year, expVal, data = df, color = exp, geom = "line") +
  ggtitle("30 biggest exporting countries = 83.5% total global export")

# Trading Balance for the 30 biggest exporting countries
# Import for the 30 biggest exporting countries
dfImp = Sum_x_ImpYear %>% filter(imp %in% topRegions)
df <- left_join(df, dfImp, by = c("exp" = "imp", "year" = "year"))
colnames(df)[4] = "impVal"
# trade balance
df$balance = df$expVal - df$impVal
qplot(year, balance, data = df, color = exp, geom = "line") +
  ggtitle("Trading balance for the 30 biggest exporting countries")

# 20 biggest commodities exported = 93.5% total global export
topComms = Sum_x_Comm[1:20,][[1]]
df = Sum_x_CommYear %>% filter(comm %in% topComms)
qplot(year, commval, data = df, color = comm, geom = "line") +
  ggtitle("20 biggest commodities exported = 93.5% total global export")
