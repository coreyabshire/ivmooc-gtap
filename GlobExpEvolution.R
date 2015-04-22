#
#
#   Plot evolution of global export + trading balance over full period  
#         - for 30 biggest exporting regions
#         - for 20 biggest exported commodities

library(ggplot2)
library(dplyr)
library(reshape2)

load("trade.plus")

# 30 biggest exporting countries = 83.5% total global export
topRegions = Sum_x_Exp[1:30,][[1]]
df = Sum_x_ExpYear %>% filter(exp %in% topRegions)
colnames(df)[3] <- "exported_value"
df$exported_value = df$exported_value / 1000
df2 = dcast(df, year ~ exp, value.var = "exported_value")
dfmin = subset(df, exp != "chn" & exp != "usa")
ggplot(df2, aes(x=year)) +
  geom_line(data = dfmin, aes(x=year, y=exported_value, col = exp, alpha = 0.1), size=2) +
  geom_text(label = "China", x = 2005, y = 1200, colour = "red", size = 10) +
  geom_line(aes(y=chn), col="red", size=3) +
  geom_text(label = "USA", x = 1997, y = 750, colour = "blue", size = 10) +
  geom_line(aes(y=usa), col= "blue", size=3) +
  ggtitle("30 biggest exporting countries = 83.5% total global export") +
  theme(plot.title=element_text(face="bold", size=25), 
        axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.position="none")

# Trading Balance for the 30 biggest exporting countries
# Import for the 30 biggest exporting countries
dfImp = Sum_x_ImpYear %>% filter(imp %in% topRegions)
df <- left_join(df, dfImp, by = c("exp" = "imp", "year" = "year"))
colnames(df)[4] = "imported_value"
df$imported_value = df$imported_value / 1000
# trade balance
df$balance = df$exported_value - df$imported_value
df3 = dcast(df, year ~ exp, value.var = "balance")
dfmin = subset(df, exp != "chn" & exp != "usa")
# qplot(year, balance, data = df, color = exp, geom = "line") +
ggplot(df3, aes(x=year)) +
  geom_line(data = dfmin, aes(x=year, y=balance, col = exp, alpha = 0.1), size=2) +
  geom_text(label = "China", x = 2005, y = 400, colour = "red", size = 10) +
  geom_line(data=df3, aes(y=chn), col="red", size=3) +
  geom_text(label = "USA", x = 1997, y = -500, colour = "blue", size = 10) +
  geom_line(data=df3, aes(y=usa), col= "blue", size=3) +
  ggtitle("Trading balance 30 biggest exporting countries") +
  theme(plot.title=element_text(face="bold", size=25), 
        axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.position="none")


# 20 biggest commodities exported = 93.5% total global export
topComms = Sum_x_Comm[1:20,][[1]]
df = Sum_x_CommYear %>% filter(comm %in% topComms)
qplot(year, commval, data = df, color = comm, geom = "line") +
  ggtitle("20 biggest commodities exported = 93.5% total global export")
