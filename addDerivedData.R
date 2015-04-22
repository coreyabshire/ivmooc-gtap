# Enrich the basic trade data with derivative data:
#
#  1. Sum Export per Exporting Region per Year
#  2. Sum Total Export over full period per Region
#  3. Sum Import per Improting Region per Year
#  4. Sum Total Import over full period per Region
#  5. Sum total Trade for a commodity per Year
#  6. Sum total Trade per Commodity over full period
#  7. Create tradeplus dataset with added variables
#       - market: market share for an exporting region for a commodity in a year
#       - weigth: export of commodity vs full export of that region in that year
#  8. Top exported commmodities for a region
#  9. Top partners for exporting region
# 10. Sectors - longer description for commodities

library(dplyr)
load("trade.clean")

# 1.  Sum Export per Exporting Region per Year (all commodities)
Sum_x_ExpYear <- trade %>%
  group_by(exp, year) %>%
  summarise(commval = sum(value)) %>%
  arrange(commval)

# 2.  Sum Total Export over full period per Region
Sum_x_Exp <- Sum_x_ExpYear %>%
  group_by(exp) %>%
  summarise(commval = sum(commval)) %>%
  arrange(desc(commval))

# 3.  Sum Import per Importing Region per Year (all commodities)
Sum_x_ImpYear <- trade %>%
  group_by(imp, year) %>%
  summarise(commval = sum(value)) %>%
  arrange(commval)

# 4.  Sum Total Import over full period per Region
Sum_x_Imp <- Sum_x_ImpYear %>%
  group_by(imp) %>%
  summarise(commval = sum(commval)) %>%
  arrange(desc(commval))

# 5.  Sum total Trade for a commodity per Year
Sum_x_CommYear <- trade %>%
  group_by(comm, year) %>%
  summarise(commval = sum(value)) %>%
  arrange(commval)

# 6.  Sum total Trade per Commodity over full period
Sum_x_Comm <- Sum_x_CommYear %>%
  group_by(comm) %>%
  summarise(commval = sum(commval)) %>%
  arrange(desc(commval))

# 7.  Create tradeplus dataset with added variables
#       - market: market share for an exporting region for a commodity in a year
#       - weigth: export of commodity vs full export of that region in that year

# add market
tradeplus = left_join(trade, Sum_x_CommYear)
tradeplus = rename(tradeplus, market = commval)
tradeplus$market = tradeplus$value / tradeplus$market
# add weight
tradeplus = left_join(tradeplus, Sum_x_ExpYear)
tradeplus = rename(tradeplus, weight = commval)
tradeplus$weight = tradeplus$value / tradeplus$weight
# Save to trade.plus
save(tradeplus, file="trade.plus")

# 8.  Top exported commodities for a region

topTrades <- function(region, number = 5) {
  t = tradeplus %>% 
    filter(exp == region) %>%
    group_by(comm) %>%
    summarise(commval = sum(value)) %>%
    arrange(desc(commval))
  totExport = sum(t$commval)
  t$weight = t$commval / totExport
  t[1:number,][[1]]
}

topTrades("pol")
topTrades("bel", 7)

# 9. Top partners for exporting region

topPartners <- function(region, number = 5) {
  t = tradeplus %>% 
    filter(exp == region) %>%
    group_by(imp) %>%
    summarise(commval = sum(value)) %>%
    arrange(desc(commval))
  totExport = sum(t$commval)
  t$weight = t$commval / totExport
  t[1:number,][[1]]
}

topPartners("usa")
topPartners("bel", 10)

# 10 Sectors - long names for commdities
sectors <- read.csv("~/Desktop/IVMOOC/FinalProject/sectors.csv")
sectors$Code = tolower(sectors$Code)