library(ggplot2)
library(dplyr)

load("trade.plus")

tradeplus$value = tradeplus$value/1000
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
##########################################################################
twoRegionsMainCommodities <- function(region1, region2, comms) {
  trade$value = trade$value / 1000
  byExp <- trade %>%
    filter(exp == region1 | exp == region2) %>%
    filter(comm %in% comms) %>%
    group_by(comm, year, exp) %>%
    summarise(exported_value = sum(value)) %>% ungroup()

  byExpLongComm = left_join(byExp, sectors, by = c("comm" = "Code"))
  byExpLongComm$comm <- factor(byExpLongComm$comm, levels = comms) # order by top traded commodities
  qplot(year, exported_value, data = byExpLongComm, facets = . ~ Description, 
        col = exp, geom = "line", size=2) + 
    scale_colour_manual(values = c("chn" = "red","usa" = "blue")) +
    facet_wrap(~ Description, ncol=4) +
    ggtitle(paste("Export Top Traded Commodities by", region1, "and", region2)) +
    theme(plot.title=element_text(face="bold", size=25), 
          axis.text = element_text(size=15),
          strip.text = element_text(size=13),
          axis.title = element_text(size=20),
          legend.position="none")   
}

ggplot(df3, aes(x=year)) +
  geom_line(data = dfmin, aes(x=year, y=balance, col = exp, alpha = 0.1), size=2) +
  geom_text(label = "China", x = 2005, y = 400, colour = "red", size = 10) +
  geom_line(data=df3, aes(y=chn), col="red", size=3) +
  geom_text(label = "USA", x = 1997, y = -500, colour = "blue", size = 10) +
  geom_line(data=df3, aes(y=usa), col= "blue", size=3) +
  ggtitle("Trading balance 30 biggest exporting countries") +
  theme(plot.title=element_text(face="bold", size=25), 
        axis.text = element_text(size=15),
        axis.title = element_text(size=20),
        legend.position="none")




############################
topcomms = trade %>% filter(year == 2009, exp == "chn" | exp == "usa") %>% 
  group_by(comm) %>% 
  summarize(val = sum(value)) %>% 
  ungroup() %>% arrange(-val) 
topcomms = topcomms[1:12,][[1]]
twoRegionsMainCommodities("chn", "usa", topcomms)

