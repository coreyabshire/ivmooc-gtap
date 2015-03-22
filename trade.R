library(plyr)
library(ggplot2)
library(igraph)
library(choroplethr)

trade <- read.csv("../tstrade.csv", header=TRUE, 
                    col.names=c("comm","exp","imp","year","value"),
                    colClasses=c("factor","factor","factor","factor","numeric"))

trade <- trade[trade$value > 0.000001,]
trade$year <- as.integer(substr(trade$year, 2, 5))

setwd("C:/Users/Corey/Dropbox/Learning/IVMOOC/project/ivmooc-gtap")
save(trade, file="trade.Rdata")

load("../trade.Rdata")

trade95 <- subset(trade, year=1995)

sumval <- function(x) 
    data.frame(list(value=sum(x$value)))

trade95regs <- ddply(trade95, .(exp, imp), sumval)

g95 <- graph.data.frame(trade95)

comm95 <- ddply(trade95, .(comm), sumval)



g95wht <- graph.data.frame(ddply(subset(trade95, comm=="wht"), 
                                 .(exp, imp), sumval))

write.graph(g95wht, "g95wht.gml", "gml")

g05oil <- graph.data.frame(ddply(subset(trade, year==2005 & comm=="oil"), 
                                 .(exp, imp), sumval))

write.graph(g05oil, "g05oil.gml", "gml")

write.gdf <- function(trade) {
    nodes <- ddply(trade, .(imp), 
                   function(x) 
                       data.frame(list(value=sum(x$value))))
}

plot(g05oil)
tkplot(g05oil)

usawht <- ddply(subset(trade, exp=="usa" & comm=="wht"), .(exp, year), sumval)

view.commexp <- function(exp1, comm1) {
    df <- ddply(subset(trade, exp==exp1 & comm==comm1), .(exp, year), sumval)
    plot(df$year, df$value, type="l")
}

view.commexp("usa", "wht")
view.commexp("irn", "oil")

view.boxcommexp <- function(exp1) {
    df <- ddply(subset(trade, exp==exp1), .(comm, year), sumval)
    boxplot(df$value ~ df$comm, las=2)
}

view.boxcommexp("usa")
view.boxcommexp("ind")
view.boxcommexp("bel")

comm.sum <- ddply(trade, .(comm), sumval)

view.commyearexp <- function(comm1, year1) {
  df <- ddply(subset(trade, comm==comm1, year=year1), .(exp), sumval)
  ggplot(df) + 
    geom_bar(aes(x=exp, y=value), stat="identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

view.commyearexp("oil", 1995)
view.commyearexp("wht", 1995)

trade.expyear <- ddply(trade, .(exp, year), sumval)
colnames(trade.expyear) <- c("exp", "year", "expval")

trade.impyear <- ddply(trade, .(imp, year), sumval)
colnames(trade.impyear) <- c("imp", "year", "impval")

trade.regyear <- merge(trade.expyear, trade.impyear)
trade.regyear$valdiff <- trade.regyear$expval - trade.regyear$impval

ggplot(subset(trade.regyear, reg=="chn")) +
  geom_line(aes(x=year, y=valdiff))

ggplot(subset(trade.regyear)) +
  geom_line(aes(x=year, y=valdiff, col=reg))

trade.regyear$reg1 <- ifelse(trade.regyear$reg=="usa", "usa", "row")
trade.reg1year <- ddply(trade.regyear, .(reg1, year), sumval)

# too many regions
# too many commodities
# doesn't take inflation into account
# make region choroplethable

ggplot(subset(trade.regyear, reg=="usa")) +
  geom_line(aes(x=year, y=exp))

trade.m <- merge(trade, trade.impyear, by=c("imp","year"))
trade.m <- merge(trade.m, trade.expyear, by=c("exp","year"))

trade.m$imprat <- trade.m$value / trade.m$impval
trade.m$exprat <- trade.m$value / trade.m$expval

trade.mexp <- ddply(trade, .(exp, comm, year), sumval)
trade.mexp <- merge(trade.mexp, trade.expyear, by=c("exp", "year"))
trade.mexp$exprat <- trade.mexp$value / trade.mexp$expval

ggplot(subset(trade.mexp, exp=="usa")) +
  geom_bar(aes(x=year, y=exprat, fill=comm), stat="identity", position="stack")

ggplot(subset(trade.mexp, exp=="usa")) +
  geom_bar(aes(x=year, y=exprat, fill=comm), stat="identity") +
  facet_grid(. ~ comm)
