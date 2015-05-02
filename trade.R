library(plyr)
library(ggplot2)
library(igraph)
library(choroplethr)
library(reshape2)
library(portfolio)
library(treemap)

trade <- read.csv("../tstrade.csv", header=TRUE, 
                    col.names=c("comm","exp","imp","year","value"),
                    colClasses=c("factor","factor","factor","factor","numeric"))

trade <- trade[trade$value > 0.000001,]
trade$year <- as.integer(substr(trade$year, 2, 5))

setwd("C:/Users/Corey/Dropbox/Learning/IVMOOC/project/ivmooc-gtap")
save(trade, file="trade.Rdata")

load("../trade.Rdata")

trade95 <- subset(trade, year=1995)


sumival <- function(x) 
  data.frame(list(ivalue=sum(x$ivalue)))

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

trade.expyear <- ddply(trade, .(exp, year), sumival)
colnames(trade.expyear) <- c("exp", "year", "expval")

trade.expyear1995 <- subset(trade.expyear, year=="1995")
trade.expyear1995 <- trade.expyear1995[order(trade.expyear1995$expval), ]

trade.expyear2009 <- subset(trade.expyear, year=="2009")
trade.expyear2009 <- trade.expyear2009[order(trade.expyear2009$expval), ]


order(trade.expyear1995$expval)

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

# would be of interest
ggplot(subset(trade.mexp, exp=="usa")) +
  geom_bar(aes(x=year, y=exprat, fill=comm), stat="identity", position="stack")

ggplot(subset(trade.mexp, exp=="ind")) +
  geom_bar(aes(x=year, y=exprat, fill=comm), stat="identity") +
  facet_grid(. ~ comm)

trade.year <- ddply(trade, .(year), "nrow")

trade.exp <- ddply(trade, .(exp), "nrow")

trade.imp <- ddply(trade, .(imp), "nrow")

ggplot(trade.year) + 
  geom_line(aes(x=year, y=nrow))

# load the metadata

# load the price indices
index <- read.csv('../index.csv')[1:19,1:4]
str(index)
index
save(index, file="index.Rdata")
load("index.Rdata")

# adjust values by index
trade <- merge(trade, index)
str(trade)
trade$ivalue <- trade$value / trade$multiplier

sumvalues <- function(x) 
  data.frame(list(
    value  = sum(x$value),
    ivalue = sum(x$ivalue)))

us.annual <- ddply(subset(trade, exp=="usa"), .(year), sumvalues)
str(us.annual)
    
us.annual.melt <- melt(us.annual, id.vars=c("year"))

ggplot(us.annual.melt) +
  geom_line(aes(x=year, y=value, col=variable))

annual <- melt(ddply(trade, .(year), sumvalues), id.vars=c("year"))
str(annual)

ggplot(annual) +
  geom_line(aes(x=year, y=value, col=variable))

annual.c <- ddply(trade, .(year, exp), sumival)
str(annual.c)

ggplot(annual.c) +
  geom_line(aes(x=year, y=ivalue, col=exp))

annual.c <- merge(annual.c, subset)
annual.c.div <- subset(annual.c, year==1995)[,c("exp","ivalue")]
colnames(annual.c.div) <- c("exp", "ivalue95")
str(annual.c.div)
annual.c <- merge(annual.c, annual.c.div)
str(annual.c)
annual.c$perc <- annual.c$ivalue / annual.c$ivalue95

ggplot(annual.c) +
  geom_line(aes(x=year, y=perc, col=exp))

head(annual.c[order(annual.c$perc, decreasing=T),])

annual.c.naze <- subset(annual.c, exp != "aze")
ggplot(annual.c.naze) +
  geom_line(aes(x=year, y=perc, col=exp))

head(annual.c.naze[order(annual.c.naze$perc, decreasing=T),])



# get some GDP data

# try making a treemap, since it seems to be popular here
# following along with: http://flowingdata.com/2010/02/11/an-easy-way-to-make-a-treemap/
map.market(id=annual.c$year, area=annual.c$ivalue, group=annual.c$exp, color=annual.c$ivalue, main="Economics Map")

df <- ddply(subset(trade, year==1995), .(exp, imp), sumival)
map.market(id=df$exp, area=df$ivalue, group=df$imp, color=df$ivalue, main="Economics Map")

treemap(df, c("exp","imp"), "ivalue")
treemap(df, c("exp"), "ivalue")
treemap(trade, c("comm"), "ivalue")
treemap(subset(trade, year==1995), c("comm"), "ivalue")
treemap(subset(trade, year==2005), c("comm"), "ivalue")
treemap(subset(trade, year==2005), c("comm","exp"), "ivalue")
treemap(subset(trade, year==2005), c("exp","comm"), "ivalue")

pdf("treemap1995.pdf", width=6, height=3)
treemap(subset(trade, year==1995), c("exp"), "ivalue", title="Treemap of Exporters Value for 1995 (in 1995 dollars)")
dev.off()

pdf("treemap2009.pdf", width=6, height=3)
treemap(subset(trade, year==2009), c("exp"), "ivalue", title="Treemap of Exporters Value for 2009 (in 1995 dollars)")
dev.off()

for (y in 1995:2009) {
  fn = paste0("images/treemap_comm_ivalue_", y, ".png")
  png(filename=fn, width=1280, height=720, units="px")
  treemap(subset(trade, year==y), c("comm"), "ivalue")
  dev.off()
}

