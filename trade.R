library(plyr)
library(ggplot2)
library(igraph)

trade <- read.csv("tstrade.csv", header=TRUE, 
                    col.names=c("comm","exp","imp","year","value"),
                    colClasses=c("factor","factor","factor","factor","numeric"))

trade <- trade[trade$value > 0.000001,]
trade$year <- as.integer(substr(trade$year, 2, 5))

save(trade, file="trade.Rdata")

load("trade.Rdata")

trade95 <- subset(trade, year=1995)

sumval <- function(x) 
    data.frame(list(value=sum(x$value)))

trade95regs <- ddply(trade95, .(exp, imp), sumval)

g95 <- graph.data.frame(trade95)

comm95 <- ddply(trade95, .(comm), sumval)



g95wht <- graph.data.frame(ddply(subset(trade95, comm=="wht"), .(exp, imp), sumval))

write.graph(g95wht, "g95wht.gml", "gml")

write.gdf <- function(trade) {
    nodes <- ddply(trade, .(imp), function(x) data.frame(list(value=sum(x$value))))
    
}

usawht <- ddply(subset(trade, exp=="usa" & comm=="wht"), .(exp, year), sumval)

view.commexp <- function(exp1, comm1) {
    df <- ddply(subset(trade, exp==exp1 & comm==comm1), .(exp, year), sumval)
    plot(df$year, df$value, type="l")
}


view.boxcommexp <- function(exp1) {
    df <- ddply(subset(trade, exp==exp1), .(comm, year), sumval)
    boxplot(df$value ~ df$comm, las=2)
}
