##  Co-occurence network

# Inspired by: http://stackoverflow.com/questions/13281303/creating-co-occurrence-matrix

#
#   co-occurrence of exported commodities
#

load("trade.clean")
ctrade = subset(trade, value > 10000)
ctr1995 = subset(ctrade, year == 2009)
dim(ctr1995)
colnames(ctr1995)
head(ctr1995)
ctr1995$imp = NULL
ctr1995$year = NULL
ctr1995$value = 1
ctr1995 = unique(ctr1995)

# reorder the columns
ctr1995 = ctr1995[c("exp", "comm", "value")]

#making the boolean matrix   
library(reshape2)
ctr1995 <- melt(ctr1995)
w <- dcast(ctr1995, comm~exp)
x <- as.matrix(w[,-1])
x[is.na(x)] <- 0
x <- apply(x, 2,  function(x) as.numeric(x > 0))  #recode as 0/1
v <- x %*% t(x)                                   #the magic matrix 
diag(v) <- 0                                      #repalce diagonal
dimnames(v) <- list(w[, 1], w[,1])                #name the dimensions
v

# graphing
library(igraph)
g <- graph.adjacency(v, weighted=TRUE, mode ='undirected')
g <- simplify(g)
# set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)
plot(g, main="year 2009 - value > 10000")

#
#   co-occurrence of exported regions
#

topRegions1995 = Sum_x_ExpYear %>% 
  filter(year == 1995) %>% 
  ungroup %>% arrange(desc(commval)) %>% 
  select(exp) 
top10 = topRegions1995[1:25,][[1]]

load("trade.plus")
rtrade = subset(tradeplus, exp %in% top10 & imp %in% top10)
# rtrade = subset(rtrade, value > 10000)
rtr1995 = subset(rtrade, year == 2009)
rtr1995 <- rtr1995 %>%
  group_by(exp, imp) %>%
  summarise(weight = sum(weight)) %>%
  arrange() %>% ungroup()
dim(rtr1995)
colnames(rtr1995)
head(rtr1995)
# rtr1995$value = 1
rtr1995$commval = rtr1995$commval/10000
rtr1995 = unique(rtr1995)
tv = rtr1995 %>% select(exp, imp, comm, value) %>% group_by(exp, imp) %>% summarise(val = sum(value))
tw = rtr1995 %>% select(exp, imp, comm, weight) %>%  group_by(exp, imp) %>% summarise(weight = sum(weight))


#making the boolean matrix   
library(reshape2)
m <- melt(tw)
w <- dcast(tw, exp~imp)
x <- as.matrix(w[,-1])
x[is.na(x)] <- 0
x <- apply(x, 2,  function(x) as.numeric(x > 0.1))  #recode as 0/1
v <- x %*% t(x)                                   #the magic matrix 
vdiag(v) <- 0                                      #repalce diagonal
dimnames(v) <- list(w[, 1], w[,1])                #name the dimensions
# v = x

# graphing
library(igraph)
g <- graph.adjacency(v, weighted=TRUE, mode ='directed')
g <- simplify(g)
# set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)
egam <- (log(E(g)$weight)+.4) / max(log(E(g)$weight)+.4)
E(g)$color <- rgb(.5, .5, 0, egam)
E(g)$width <- egam * 5
layout1 <- layout.fruchterman.reingold(g)
plot(g, layout=layout1, main="year 2009 - value > 10000")



