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
top10 = topRegions1995[1:10,][[1]]
# rtrade = subset(trade, exp %in% top10, imp %in top10)
rtrade = subset(trade, value > 10000)
rtr1995 = subset(rtrade, year == 2009)
dim(rtr1995)
colnames(rtr1995)
head(rtr1995)
rtr1995$imp = NULL
rtr1995$year = NULL
rtr1995$value = 1
rtr1995 = unique(rtr1995)

#making the boolean matrix   
library(reshape2)
rtr1995 <- melt(rtr1995)
w <- dcast(rtr1995, exp~comm)
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



