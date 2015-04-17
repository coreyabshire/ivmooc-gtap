##  Co-occurence network of commodities

# Inspired by: http://stackoverflow.com/questions/13281303/creating-co-occurrence-matrix

load("trade.clean")
tr1995 = subset(trade, year == 1995)
dim(tr1995)
colnames(tr1995)
head(tr1995)
tr1995$imp = NULL
tr1995$year = NULL
tr1995$value = 1
tr1995 = unique(tr1995)

# reorder the columns
tr1995 = tr1995[c("exp", "comm", "value")]

#making the boolean matrix   
library(reshape2)
tr1995 <- melt(tr1995)
w <- dcast(tr1995, comm~exp)
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
plot(g)


