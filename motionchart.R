# 
#   read raw data, cleanup values less than 1mUSD, save as trade.clean
# 

library(dplyr)
library(ggplot2)
library(googleVis)

load("trade.clean")

trade <- read.csv("C:/tstrade.csv", header=TRUE, 
                  col.names=c("comm","exp","imp","year","value"),
                  colClasses=c("factor","factor","factor","factor","numeric"))

# ignore all values of less than 1 million USD per commodity traded
trade <- subset(trade, value > 1)

# remove prefix "Y" from year
trade$year <- as.integer(substr(trade$year, 2, 5))

str(trade)

save(trade, file="trade.clean")


byComm <- trade %>%
  group_by(comm, year) %>%
  summarise(commval = sum(value)) %>%
  filter(commval >1)

?gvisMotionChart

df=unique(byComm)
df$exp=rnorm(nrow(df))
df$imp=rnorm(nrow(df))

#gvis<-gvisMotionChart(df, idvar = "exp", timevar = "year")

gvis<-gvisMotionChart(df, idvar = "comm", timevar = "year" , options=list(width="1299px", height="500px"))

#gvis<-gvisMotionChart(df, idvar = "exp", timevar = "year")


plot(gvis)

# only USA

tradeUSA <- subset(trade, exp=usa)

byComm <- tradeUSA %>%
  group_by(comm, year) %>%
  summarise(commval = sum(value)) %>%
  filter(commval > 100)

?gvisMotionChart

df=unique(byComm)
df$exp=rnorm(nrow(df))
df$imp=rnorm(nrow(df))

#gvis<-gvisMotionChart(df, idvar = "exp", timevar = "year")

gvis<-gvisMotionChart(df, idvar = "comm", timevar = "year" , options=list(width="1400px", height="500px"))

#gvis<-gvisMotionChart(df, idvar = "exp", timevar = "year")

plot(gvis)



byComm <- trade %>%
  group_by(exp, year) %>%
  summarise(commval = sum(value)) %>%
  filter(commval > 100)

df=unique(byComm)
df$comm=rnorm(nrow(df))
df$exp=rnorm(nrow(df))

#gvis<-gvisMotionChart(df, idvar = "exp", timevar = "year")

gvis<-gvisMotionChart(df, idvar = "exp", timevar = "year" , options=list(width="1400px", height="500px"))

#gvis<-gvisMotionChart(df, idvar = "exp", timevar = "year")


plot(gvis)
