# 
#   read raw data, cleanup values less than 1mUSD, save as trade.clean
# 

trade <- read.csv("tstrade.csv", header=TRUE, 
                    col.names=c("comm","exp","imp","year","value"),
                    colClasses=c("factor","factor","factor","factor","numeric"))

# ignore all values of less than 1 million USD per commodity traded
trade <- subset(trade, value > 1)

# remove prefix "Y" from year
trade$year <- as.integer(substr(trade$year, 2, 5))

str(trade)

save(trade, file="trade.clean")
