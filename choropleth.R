library(choroplethr)
library(choroplethrAdmin1)
library(choroplethrMaps)
library(WDI)
library(rworldmap)
require(maptools)

## Not run:
# See http://data.worldbank.org/indicator/SP.POP.TOTL
choroplethr_wdi(code="SP.POP.TOTL", year=2012, title="2012 Population Estimates", num_colors=1)
# See http://data.worldbank.org/indicator/SP.DYN.LE00.IN
choroplethr_wdi(code="SP.DYN.LE00.IN", year=2012, title="2012 Life Expectancy Estimates")
# See http://data.worldbank.org/indicator/NY.GDP.PCAP.CD
choroplethr_wdi(code="NY.GDP.PCAP.CD", year=2012, title="2012 Per Capita Income")
## End(Not run)

data(countryExData)
sPDF <- joinCountryData2Map(countryExData, joinCode="ISO3", nameJoinColumn="ISO3V10")
mapCountryData(sPDF, nameColumnToPlot="BIODIVERSITY")

trade.expyear$expiso <- as.factor(toupper(as.character(trade.expyear$exp)))
sPDF <- joinCountryData2Map(subset(trade.expyear, year==2000), joinCode="ISO3", nameJoinColumn="expiso")
mapCountryData(sPDF, nameColumnToPlot="expval")

str(subset(trade.expyear, year==1995))

countries <- read.csv("../countries.csv")

trade.expyear2000 <- subset(trade.expyear, year==2000)
trade.exp2000 <- merge(countries, trade.expyear2000, by.x="gtapcode", by.y="exp", all.x=T)
trade.exp2000.spdf <- joinCountryData2Map(trade.exp2000, joinCode="ISO3", nameJoinColumn="iso3code")
mapCountryData(trade.exp2000.spdf, nameColumnToPlot="expval")
mapCountryData(trade.exp2000.spdf, nameColumnToPlot="adjval")

trade.exp2000$adjvallog <- log(trade.exp2000$adjval)
trade.exp2000.spdf <- joinCountryData2Map(trade.exp2000, joinCode="ISO_A3", nameJoinColumn="iso3code")
mapCountryData(trade.exp2000.spdf, nameColumnToPlot="adjvallog")




s <- map_data("world")
m <- ggplot(s, aes(x=long, y=lat, group=group)) + geom_polygon(fill="green", colour="black")
m


data(wrld_simpl)
plot(wrld_simpl)

## or subset based on the name
plot(wrld_simpl[wrld_simpl$NAME == "Russia", ])
## explore other attributes
summary(wrld_simpl)



library(ggplot2)
library(maps)
d1 <- map_data("state")
d2 <- unique(d1$group)
n <- length(d2)
d2 <- data.frame( 
  group=rep(d2,each=6), 
  g1=rep(1:3,each=2,length=6*n),
  g2=rep(1:2,length=6*n),
  value=runif(6*n)
)
d <- merge(d1, d2,  by="group")
qplot(
  long, lat, data = d, group = group, 
  fill = value, geom = "polygon" 
) + 
  facet_wrap( ~ g1 + g2 )





trade.exp1995 <- merge(countries, trade.expyear1995, by.x="gtapcode", by.y="exp", all.x=T)
trade.exp1995.spdf <- joinCountryData2Map(trade.exp1995, joinCode="ISO3", nameJoinColumn="iso3code")
mapCountryData(trade.exp1995.spdf, nameColumnToPlot="expval")
mapCountryData(trade.exp1995.spdf, nameColumnToPlot="adjval")
trade.exp1995$adjvallog <- log(trade.exp1995$adjval)
trade.exp1995.spdf <- joinCountryData2Map(trade.exp1995, joinCode="ISO_A3", nameJoinColumn="iso3code")


trade.exp2009 <- merge(countries, trade.expyear2009, by.x="gtapcode", by.y="exp", all.x=T)
trade.exp2009.spdf <- joinCountryData2Map(trade.exp2009, joinCode="ISO3", nameJoinColumn="iso3code")
mapCountryData(trade.exp2009.spdf, nameColumnToPlot="expval")
mapCountryData(trade.exp2009.spdf, nameColumnToPlot="adjval")
trade.exp2009$adjvallog <- log(trade.exp2009$adjval)
trade.exp2009.spdf <- joinCountryData2Map(trade.exp2009, joinCode="ISO_A3", nameJoinColumn="iso3code")
