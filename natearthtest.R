library(raster)
library(rgdal)
library(ggplot2)
library(reshape2)
library(plyr)

# --------------------------------------------------------------------------------
# Note: This first part I found in a tutorial online:
# https://downwithtime.wordpress.com/2013/12/04/naturalearthdata-and-r-in-ggplot2/
# --------------------------------------------------------------------------------

#  Assuming you have a path 'Maps' that you store your spatial files in.  This
#  is all downloaded from <a href="http://www.naturalearthdata.com/downloads/">http://www.naturalearthdata.com/downloads/</a> using the
#  1:50m "Medium" scale data.

nat.earth <- stack('./Maps/NE2_50M_SR_W/NE2_50M_SR_W.tif')

ne_lakes <- readOGR('./Maps/NaturalEarth/ne_50m_lakes.shp',
                    'ne_50m_lakes')

ne_rivers <- readOGR('./Maps/NaturalEarth/ne_50m_rivers_lake_centerlines.shp',
                     'ne_50m_rivers_lake_centerlines')

ne_coast <- readOGR('./Maps/NaturalEarth/ne_50m_coastline.shp',
                    'ne_50m_coastline')

#  I have a domain I'm interested in, but there's no reason you can't define something else:
quick.subset <- function(x, longlat){
  # longlat should be a vector of four values: c(xmin, xmax, ymin, ymax)
  x@data$id <- rownames(x@data)
  x.f = fortify(x, region="id")
  x.join = join(x.f, x@data, by="id")
  x.subset <- subset(x.join, x.join$long > longlat[1] & x.join$long < longlat[2] &
                       x.join$lat > longlat[3] & x.join$lat < longlat[4])
  x.subset
}

domain <- c(-98.6, -66.1, 36.5, 49.7)
lakes.subset <- quick.subset(ne_lakes, domain)
river.subset <- quick.subset(ne_rivers, domain)
coast.subset <- quick.subset(ne_coast, domain)

nat.crop <- crop(nat.earth, y=extent(domain))

rast.table <- data.frame(xyFromCell(nat.crop, 1:ncell(nat.crop)),
                         getValues(nat.crop/255))

rast.table$rgb <- with(rast.table, rgb(NE2_50M_SR_W.1,
                                       NE2_50M_SR_W.2,
                                       NE2_50M_SR_W.3,
                                       1))
# et voila!

ggplot(data = rast.table, aes(x = x, y = y)) +
  geom_tile(fill = rast.table$rgb) +
  geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#ADD8E6') +
  scale_alpha_discrete(range=c(1,0)) +
  geom_path(data=lakes.subset, aes(x = long, y = lat, group = group), color = 'blue') +
  geom_path(data=river.subset, aes(x = long, y = lat, group = group), color = 'blue') +
  geom_path(data=coast.subset, aes(x = long, y = lat, group = group), color = 'blue') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  xlab('') + ylab('')


ggplot(data = lakes.subset) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = '#ADD8E6') +
  scale_alpha_discrete(range=c(1,0)) +
  geom_path(data=lakes.subset, aes(x = long, y = lat, group = group), color = 'blue') +
  geom_path(data=river.subset, aes(x = long, y = lat, group = group), color = 'blue') +
  geom_path(data=coast.subset, aes(x = long, y = lat, group = group), color = 'blue') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  xlab('') + ylab('')



# __________________________________________________________________________
# This is me borrowing from above to try with india (for my Ptolemy project..)
#

domain <- c(40, 120, 0, 45)
lakes.subset <- quick.subset(ne_lakes, domain)
river.subset <- quick.subset(ne_rivers, domain)
coast.subset <- quick.subset(ne_coast, domain)

nat.crop <- crop(nat.earth, y=extent(domain))

rast.table <- data.frame(xyFromCell(nat.crop, 1:ncell(nat.crop)),
                         getValues(nat.crop/255))

rast.table$rgb <- with(rast.table, rgb(NE2_50M_SR_W.1,
                                       NE2_50M_SR_W.2,
                                       NE2_50M_SR_W.3,
                                       1))
# et voila!

ggplot(data = rast.table, aes(x = x, y = y)) +
  geom_tile(fill = rast.table$rgb) +
  geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#ADD8E6') +
  scale_alpha_discrete(range=c(1,0)) +
  geom_path(data=lakes.subset, aes(x = long, y = lat, group = group), color = 'blue') +
  geom_path(data=river.subset, aes(x = long, y = lat, group = group), color = 'blue') +
  geom_path(data=coast.subset, aes(x = long, y = lat, group = group), color = 'blue') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  xlab('') + ylab('')


ggplot(data = lakes.subset) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = '#ADD8E6') +
  scale_alpha_discrete(range=c(1,0)) +
  geom_path(data=lakes.subset, aes(x = long, y = lat, group = group), color = 'blue') +
  geom_path(data=river.subset, aes(x = long, y = lat, group = group), color = 'blue') +
  geom_path(data=coast.subset, aes(x = long, y = lat, group = group), color = 'blue') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  xlab('') + ylab('')


# __________________________________________________________________________
# This is the one I used for the GTAP data (and thus the poster).
#

admin0 <- readOGR('../Maps/NaturalEarth/ne_50m_admin_0_countries.shp',
                    'ne_50m_admin_0_countries')

domain <- c(-180, 180, -90, 90)
admin0.subset <- quick.subset(admin0, domain)
admin0.subset$id <- as.integer(admin0.subset$id)

nat.crop <- crop(nat.earth, y=extent(domain))

rast.table <- data.frame(xyFromCell(nat.crop, 1:ncell(nat.crop)),
                         getValues(nat.crop/255))

ggplot(data = admin0.subset) +
  geom_polygon(aes(x = long, y = lat, group = group, color = pop_est, fill = pop_est)) +
  scale_alpha_discrete(range=c(1,0)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  xlab('') + ylab('')

admin0.merge <- merge(x=admin0.subset, y=trade.exp1995, by.x="iso_a3", by.y="iso3code", all.x=T)
admin0.merge$id <- as.integer(admin0.merge$id)
admin0.merge <- admin0.merge[order(admin0.merge$id, admin0.merge$order),]
admin0.iso3 <- ddply(admin0.merge, .(iso_a3), "nrow")
pdf("geo1995.pdf", width=6, height=3)
ggplot(data = admin0.merge) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = expval), color = "#000000") +
  scale_alpha_discrete(range=c(1,0)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  xlab('') + ylab('')
dev.off()


admin0.merge <- merge(x=admin0.subset, y=trade.exp2009, by.x="iso_a3", by.y="iso3code", all.x=T)
admin0.merge$id <- as.integer(admin0.merge$id)
admin0.merge <- admin0.merge[order(admin0.merge$id, admin0.merge$order),]
admin0.iso3 <- ddply(admin0.merge, .(iso_a3), "nrow")
pdf("geo2009.pdf", width=6, height=3)
ggplot(data = admin0.merge) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = expval), color = "#000000") +
  scale_alpha_discrete(range=c(1,0)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  xlab('') + ylab('')
dev.off()




admin0.iso3$iso_a3  

trade.exp2000[!(trade.exp2000$iso3code %in% admin0.iso3$iso_a3), ]

admin0.iso3[!(admin0.iso3$iso_a3 %in% trade.exp2000$iso3code), ]  
