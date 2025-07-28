require(tidyverse)
require(sp)
require(rgeos)
require(sf)
require(extrafont)

load("maps.RData")
load("data.RData")

districtmap@data$ST_NM[districtmap@data$DISTRICT == "Nizamabad"] = "Telangana"
districtmap1 = districtmap[districtmap@data$ST_NM == "Telangana",]

data$ST_NM[data$DISTRICT == "Nizamabad"] = "Telangana"
datag = data %>% filter(ST_NM == "Telangana") %>%
  group_by(DISTRICT) %>% summarize(freq = n_distinct(OBSERVER.ID))


fortified = fortify(districtmap1, region = c("DISTRICT"))
plotdis = na.omit(left_join(fortified,datag, by = c('id' = "DISTRICT"))) # SPDF to plot

require(viridis)

plotdismap = ggplot() +
  geom_polygon(data = plotdis, aes(x=long, y=lat, group=group,fill = freq), colour = "black")+  
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(0.5,1,0,1), "cm"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = NA, colour = NA),
        plot.background = element_rect(fill = NA, colour = NA))+
  theme(text=element_text(family="Gill Sans MT"), legend.title = element_text(size = 16),
        legend.text = element_text(size = 12)) +
  scale_fill_viridis(breaks = c(50,100,200,400), trans = "log") +
  coord_map()

print(plotdismap)
ggsave(file="Telangana_observers.jpeg", units="in", width=6.6, height=7)
dev.off()  


###################################


################################################################

require(tidyverse)
require(ggfortify)
require(rgdal)
require(sp)
require(sf)
require(rgeos)
require(mapview)


path1="India"
name1="India_2011"
path2="India States"
name2="IndiaStates_2011"
path3="India Districts"
name3="IndiaDistricts_2011"

statemap1 = readOGR(path2,name2)
districtmap1 = readOGR(path3,name3)

states = c("Maharashtra")

finalmap = districtmap1[districtmap1@data$ST_NM %in% states,]

proj4string(finalmap) = "+proj=longlat +datum=WGS84"

mapviewOptions(fgb = FALSE)
a = mapView(finalmap, zcol = NULL, map.types = c("Esri.WorldImagery","OpenTopoMap"),
            layer.name = NULL, 
            popup = leafpop::popupTable(finalmap,c("DISTRICT"), 
                                        feature.id=FALSE, 
                                        row.numbers=FALSE), 
            alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
mapshot(a, "Maharashtra_Districts.html")



################################################################

require(tidyverse)
require(ggfortify)
require(rgdal)
require(sp)
require(sf)
require(rgeos)
require(mapview)


path4="2021 India Admin Boundary Maps"
name4="India_States_2021"
path5="2021 India Admin Boundary Maps"
name5="India_Districts_2021"

statemap2 = readOGR(path4,name4)
districtmap2 = readOGR(path5,name5)

states = c("KARNATAKA")

finalmap = districtmap2[districtmap2@data$stname %in% states,]

proj4string(finalmap) = "+proj=longlat +datum=WGS84"

mapviewOptions(fgb = FALSE)
a = mapView(finalmap, zcol = NULL, map.types = c("Esri.WorldImagery","OpenTopoMap"),
            layer.name = NULL, 
            popup = leafpop::popupTable(finalmap,c("dtname"), 
                                        feature.id=FALSE, 
                                        row.numbers=FALSE), 
            alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
mapshot(a, "Telangana_Districts.html")



################################################################

require(tidyverse)
require(ggfortify)
require(rgdal)
require(sp)
require(sf)
require(rgeos)
require(mapview)


path6="in_states_2019"
name6="in_states_2019"
path7="in_dists_2019"
name7="in_dist_2019"

statemap3 = readOGR(path6,name6)
districtmap3 = readOGR(path7,name7)

#states = c("KARNATAKA")

#finalmap = districtmap3[districtmap3@data$stname %in% states,]
districtmap1 = gSimplify(districtmap3, tol=0.01, topologyPreserve=TRUE)
d1 = districtmap3@data
districtmap1 = sp::SpatialPolygonsDataFrame(districtmap1, d1)
finalmap = gBuffer(districtmap1, byid=TRUE, width=0)

proj4string(finalmap) = "+proj=longlat +datum=WGS84"

mapviewOptions(fgb = FALSE)
a = mapView(finalmap, zcol = NULL, map.types = c("Esri.WorldImagery","OpenTopoMap"),
            layer.name = NULL, 
            popup = leafpop::popupTable(finalmap,c("dtname"), 
                                        feature.id=FALSE, 
                                        row.numbers=FALSE), 
            alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
mapshot(a, "India_Districts.html")
