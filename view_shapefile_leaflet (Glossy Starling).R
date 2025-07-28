require(tidyverse)
require(rgdal)
require(sp)
require(sf)
require(mapview)
require(leaflet)

map_2019 = readOGR("in_dists_2019","in_dist_2019")
state = "WEST BENGAL"
stp = map_2019[map_2019@data$stname %in% state,]

proj4string(stp) = "+proj=longlat +datum=WGS84"

a = mapView(stp, map.types = c("Esri.WorldImagery","OpenTopoMap","OpenStreetMap"),
            alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
mapshot(a, "WBdistricts.html")





####### India map and points

require(tidyverse)
require(rgdal)
require(sp)
require(sf)
require(mapview)
require(leaflet)

indiamap = readOGR("India","India_2011")
statemap = readOGR("in_states_2019","in_states_2019")
proj4string(statemap) = "+proj=longlat +datum=WGS84"
proj4string(indiamap) = "+proj=longlat +datum=WGS84"

state = "ANDAMAN & NICOBAR"
anmap = statemap[statemap@data$stname %in% state,]

mmrmap = readOGR("MMR_adm","MMR_adm1")
mmrmap1 = mmrmap[mmrmap@data$NAME_1 %in% c("Tanintharyi"),]
mmrmap = mmrmap[mmrmap@data$NAME_1 %in% c("Rakhine"),]

bgdmap = readOGR("BGD_adm","BGD_adm2")
bgdmap = bgdmap[bgdmap@data$NAME_2 %in% c("Cox's Bazar","Bandarbon","Parbattya Chattagram"),]
bgdmap@data$NAME_1 = bgdmap@data$NAME_2
bgdmap@data$ID_1 = bgdmap@data$ID_2

mmrmap@data = mmrmap@data[,1:5]
bgdmap@data = bgdmap@data[,1:5]

rmap = rbind(mmrmap,bgdmap)

require(rgeos)
centers = data.frame(gCentroid(rmap, byid = TRUE))
centers$region = rmap@data$NAME_1
centers$region[1] = "Arakan"
centers$region[4] = "Rangamati"
centers$x[centers$region %in% c("Arakan")] = centers$x[centers$region %in% c("Arakan","Ayeyarwady")]+1.2
centers$x[centers$region %in% c("Bandarbon")] = centers$x[centers$region %in% c("Bandarbon")]+1.1
centers$y[centers$region %in% c("Bandarbon")] = centers$y[centers$region %in% c("Bandarbon")]-0.5
centers$x[centers$region %in% c("Rangamati")] = centers$x[centers$region %in% c("Rangamati")]+1
centers$x[centers$region %in% c("Cox's Bazar")] = centers$x[centers$region %in% c("Cox's Bazar")]-0.9
centers$y[centers$region %in% c("Cox's Bazar")] = centers$y[centers$region %in% c("Cox's Bazar")]-0.5

recs1 = data.frame(y = NA, x = NA)
recs1[1,] = c(23.863, 91.299)
recs1[2,] = c(16.097360, 94.244787)
recs1[3,] = c(26.966697, 94.619368)
recs1[4,] = c(26.583333, 87.083333)

recs1$labels = c("Agartala, Tripura","Ayeyarwady, Myanmar","Sibsagar, Assam","Koshi Tappu Wildlife Reserve, Nepal")
recs1$labs = c(1:4)

recs3 = data.frame(y = NA, x = NA)
#recs3[1,] = c(22.502738, 92.201901)
#recs3[2,] = c(20.905424, 92.260737)
recs3[1,] = c(24.127113, 91.442143)
recs3[2,] = c(24.341677, 91.798398)
recs3[3,] = c(24.431266, 92.330418)


#"Kaptai National Park","Teknaf Wildife Sanctuary"
recs3$labels = c("Satchari National Park",
                 "Lawachara National Park","Makunda Christian Leprosy and General Hospital")
recs3$labs = c("a","b","c")

recs4 = data.frame(y = NA, x = NA)
recs4[1,] = c(19.3, 91.7)
recs4$labs = "affinis"
recs5 = data.frame(y = NA, x = NA)
recs5[1,] = c(13.5, 91.5)
recs5$labs = "tytleri"
recs6 = data.frame(y = NA, x = NA)
recs6[1,] = c(15, 99.2)
recs6$labs = "strigata"

a = mapView(map.types = c("Esri.WorldImagery"))

a@map = a@map %>% 
  addPolygons(data = indiamap, color = "black", fill = F, weight = 12, opacity = 1) %>%
  addPolygons(data = rmap, color = "white", fill = F, weight = 12, opacity = 1) %>%
  addPolygons(data = mmrmap1, color = "#f5bc42", fill = F, weight = 12, opacity = 1) %>%
  addPolygons(data = anmap, color = "#b0c75b", fill = F, weight = 12, opacity = 1) %>%
  addCircleMarkers(data = recs1,
                   lng = ~x, lat = ~y, label = ~labs,
                   radius = 35,
                   color = "#e62c2f",
                   stroke = FALSE, fillOpacity = 1,
                   labelOptions = labelOptions(noHide = TRUE, direction = 'top', alpha = 1,
                                               textOnly = TRUE, 
                                               style = list(
                                                 "color" = "#e62c2f", 
                                                 "font-family" = "Gill Sans",
                                                 "font-style" = "bold",
                                                 "font-size" = "120px"
                                               ))) %>%
  addCircleMarkers(data = recs3,
                   lng = ~x, lat = ~y, label = ~labs,
                   radius = 35,
                   color = "#6baeed",
                   stroke = FALSE, fillOpacity = 1,
                   labelOptions = labelOptions(noHide = TRUE, direction = 'top', alpha = 1,
                                               textOnly = TRUE, 
                                               style = list(
                                                 "color" = "#6baeed", 
                                                 "font-family" = "Gill Sans",
                                                 "font-style" = "bold",
                                                 "font-size" = "120px"
                                               ))) %>%
  addLabelOnlyMarkers(data = centers,
                      lng = ~x, lat = ~y, label = ~region,
                      labelOptions = labelOptions(noHide = TRUE, direction = 'top', alpha = 1,
                                                  textOnly = TRUE,
                                                  style = list(
                                                    "color" = "white", 
                                                    "font-family" = "Gill Sans",
                                                    "font-style" = "bold",
                                                    "font-size" = "120px"
                                                  ))) %>%
  #addLabelOnlyMarkers(data = recs4,
  #                    lng = ~x, lat = ~y, label = ~labs,
  #                    labelOptions = labelOptions(noHide = TRUE, direction = 'top', alpha = 1,
  #                                                textOnly = TRUE,
  #                                                style = list(
  #                                                  "color" = "white", 
  #                                                  "font-family" = "Gill Sans",
  #                                                  "font-style" = "italic",
  #                                                  "font-size" = "140px"
  #                                                ))) %>%
  #addLabelOnlyMarkers(data = recs5,
  #                    lng = ~x, lat = ~y, label = ~labs,
  #                    labelOptions = labelOptions(noHide = TRUE, direction = 'top', alpha = 1,
  #                                                textOnly = TRUE,
  #                                                style = list(
  #                                                  "color" = "#b0c75b", 
  #                                                  "font-family" = "Gill Sans",
  #                                                  "font-style" = "italic",
  #                                                  "font-size" = "140px"
  #                                                ))) %>%
  #addLabelOnlyMarkers(data = recs6,
  #                    lng = ~x, lat = ~y, label = ~labs,
  #                    labelOptions = labelOptions(noHide = TRUE, direction = 'top', alpha = 1,
  #                                                textOnly = TRUE,
  #                                                style = list(
  #                                                  "color" = "#f5bc42", 
  #                                                  "font-family" = "Gill Sans",
  #                                                  "font-style" = "italic",
  #                                                  "font-size" = "140px"
  #                                                ))) %>%
  setView(zoom=RgoogleMaps::MaxZoom(latrange=40:10, lonrange=87:100, size = c(480, 480)),lat=20.7, lng=93)
mapshot(a, file = "AGST_ssp.jpeg", remove_controls = c("zoomControl", "layersControl", "homeButton", "scaleBar"),
        vwidth = 8000, vheight = 6000)



################ Plain background

require(tidyverse)
require(rgdal)
require(sp)
require(sf)
require(mapview)
require(leaflet)

indiamap = readOGR("India","India_2011")
statemap = readOGR("in_states_2019","in_states_2019")
proj4string(statemap) = "+proj=longlat +datum=WGS84"
proj4string(indiamap) = "+proj=longlat +datum=WGS84"

state = "ANDAMAN & NICOBAR"
anmap = statemap[statemap@data$stname %in% state,]

mmrmap = readOGR("MMR_adm","MMR_adm1")
mmrmap2 = mmrmap[mmrmap@data$NAME_1 %in% c("Tanintharyi"),]
mmrmap1 = mmrmap[mmrmap@data$NAME_1 %in% c("Rakhine"),]

mmrmapx = readOGR("MMR_adm","MMR_adm0")

bgdmap = readOGR("BGD_adm","BGD_adm2")
bgdmap1 = bgdmap[bgdmap@data$NAME_2 %in% c("Cox's Bazar","Bandarbon","Parbattya Chattagram"),]
bgdmap1@data$NAME_1 = bgdmap1@data$NAME_2
bgdmap1@data$ID_1 = bgdmap1@data$ID_2

bgdmapx = readOGR("BGD_adm","BGD_adm0")

mmrmap1@data = mmrmap1@data[,1:5]
bgdmap1@data = bgdmap1@data[,1:5]

rmap = rbind(mmrmap1,bgdmap1)

recs1 = data.frame(y = NA, x = NA)
recs1[1,] = c(23.863, 91.299)
recs1[2,] = c(16.097360, 94.244787)
recs1[3,] = c(26.966697, 94.619368)
recs1[4,] = c(26.583333, 87.083333)

recs1$labels = c("Agartala, Tripura","Ayeyarwady, Myanmar","Sibsagar, Assam","Koshi Tappu Wildlife Reserve, Nepal")
recs1$labs = c(1:4)

recs2 = recs1[1,]
recs1 = recs1[-1,]

recs3 = data.frame(y = NA, x = NA)
#recs3[1,] = c(22.502738, 92.201901)
#recs3[2,] = c(20.905424, 92.260737)
recs3[1,] = c(24.127113, 91.442143)
recs3[2,] = c(24.341677, 91.798398)
recs3[3,] = c(24.431266, 92.330418)


#"Kaptai National Park","Teknaf Wildife Sanctuary"
recs3$labels = c("Satchari National Park",
                 "Lawachara National Park","Makunda Christian Leprosy and General Hospital")
recs3$labs = c("a","b","c")


plotdismap = ggplot() +
  geom_polygon(data = anmap, aes(x=long, y=lat, group=group), colour = NA, fill = "#b0c75b")+  
  geom_polygon(data = rmap, aes(x=long, y=lat, group=group), colour = NA, fill = "#e62c2f")+  
  geom_polygon(data = mmrmap2, aes(x=long, y=lat, group=group), colour = NA, fill = "#f5bc42")+ 
  geom_polygon(data = indiamap, aes(x=long, y=lat, group=group), colour = 'black', fill = NA)+  
  geom_polygon(data = bgdmapx, aes(x=long, y=lat, group=group), colour = 'black', fill = NA)+  
  geom_polygon(data = mmrmapx, aes(x=long, y=lat, group=group), colour = 'black', fill = NA)+  
  geom_point(data = recs1, aes(x = x, y = y), col = "#e62c2f", size = 3) +
  geom_point(data = recs2, aes(x = x, y = y), col = "#e62c2f", size = 2, shape = 4, stroke = 2) +
  #geom_point(data = recs3, aes(x = x, y = y), col = "#6baeed", size = 3) +
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
        plot.margin=unit(c(0,0,0,0), "cm"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank())+
  theme(text=element_text(family="Gill Sans MT"), legend.title = element_text(size = 12)) +
  coord_map()

print(plotdismap)
ggsave(file="AGST_plain.jpeg", units="in", width=10, height=7)
dev.off()
