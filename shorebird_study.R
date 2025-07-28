load("data.RData")
load("maps.RData")
names(gridmapg1)[1] = "NAME"

pamap = pamap[pamap@data$NAME != "Chintamani Kar Bird Sanctuary",]
data$NAME[data$NAME == "Chintamani Kar Bird Sanctuary"] = NA

onepercent = read.csv("onepercentCAF.csv")
pointfivepercent = read.csv("pointfivepercentCAF.csv")

require(tidyverse)
require(ggfortify)
require(rgdal)
require(sp)
require(sf)
require(rgeos)
require(mapview)

tax = read.csv("conservation_tool_mapping_2021.csv")
tax = tax %>% select(COMMON.NAME,Family,IUCN.Category,SOIB.Long.Term.Status,SOIB.Concern.Status)

caf = c("Eurasian Curlew","Crab-Plover","Black-tailed Godwit","Bar-tailed Godwit","Red Knot","Long-toed Stint",
        "Indian Skimmer","Greater Flamingo","Lesser Flamingo","Curlew Sandpiper","Little Stint","Lesser Sand-Plover",
        "Black-bellied Tern","Great Knot","Common Pochard","Ferruginous Duck","European Roller","Sociable Lapwing",
        "Yellow-breasted Bunting","White-headed Duck")

datacaf = data %>% filter(COMMON.NAME %in% caf) %>% group_by(COMMON.NAME) %>% 
  summarize(n = n()) %>% ungroup

data0 = left_join(data,tax)

data1 = data0 %>% 
  filter(Family %in% c("Scolopacidae (Sandpipers and Allies)",
                       "Glareolidae (Pratincoles and Coursers)","Dromadidae (Crab-Plover)",
                       "Burhinidae (Thick-knees)","Recurvirostridae (Stilts and Avocets)",
                       "Haematopodidae (Oystercatchers)","Charadriidae (Plovers and Lapwings)"), 
         cyear > 2011) %>%
  group_by(group.id,COMMON.NAME) %>% slice(1) %>% ungroup



# criterion 1

data2 = data1 %>% filter(!is.na(NAME), OBSERVATION.COUNT != "X") %>% 
  mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
  group_by(NAME,group.id,month,cyear) %>% summarize(agg = sum(OBSERVATION.COUNT)) %>% ungroup

data3 = data2 %>% filter(agg >= 2000)

dataPA1 = data3 %>% distinct(NAME,month,cyear) %>% group_by(NAME) %>% summarize(count = n()) %>%
  filter(count >= 3) %>% ungroup

dataPA1x = data3 %>% filter(NAME %in% unique(dataPA1$NAME)) %>% distinct(NAME,agg)
dataPA1x = dataPA1x %>% group_by(NAME) %>% summarize(criterion1.aggregations = paste0(agg, collapse = ",")) %>%
  ungroup



data2 = data1 %>% filter(is.na(NAME), OBSERVATION.COUNT != "X") %>% 
  mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
  group_by(gridg1,group.id,month,cyear) %>% summarize(agg = sum(OBSERVATION.COUNT)) %>% ungroup

data3 = data2 %>% filter(agg >= 2000)

dataNPA1 = data3 %>% distinct(gridg1,month,cyear) %>% group_by(gridg1) %>% summarize(count = n()) %>%
  filter(count >= 3) %>% ungroup

dataNPA1x = data3 %>% filter(gridg1 %in% unique(dataNPA1$gridg1)) %>% distinct(gridg1,agg)
dataNPA1x = dataNPA1x %>% group_by(gridg1) %>% summarize(criterion1.aggregations = paste0(agg, collapse = ",")) %>%
  ungroup





# criterion 2 
# Great Knot, Sociable Lapwing, Spoon-billed Sandpiper, Wood Snipe in PAs

data2 = data1 %>% 
  filter(!is.na(NAME), IUCN.Category %in% c("Vulnerable","Endangered","Critically Endangered")) %>% 
  group_by(NAME,COMMON.NAME,group.id,month,cyear) %>% ungroup

data3 = data2 %>% distinct(NAME,COMMON.NAME,group.id,month,cyear)

dataPA2 = data3 %>% distinct(NAME,month,cyear) %>% group_by(NAME) %>% summarize(count = n()) %>%
  filter(count >= 3) %>% ungroup

dataPA2x = data3 %>% filter(NAME %in% dataPA2$NAME) %>% distinct(NAME,COMMON.NAME)
dataPA2x = dataPA2x %>% group_by(NAME) %>% 
  summarize(criterion2.threatened = paste0(COMMON.NAME, collapse = ",")) %>% ungroup



# Great Knot, Sociable Lapwing, Wood Snipe in NPAs
data2 = data1 %>% 
  filter(is.na(NAME), IUCN.Category %in% c("Vulnerable","Endangered","Critically Endangered")) %>% 
  group_by(gridg1,COMMON.NAME,group.id,month,cyear) %>% ungroup

data3 = data2 %>% distinct(gridg1,COMMON.NAME,group.id,month,cyear)

dataNPA2 = data3 %>% distinct(gridg1,month,cyear) %>% group_by(gridg1) %>% summarize(count = n()) %>%
  filter(count >= 3) %>% ungroup

dataNPA2x = data3 %>% filter(gridg1 %in% dataNPA2$gridg1) %>% distinct(gridg1,COMMON.NAME)
dataNPA2x = dataNPA2x %>% group_by(gridg1) %>% 
  summarize(criterion2.threatened = paste0(COMMON.NAME, collapse = ",")) %>% ungroup





# criterion 3 
# Black-tailed Godwit, Curlew Sandpiper, Bar-tailed Godwit, Eurasian Curlew, Eurasian Oystercatcher,
# Northern Lapwing, River Lapwing, Great Thick-knee, Red Knot, Asian Dowitcher, Red-necked Stint,
# Beach Thick-knee, Gray-tailed Tattler in PAs

data2 = data1 %>% 
  filter(!is.na(NAME), IUCN.Category %in% c("Near Threatened"), OBSERVATION.COUNT != "X") %>% 
  mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
  group_by(NAME,group.id,month,cyear,COMMON.NAME) %>% ungroup

data2 = left_join(data2,pointfivepercent)

data3 = data2 %>% filter(OBSERVATION.COUNT >= thresh05)

dataPA3 = data3 %>% distinct(NAME,month,cyear) %>% group_by(NAME) %>% summarize(count = n()) %>%
  filter(count >= 3) %>% ungroup

dataPA3x = data3 %>% filter(NAME %in% dataPA3$NAME) %>% distinct(NAME,COMMON.NAME)
dataPA3x = dataPA3x %>% group_by(NAME) %>% 
  summarize(criterion3.nearthreatened = paste0(COMMON.NAME, collapse = ",")) %>% ungroup



# Black-tailed Godwit, Curlew Sandpiper, Bar-tailed Godwit, Eurasian Curlew, Eurasian Oystercatcher,
# Northern Lapwing, River Lapwing, Great Thick-knee, Red Knot, Asian Dowitcher, Red-necked Stint,
# Beach Thick-knee, Gray-tailed Tattler in PAs

data2 = data1 %>% 
  filter(is.na(NAME), IUCN.Category %in% c("Near Threatened"), OBSERVATION.COUNT != "X") %>% 
  mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
  group_by(gridg1,group.id,month,cyear,COMMON.NAME) %>% ungroup

data2 = left_join(data2,pointfivepercent)

data3 = data2 %>% filter(OBSERVATION.COUNT >= thresh05)

dataNPA3 = data3 %>% distinct(gridg1,month,cyear) %>% group_by(gridg1) %>% summarize(count = n()) %>%
  filter(count >= 3) %>% ungroup

dataNPA3x = data3 %>% filter(gridg1 %in% dataNPA3$gridg1) %>% distinct(gridg1,COMMON.NAME)
dataNPA3x = dataNPA3x %>% group_by(gridg1) %>% 
  summarize(criterion3.nearthreatened = paste0(COMMON.NAME, collapse = ",")) %>% ungroup


names(dataNPA1x)[1] = "NAME"
names(dataNPA2x)[1] = "NAME"
names(dataNPA3x)[1] = "NAME"
dataNPA1x$NAME = as.character(dataNPA1x$NAME)
dataNPA2x$NAME = as.character(dataNPA2x$NAME)
dataNPA3x$NAME = as.character(dataNPA3x$NAME)



locs = read.csv("localities_PA_NPA_final.csv")


lpa = unique(c(dataPA1x$NAME,dataPA2x$NAME,dataPA3x$NAME))
PAx = data.frame(NAME = lpa)
PAx = left_join(PAx,locs)
PAx = PAx %>% group_by(NAME) %>% 
  summarize(locations = paste0(LOCALITY, collapse = ",")) %>% ungroup
PAx = left_join(PAx,dataPA1x)
PAx = left_join(PAx,dataPA2x)
PAx = left_join(PAx,dataPA3x)


lnpa = unique(c(dataNPA1x$NAME,dataNPA2x$NAME,dataNPA3x$NAME))
NPAx = data.frame(NAME = lnpa)
NPAx$NAME = as.character(NPAx$NAME)
NPAx = left_join(NPAx,locs)
NPAx = NPAx %>% group_by(NAME) %>% 
  summarize(locations = paste0(LOCALITY, collapse = ",")) %>% ungroup
NPAx = left_join(NPAx,dataNPA1x)
NPAx = left_join(NPAx,dataNPA2x)
NPAx = left_join(NPAx,dataNPA3x)


final_PA_NPA = rbind(PAx,NPAx)
write.csv(final_PA_NPA,"final_PA_NPA.csv",row.names=F)




## plot PAs with information


PAx$NAME = as.character(PAx$NAME)
effortmap1 = merge(pamap,PAx, by =  "NAME")
effortmap1 = effortmap1[effortmap1@data$NAME %in% PAx$NAME,]
effortmap1$type = "Protected Area"

gridmapg1$REP_AREA = 625
effortmap2 = merge(gridmapg1,NPAx, by = "NAME")
effortmap2 = effortmap2[effortmap2@data$NAME %in% NPAx$NAME,]
effortmap2$type = "Outside Protected Area"

proj4string(effortmap1) = "+proj=longlat +datum=WGS84"
proj4string(effortmap2) = "+proj=longlat +datum=WGS84"


effortmap = rbind(effortmap1,effortmap2)


mapviewOptions(fgb = FALSE)
a = mapView(effortmap, zcol = "type", map.types = c("Esri.WorldImagery","OpenTopoMap"),
            layer.name = "Protection Status",
            popup = leafpop::popupTable(effortmap,c("type","NAME","locations","criterion1.aggregations",
                                                    "criterion2.threatened","criterion3.nearthreatened","REP_AREA"), 
                                        feature.id=FALSE, 
                                        row.numbers=FALSE),
            alpha.regions = 0.5, lwd = 2)
mapshot(a, "shorebirds_PA_NPA.html")



## plot PAs

require(tidyverse)
require(ggfortify)
require(rgdal)
require(sp)
require(sf)
require(rgeos)
require(mapview)

proj4string(pamap) = "+proj=longlat +datum=WGS84"

mapviewOptions(fgb = FALSE)
a = mapView(pamap, zcol = NULL, map.types = c("Esri.WorldImagery","OpenTopoMap"),
            layer.name = NULL, 
            popup = leafpop::popupTable(pamap,c("NAME"), 
                                        feature.id=FALSE, 
                                        row.numbers=FALSE), 
            alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
mapshot(a, "pamap.html")







####################### create locations file

data2 = data1 %>% filter(!is.na(NAME), OBSERVATION.COUNT != "X") %>% 
  mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
  group_by(NAME,LOCALITY,group.id,month,cyear) %>% summarize(agg = sum(OBSERVATION.COUNT)) %>% ungroup

data3 = data2 %>% filter(agg >= 2000)

a1 = data3 %>% distinct(NAME,LOCALITY)


data2 = data1 %>% filter(is.na(NAME), OBSERVATION.COUNT != "X") %>% 
  mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
  group_by(gridg1,LOCALITY,group.id,month,cyear) %>% summarize(agg = sum(OBSERVATION.COUNT)) %>% ungroup

data3 = data2 %>% filter(agg >= 2000)

b1 = data3 %>% distinct(gridg1,LOCALITY)


data2 = data1 %>% 
  filter(!is.na(NAME), IUCN.Category %in% c("Vulnerable","Endangered","Critically Endangered")) %>% 
  group_by(NAME,LOCALITY,COMMON.NAME,group.id,month,cyear) %>% ungroup

data3 = data2 %>% distinct(NAME,LOCALITY,COMMON.NAME,group.id,month,cyear)

a2 = data3 %>% distinct(NAME,LOCALITY)



data2 = data1 %>% 
  filter(is.na(NAME), IUCN.Category %in% c("Vulnerable","Endangered","Critically Endangered")) %>% 
  group_by(gridg1,LOCALITY,COMMON.NAME,group.id,month,cyear) %>% ungroup

data3 = data2 %>% distinct(gridg1,LOCALITY,COMMON.NAME,group.id,month,cyear)

b2 = data3 %>% distinct(gridg1,LOCALITY)



data2 = data1 %>% 
  filter(!is.na(NAME), IUCN.Category %in% c("Near Threatened"), OBSERVATION.COUNT != "X") %>% 
  mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
  group_by(NAME,LOCALITY,group.id,month,cyear,COMMON.NAME) %>% ungroup

data2 = left_join(data2,pointfivepercent)

data3 = data2 %>% filter(OBSERVATION.COUNT >= thresh05)

a3 = data3 %>% distinct(NAME,LOCALITY)



data2 = data1 %>% 
  filter(is.na(NAME), IUCN.Category %in% c("Near Threatened"), OBSERVATION.COUNT != "X") %>% 
  mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
  group_by(gridg1,LOCALITY,group.id,month,cyear,COMMON.NAME) %>% ungroup

data2 = left_join(data2,pointfivepercent)

data3 = data2 %>% filter(OBSERVATION.COUNT >= thresh05)

b3 = data3 %>% distinct(gridg1,LOCALITY)


a = rbind(a1,a2,a3)
b = rbind(b1,b2,b3)
names(b)[1] = "NAME"

datax = rbind(a,b)


datax1 = datax %>%
  distinct(NAME,LOCALITY) %>% filter(NAME %in% PAx$NAME | NAME %in% NPAx$id)

write.csv(datax1,"localities_PA_NPA.csv",row.names = F)








######## standardizing locations

library(tidyverse)
require(ggfortify)
require(rgdal)
require(sp)
require(sf)
require(rgeos)
require(mapview)


locmap = read.csv("location_map_f.csv")
datalocs = read.csv("EastIndia_allsites.csv")

datalocs = left_join(datalocs,locmap)

write.csv(datalocs,"datalocs_f.csv",row.names = F)


coordinates(locmap) = ~LONGITUDE + LATITUDE

proj4string(locmap) = "+proj=longlat +datum=WGS84"

mapviewOptions(fgb = FALSE)
a = mapView(locmap, zcol = c("Standardized.Site.Name"), map.types = c("Esri.WorldImagery"),
            layer.name = c("Locations"), 
            popup = leafpop::popupTable(locmap,c("LOCALITY"), 
                                        feature.id=FALSE, 
                                        row.numbers=FALSE))

mapshot(a, "shorebird_locs.html")
