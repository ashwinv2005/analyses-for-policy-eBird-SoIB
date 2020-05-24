source('~/GitHub/vulture-analysis/functions.R')
createmaps()
source('~/GitHub/vulture-analysis/functions.R')
readcleanrawdata("ebd_IN_relApr-2020.txt","Sensitive_India_may 2019.csv") 
source('~/GitHub/vulture-analysis/functions.R')
addmapvars()



#################################



load("data.RData")
load("maps.RData")

require(tidyverse)
require(rgdal)
require(sp)
require(sf)

soib = read.csv("stateofindiasbirdsfull.csv")
map = read.csv("Map to Other Lists - map.csv")
map = map %>%
  filter(!eBird.English.Name.2018 %in% c("Sykes's Short-toed Lark","Green Warbler","Sykes's Warbler",
                                         "Taiga Flycatcher","Chestnut Munia","Desert Whitethroat",
                                         "Hume's Whitethroat","Changeable Hawk-Eagle")) %>%
  dplyr::select(eBird.English.Name.2019,India.Checklist.Name)


soib = left_join(soib,map,by = c("Common.Name" = "India.Checklist.Name"))
soib = soib %>% select(eBird.English.Name.2019,Concern.Status,IUCN)

data = left_join(data,soib,by = c("COMMON.NAME" = "eBird.English.Name.2019"))

data$Concern.Status = factor(data$Concern.Status, levels = c("High","Moderate","Low"))

pasum = data %>%
  filter(!is.na(NAME),!is.na(Concern.Status)) %>%
  group_by(NAME,Concern.Status) %>% summarize(n = n_distinct(COMMON.NAME))%>%
  arrange(Concern.Status)

pacomparison = pasum %>%
  filter(Concern.Status == "High") %>%
  arrange(desc(n))

gridg3sum = data %>%
  filter(!is.na(gridg3),!is.na(Concern.Status)) %>%
  group_by(gridg3,Concern.Status) %>% summarize(n = n_distinct(COMMON.NAME))%>%
  arrange(Concern.Status)

gridg3comparison = gridg3sum %>%
  filter(Concern.Status == "High") %>%
  arrange(desc(n))

dibangpasum = data %>%
  filter(!is.na(NAME),!is.na(Concern.Status),NAME %in% c("Dibang","Mehao")) %>%
  group_by(Concern.Status) %>% summarize(n = n_distinct(COMMON.NAME))%>%
  arrange(Concern.Status)

dibangpaspecies = data %>%
  filter(NAME %in% c("Dibang","Mehao"),!is.na(Concern.Status)) %>%
  distinct(COMMON.NAME, Concern.Status, IUCN) %>%
  arrange(Concern.Status)

dppaspecies = data %>%
  filter(NAME %in% c("Dihing Patkai"),!is.na(Concern.Status)) %>%
  distinct(COMMON.NAME, Concern.Status, IUCN) %>%
  arrange(Concern.Status)



districtsum = data %>%
  filter(!is.na(Concern.Status)) %>%
  group_by(DISTRICT,Concern.Status) %>% summarize(n = n_distinct(COMMON.NAME)) %>%
  arrange(Concern.Status)

districtcomparison = districtsum %>%
  filter(!is.na(DISTRICT),Concern.Status == "High") %>%
  arrange(desc(n))

dibangdistrictsum = data %>%
  filter(!is.na(Concern.Status),DISTRICT %in% c("Lower Dibang Valley","Dibang Valley")) %>%
  group_by(Concern.Status) %>% summarize(n = n_distinct(COMMON.NAME)) %>%
  arrange(Concern.Status)

dibangdistrictspecies = data %>%
  filter(DISTRICT %in% c("Lower Dibang Valley","Dibang Valley"),!is.na(Concern.Status)) %>%
  distinct(COMMON.NAME, Concern.Status, IUCN) %>%
  arrange(Concern.Status)

dpdistrictspecies = data %>%
  filter(DISTRICT %in% c("Tinsukia","Dibrugarh"),!is.na(Concern.Status)) %>%
  distinct(COMMON.NAME, Concern.Status, IUCN) %>%
  arrange(Concern.Status)





write.csv(dibangpaspecies,"dibang_PAs_species.csv",row.names = F)
write.csv(dibangdistrictspecies,"dibang_districts_species.csv",row.names = F)
write.csv(pacomparison,"PA_comparison.csv",row.names = F)
write.csv(districtcomparison,"district_comparison.csv",row.names = F)



statesum = data %>%
  filter(!is.na(Concern.Status)) %>%
  group_by(ST_NM,Concern.Status) %>% summarize(n = n_distinct(COMMON.NAME))


require(viridis)

fortified = fortify(gridmapg3, region = c("id"))
gridg3comparison$gridg3 = as.character(gridg3comparison$gridg3)
plotg3 = na.omit(left_join(fortified,gridg3comparison, by = c('id' = "gridg3"))) # SPDF to plot

plotg3map = ggplot() +
  #geom_polygon(data = pamap, aes(x=long, y=lat, group=group), colour = 'black', fill = NA)+  
  geom_polygon(data = plotg3, aes(x=long, y=lat, group=group,fill = n), colour = NA)+  
  geom_polygon(data = indiamap, aes(x = long, y = lat, group = group), col = "black", fill = NA, size = 1) +
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
  scale_fill_viridis()+
  coord_map()

require(rgeos)

fortified = fortify(gBuffer(pamap), region = "NAME")
pacomparison$NAME = as.character(pacomparison$NAME)
plotpa = na.omit(left_join(fortified,pacomparison, by = c('NAME' = "gridg3"))) # SPDF to plot

plotg3map = ggplot() +
  #geom_polygon(data = pamap, aes(x=long, y=lat, group=group), colour = 'black', fill = NA)+  
  geom_polygon(data = plotg3, aes(x=long, y=lat, group=group,fill = n), colour = NA)+  
  geom_polygon(data = indiamap, aes(x = long, y = lat, group = group), col = "black", fill = NA, size = 1) +
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
  scale_fill_viridis()+
  coord_map()
