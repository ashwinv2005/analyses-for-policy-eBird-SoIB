load("data.RData")
load("maps.RData")
load("clips.RData")

require(tidyverse)
require(rgdal)
require(sp)
require(sf)
require(rgeos)
require(extrafont)

soib = read.csv("stateofindiasbirdsfull.csv")
map = read.csv("Map to Other Lists - map.csv")
map1 = map %>%
  filter(!eBird.English.Name.2018 %in% c("Sykes's Short-toed Lark","Green Warbler","Sykes's Warbler",
                                         "Taiga Flycatcher","Chestnut Munia","Desert Whitethroat",
                                         "Hume's Whitethroat","Changeable Hawk-Eagle")) %>%
  dplyr::select(eBird.English.Name.2019,India.Checklist.Name)


soib = left_join(soib,map1,by = c("Common.Name" = "India.Checklist.Name"))
soib = soib %>% select(eBird.English.Name.2019,Concern.Status)

data = left_join(data,soib,by = c("COMMON.NAME" = "eBird.English.Name.2019"))

districtmap@data$DISTRICT[districtmap@data$DISTRICT == "Lower Dibang Valley"] = "Dibang Valley"
data$DISTRICT[data$DISTRICT == "Lower Dibang Valley"] = "Dibang Valley"

data$Concern.Status = factor(data$Concern.Status, levels = c("High","Moderate","Low"))

g3clipsum = data %>%
  filter(!is.na(g3clip),!is.na(Concern.Status)) %>%
  group_by(g3clip,Concern.Status) %>% summarize(n = n_distinct(COMMON.NAME))%>%
  arrange(Concern.Status)

g3clipcomparison = g3clipsum %>%
  filter(Concern.Status == "High") %>%
  arrange(desc(n))

pasum = data %>%
  filter(!is.na(NAME),!is.na(Concern.Status)) %>%
  group_by(NAME,Concern.Status) %>% summarize(n = n_distinct(COMMON.NAME))%>%
  arrange(Concern.Status)

pacomparison = pasum %>%
  filter(Concern.Status == "High") %>%
  arrange(desc(n))

districtsum = data %>%
  filter(!is.na(DISTRICT),!is.na(Concern.Status)) %>%
  group_by(DISTRICT,Concern.Status) %>% summarize(n = n_distinct(COMMON.NAME))%>%
  arrange(Concern.Status)

districtcomparison = districtsum %>%
  filter(Concern.Status == "High") %>%
  arrange(desc(n))



require(viridis)

fortified = fortify(g3clip, region = c("id"))
g3clipcomparison$g3clip = as.character(g3clipcomparison$g3clip)
plotg3 = na.omit(left_join(fortified,g3clipcomparison, by = c('id' = "g3clip"))) # SPDF to plot

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
  theme(text=element_text(family="Gill Sans MT"), legend.title = element_text(size = 12)) +
  scale_fill_viridis(name = "no. of species of\nHigh Conservation\nConcern (SoIB)")+
  coord_map()

print(plotg3map)
ggsave(file="total_high_concern.tiff", units="in", width=11, height=7)
dev.off()

fortified = fortify(districtmap, region = c("DISTRICT"))
districtcomparison$DISTRICT = as.character(districtcomparison$DISTRICT)
plotdis = na.omit(left_join(fortified,districtcomparison, by = c('id' = "DISTRICT"))) # SPDF to plot

plotdismap = ggplot() +
  #geom_polygon(data = pamap, aes(x=long, y=lat, group=group), colour = 'black', fill = NA)+  
  geom_polygon(data = plotdis, aes(x=long, y=lat, group=group,fill = n), colour = NA)+  
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
  theme(text=element_text(family="Gill Sans MT"), legend.title = element_text(size = 12)) +
  scale_fill_viridis(name = "no. of species of\nHigh Conservation\nConcern (SoIB)")+
  coord_map()

print(plotdismap)
ggsave(file="total_high_concern_district.tiff", units="in", width=11, height=7)
dev.off()

require(rgeos)

fortified = fortify(gBuffer(pamap, byid=TRUE, width=0), region = "NAME")
pacomparison$NAME = as.character(pacomparison$NAME)
plotpa = na.omit(left_join(fortified,pacomparison, by = c('id' = "NAME"))) # SPDF to plot

plotpamap = ggplot() +
  #geom_polygon(data = pamap, aes(x=long, y=lat, group=group), colour = 'black', fill = NA)+  
  geom_polygon(data = plotpa, aes(x=long, y=lat, group=group,fill = n), colour = NA)+  
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
  theme(text=element_text(family="Gill Sans MT")) +
  scale_fill_viridis(name = "no. of species of\nHigh Conservation\nConcern (SoIB)")+
  coord_map()

print(plotpamap)
ggsave(file="total_high_concern_pa.png", units="in", width=11, height=7)
dev.off()


############################################### Overall richness



g3clipsum = data %>%
  filter(!is.na(g3clip),CATEGORY %in% c("species","issf")) %>%
  group_by(g3clip) %>% summarize(n = n_distinct(COMMON.NAME))%>%
  arrange(desc(n))

g3clipcomparison = g3clipsum

pasum = data %>%
  filter(!is.na(NAME),CATEGORY %in% c("species","issf")) %>%
  group_by(NAME) %>% summarize(n = n_distinct(COMMON.NAME))%>%
  arrange(desc(n))

pacomparison = pasum

districtsum = data %>%
  filter(!is.na(DISTRICT),CATEGORY %in% c("species","issf")) %>%
  group_by(DISTRICT) %>% summarize(n = n_distinct(COMMON.NAME))%>%
  arrange(desc(n))

districtcomparison = districtsum



require(viridis)

fortified = fortify(g3clip, region = c("id"))
g3clipcomparison$g3clip = as.character(g3clipcomparison$g3clip)
plotg3 = na.omit(left_join(fortified,g3clipcomparison, by = c('id' = "g3clip"))) # SPDF to plot

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
  theme(text=element_text(family="Gill Sans MT"), legend.title = element_text(size = 12)) +
  scale_fill_viridis(name = "no. of species")+
  coord_map()

print(plotg3map)
ggsave(file="total_species.tiff", units="in", width=11, height=7)
dev.off()

fortified = fortify(districtmap, region = c("DISTRICT"))
districtcomparison$DISTRICT = as.character(districtcomparison$DISTRICT)
plotdis = na.omit(left_join(fortified,districtcomparison, by = c('id' = "DISTRICT"))) # SPDF to plot

plotdismap = ggplot() +
  #geom_polygon(data = pamap, aes(x=long, y=lat, group=group), colour = 'black', fill = NA)+  
  geom_polygon(data = plotdis, aes(x=long, y=lat, group=group,fill = n), colour = NA)+  
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
  theme(text=element_text(family="Gill Sans MT"), legend.title = element_text(size = 12)) +
  scale_fill_viridis(name = "no. of species")+
  coord_map()

print(plotdismap)
ggsave(file="total_species_district.tiff", units="in", width=11, height=7)
dev.off()

fortified = fortify(gBuffer(pamap, byid=TRUE, width=0), region = "NAME")
pacomparison$NAME = as.character(pacomparison$NAME)
plotpa = na.omit(left_join(fortified,pacomparison, by = c('id' = "NAME"))) # SPDF to plot

plotpamap = ggplot() +
  #geom_polygon(data = pamap, aes(x=long, y=lat, group=group), colour = 'black', fill = NA)+  
  geom_polygon(data = plotpa, aes(x=long, y=lat, group=group,fill = n), colour = NA)+  
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
  theme(text=element_text(family="Gill Sans MT")) +
  scale_fill_viridis(name = "no. of species")+
  coord_map()

print(plotpamap)
ggsave(file="total_species_pa.png", units="in", width=11, height=7)
dev.off()


######################### Species in MWLS

spmehao = data %>%
  filter(!is.na(NAME),CATEGORY %in% c("species","issf"),NAME == "Mehao") %>%
  summarize(n = n_distinct(COMMON.NAME))

######################### Species in DWLS

spdibang = data %>%
  filter(!is.na(NAME),CATEGORY %in% c("species","issf"),NAME == "Dibang") %>%
  summarize(n = n_distinct(COMMON.NAME))

######################### Species pa

sppa = data %>%
  filter(NAME %in% c("Mehao","Dibang Valley"),
         CATEGORY %in% c("species","issf")) %>%
  summarize(n = n_distinct(COMMON.NAME))

######################### Species outside

spoutside = data %>%
  filter(DISTRICT %in% c("Dibang Valley"),!NAME %in% c("Mehao","Dibang Valley"),
         CATEGORY %in% c("species","issf")) %>%
  summarize(n = n_distinct(COMMON.NAME))

######################## Species total

sptotal = data %>%
  filter(DISTRICT %in% c("Dibang Valley"),
         CATEGORY %in% c("species","issf")) %>%
  summarize(n = n_distinct(COMMON.NAME))

######################## High Concern species total

sphightotal = data %>%
  filter(DISTRICT %in% c("Dibang Valley"),
         CATEGORY %in% c("species","issf"),Concern.Status == "High") %>%
  distinct(COMMON.NAME)


###################### Dibang checklist

dibangchecklist = data %>%
  filter(DISTRICT %in% c("Dibang Valley"),
         CATEGORY %in% c("species","issf")) %>%
  distinct(COMMON.NAME,Concern.Status) %>%
  filter(COMMON.NAME != "Assam Laughingthrush")

clem = read.csv("eBird-Clements-v2019-integrated-checklist-August-2019.csv")

clem = clem %>% filter(category %in% c("species")) %>% select(English.name,scientific.name,eBird.species.group) %>%
  filter(English.name %in% dibangchecklist$COMMON.NAME)

dibangchecklist = left_join(clem,dibangchecklist,by = c("English.name" = "COMMON.NAME"))
dibangchecklist = left_join(dibangchecklist,map,by = c("English.name" = "eBird.English.Name.2019"))

dibangchecklist = dibangchecklist %>% select(eBird.species.group,English.name,scientific.name,
                                             Concern.Status,IUCN,Schedule)

names(dibangchecklist) = c("Group","Common Name","Scientific Name","SoIB Concern Status","IUCN Red List","WLPA Schedule")
write.csv(a,"Dibang Valley Bird Checklist.csv",row.names = F)


##################################

require(tidyverse)
a = read.csv("dibcheck.csv")
clem = read.csv("eBird-Clements-v2019-integrated-checklist-August-2019.csv")

clem = clem %>% select(English.name,family)

a = left_join(a,clem,by = c("Common.Name" = "English.name"))

ldv = unique(data[data$DISTRICT == "Lower Dibang Valley" & data$CATEGORY %in% c("species","issf"),]$COMMON.NAME)
dv = unique(data[data$DISTRICT == "Dibang Valley" & data$CATEGORY %in% c("species","issf"),]$COMMON.NAME)

ldv1 = setdiff(ldv,dv)
dv1 = setdiff(dv,ldv)

a$ldv = ""
a$ldv[a$Common.Name %in% ldv1] = "*"
a$dv = ""
a$dv[a$Common.Name %in% dv1] = "!"