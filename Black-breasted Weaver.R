######################### simple plotting

require(tidyverse)
require(sp)
require(rgeos)
require(sf)

bbwe = read.csv("BBWeMediaf.csv")
bbwe = bbwe %>% select(-g2clip,-g3clip) %>% filter(Type != "transition")
data = bbwe
load("clips.RData")
load("maps.RData")

data$sl = 1:length(data$ASSET.ID)

data = data %>%
  mutate(group.id1 = ifelse(group.id == "", sl, group.id))

temp = data %>% group_by(group.id1) %>% slice(1)

rownames(temp) = temp$group.id1
coordinates(temp) = ~LONGITUDE + LATITUDE
temp = over(temp,g2clip)
temp = data.frame(temp)
temp$group.id1 = rownames(temp)
data = left_join(temp,data)
names(data)[1] = "g2clip"

temp = data %>% group_by(group.id1) %>% slice(1)

rownames(temp) = temp$group.id1
coordinates(temp) = ~LONGITUDE + LATITUDE
temp = over(temp,g3clip)
temp = data.frame(temp)
temp$group.id1 = rownames(temp)
data = left_join(temp,data)
names(data)[1] = "g3clip"

temp = data %>% group_by(group.id1) %>% slice(1)

rownames(temp) = temp$group.id1
coordinates(temp) = ~LONGITUDE + LATITUDE
temp = over(temp,gridmapg2)
temp = data.frame(temp)
temp$group.id1 = rownames(temp)
data = left_join(temp,data)
names(data)[1] = "gridg2"

temp = data %>% group_by(group.id1) %>% slice(1)

rownames(temp) = temp$group.id1
coordinates(temp) = ~LONGITUDE + LATITUDE
temp = over(temp,gridmapg3)
temp = data.frame(temp)
temp$group.id1 = rownames(temp)
data = left_join(temp,data)
names(data)[1] = "gridg3"

g2extra = data %>% filter(is.na(g2clip))
g2extra = g2extra$gridg2
g2extra = gridmapg2[gridmapg2@data$id %in% g2extra,]
g2extra = rbind(g2clip,g2extra)

g3extra = data %>% filter(is.na(g3clip))
g3extra = g3extra$gridg3
g3extra = gridmapg3[gridmapg3@data$id %in% g3extra,]
g3extra = rbind(g3clip,g3extra)

data = data %>%
  mutate(g2clip1 = ifelse(is.na(g2clip), gridg2, g2clip))

data = data %>%
  mutate(g3clip1 = ifelse(is.na(g3clip), gridg3, g3clip))

data = data %>%
  select(group.id1,LONGITUDE,LATITUDE,g2clip1,g3clip1,type)
names(data) = c("group.id","LONGITUDE","LATITUDE","g2clip","g3clip","type")

datag1 = data %>%
  distinct(g2clip,type)

datag2 = data %>%
  distinct(g3clip,type)

tp1 = datag1 %>% filter(!is.na(type)) %>% group_by(g2clip) %>% summarize(l = n())
tp1 = tp1 %>% filter(l > 1) %>% select(g2clip)
tp1$type = "B/W"

tp2 = datag2 %>% filter(!is.na(type)) %>% group_by(g3clip) %>% summarize(l = n())
tp2 = tp2 %>% filter(l > 1) %>% select(g3clip)
tp2$type = "B/W"

datap = data %>%
  distinct(LONGITUDE,LATITUDE,type)

require(extrafont)
require(ggthemes)
theme_set(theme_tufte())

require(magick)
require(cowplot)

a = image_read("BBWE1.jpg")
a = image_scale(a, "300")
#a = image_border(a, "#ffffff", "3x3")
#a = image_annotate(a, "Siddu Verma", font = 'Gill Sans', size = 20, location = "+8+276")

b = image_read("BBWE2.png")
b = image_scale(b, "300")
#b = image_border(b, "#ffffff", "2x2")
#b = image_annotate(b, "Mukesh Sehgal", font = 'Gill Sans', size = 20, location = "+8+276")

datag = datag1 %>% filter(!g2clip %in% tp1$g2clip)
datag = rbind(datag,tp1)

fortified = fortify(g2extra, region = c("id"))
datag$g2clip = as.character(datag$g2clip)
plotg2 = na.omit(left_join(fortified,datag, by = c('id' = "g2clip"))) # SPDF to plot

colx =  "#92972f"

plotg2map = ggplot() +
  geom_polygon(data = indiamap, aes(x = long, y = lat, group = group), col = "black",fill = colx, size = 1) +
  geom_polygon(data = plotg2, aes(x=long, y=lat, group=group,fill = type), colour = NA)+  
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
        panel.background = element_rect(fill = colx, colour = NA),
        plot.background = element_rect(fill = colx, colour = NA))+
  theme(text=element_text(family="Gill Sans MT"), legend.title = element_text(size = 16),
        legend.text = element_text(size = 12)) +
  scale_fill_manual(name = "breeding plumage", breaks = c("B","W","B/W"), 
                    labels = c("Black-headed","White-headed","both"),
                    values = c("black","white","grey"))+
  theme(legend.position = "bottom")+
  coord_map()

datag = datag2 %>% filter(!g3clip %in% tp2$g3clip)
datag = rbind(datag,tp2)

fortified = fortify(g3extra, region = c("id"))
datag$g3clip = as.character(datag$g3clip)
plotg3 = na.omit(left_join(fortified,datag, by = c('id' = "g3clip"))) # SPDF to plot

plotg3map = ggplot() +
  geom_polygon(data = indiamap, aes(x = long, y = lat, group = group), col = "#582603",fill = colx, size = 1) +
  geom_polygon(data = plotg3, aes(x=long, y=lat, group=group,fill = type), colour = NA)+  
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
        plot.margin=unit(c(0.5,1,0,1.8), "cm"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = colx, colour = NA),
        plot.background = element_rect(fill = colx, colour = NA))+
  theme(text=element_text(family="Gill Sans MT"), legend.title = element_text(size = 16),
        legend.text = element_text(size = 12)) +
  scale_fill_manual(name = "breeding plumage", breaks = c("B","W","B/W"), 
                    labels = c("Black-faced","White-faced","both"),
                    values = c("black","white","grey"))+
  theme(legend.position = "bottom")+
  coord_map()

plotg3map1 = ggdraw(plotg3map) + draw_image(a, x = 0.84, y = 0.45, hjust = 1, vjust = 0.9, width = 0.15, height = 0.15) +
  draw_image(b, x = 0.21, y = 0.88, hjust = 1, vjust = 0.9, width = 0.15, height = 0.15)

print(plotg3map1)
ggsave(file="BBWEmap.jpeg", units="in", width=6.6, height=7)
dev.off()  
