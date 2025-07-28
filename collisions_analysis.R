#### analyses

library(lubridate)
library(tidyverse)

coll_eBird = read.csv("collisions_eBird.csv")
coll_minus_eBird = read.csv("collisions_minus_eBird.csv")

coll = rbind(coll_eBird,coll_minus_eBird)


mp = read.csv("SoIB_mapping_2022.csv")

coll = left_join(coll,mp,by = c("COMMON.NAME" = "eBird.English.Name.2022"))


total = length(unique(coll[coll$CATEGORY %in% c("species","issf"),]$COMMON.NAME))

table(coll$source)
sort(table(coll$COMMON.NAME))

tab = coll %>%
  mutate(lists = n()) %>%
  group_by(COMMON.NAME) %>% summarize(n = round(100*n()/max(lists))) %>%
  arrange(desc(n))

months = coll %>%
  filter(year != 1990)
months_BBDK = coll %>%
  filter(COMMON.NAME == "Black-backed Dwarf-Kingfisher", year != 1990) %>% select(month)
months_INPI = coll %>%
  filter(COMMON.NAME == "Indian Pitta", year != 1990) %>% select(month)
months_ALL = coll %>%
  filter(!COMMON.NAME %in% c("Black-backed Dwarf-Kingfisher","Indian Pitta"), year != 1990) %>% select(month)



hist(months_BBDK$month)
hist(months_INPI$month)
hist(months_ALL$month)

months_BBDK$species = "Black-backed Dwarf-Kingfisher"
months_INPI$species = "Indian Pitta"
months_ALL$species = "All species except Dwarf-Kingfisher and Pitta"

months_all = rbind(months_BBDK,months_INPI,months_ALL)

library(ggthemes)
theme_set(theme_tufte())

cols = c("#869B27", "#E49B36", "#A13E2B", "#78CAE0", "#B69AC9", "#EA5599", "#31954E", "#493F3D",
         "#CC6666", "#9999CC", "#000000", "#66CC99")

is.extrafont.installed <- function(){
  if(is.element("extrafont", installed.packages()[,1])){
    library(extrafont)
    # probably need something here to run font_import()
    return(T)
  }else{
    warning("Library extrafont installed; using system sans/serif libraries as fallback fonts. 
    To enable full font support, run: 
      install.packages('extrafont') 
      font_import()")
    return(F)
  }
}

base_font_family_tufte <- function(){
  if(is.extrafont.installed()){
    library(extrafont)
    tuftefont <- choose_font(c("Gill Sans MT", "Gill Sans", "GillSans", "Verdana", "serif"), quiet = FALSE)  
  }else{
    tuftefont <- "serif"
  }
  return(tuftefont)
}

theme_tufte_revised <- function(base_size = 11, base_family = base_font_family_tufte(), ticks = TRUE) {
  
  ret <- theme_bw(base_family = base_family, base_size = base_size) + 
    theme(
      axis.line = element_line(color = 'black'),
      axis.title.x = element_text(vjust = -0.3), 
      axis.title.y = element_text(vjust = 0.8),
      legend.background = element_blank(), 
      legend.key = element_blank(), 
      legend.title = element_text(face="plain"),
      panel.background = element_blank(), 
      panel.border = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_blank(),
      strip.background = element_blank()
    )
  
  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
  }
  
  ret
} 

require(extrafont)

pd = position_dodge(1)
ggp = ggplot(data = months_all[months_all$species == "All species except Dwarf-Kingfisher and Pitta",], aes(fill = species)) +
  geom_histogram(aes(x = month), bins = 13, position = pd, breaks = 0:12, col = "black") +
  xlab("Month") +
  ylab("Count")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 15)) +
  scale_fill_manual(values = cols[2]) +
  scale_x_continuous(breaks = 0:12, labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
                                               "Sep","Oct","Nov","Dec","")) +
  scale_y_continuous(breaks = seq(0,80,5)) +
  theme(legend.position = "bottom")

n1 = "collisions_all.jpg"

print(ggp1)
ggsave(file=n1, units="in", width=10, height=7)


ggp = ggplot(data = months_all[!months_all$species == "All species except Dwarf-Kingfisher and Pitta",], aes(fill = species)) +
  geom_histogram(aes(x = month), bins = 13, position = pd, breaks = 0:12, col = "black") +
  xlab("Month") +
  ylab("Count")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 15)) +
  scale_fill_manual(values = cols[c(1,3)]) +
  scale_x_continuous(breaks = 0:12, labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
                                               "Sep","Oct","Nov","Dec","")) +
  scale_y_continuous(breaks = 1:18) +
  theme(legend.position = "bottom")

n1 = "collisions_kp.jpg"

print(ggp1)
ggsave(file=n1, units="in", width=10, height=7)

####### plot map

require(sp)
require(rgeos)
require(ggfortify)
require(rgdal)
require(sf)

load("maps.RData")

districtmap1 = gSimplify(districtmap, tol=0.01, topologyPreserve=TRUE)
d1 = districtmap@data
districtmap1 = sp::SpatialPolygonsDataFrame(districtmap1, d1)
statemap1 = gSimplify(statemap, tol=0.01, topologyPreserve=TRUE)
s1 = statemap@data
statemap1 = sp::SpatialPolygonsDataFrame(statemap1, s1)

load("clips.RData")

data = coll

data = data %>% filter(!is.na(group.id), !is.na(LATITUDE), !is.na(LONGITUDE))

# add columns with DISTRICT and ST_NM to main data 

temp = data %>% group_by(group.id) %>% slice(1) # same group ID, same grid/district/state 

rownames(temp) = temp$group.id # only to setup adding the group.id column for the future left_join
coordinates(temp) = ~LONGITUDE + LATITUDE # convert to SPDF?
proj4string(temp) = "+proj=longlat +datum=WGS84"
temp = over(temp,districtmap) # returns only ATTRIBUTES of districtmap (DISTRICT and ST_NM)
temp = data.frame(temp) # convert into data frame for left_join
temp = temp[,1:2]
temp$group.id = rownames(temp) # add column to join with the main data
data = left_join(temp,data)

ggp = ggplot() +
  geom_polygon(data = statemap1, aes(x=long, y=lat, group=group), colour = "black", fill = "white")+  
  geom_point(data = data, aes(x=LONGITUDE,y=LATITUDE), colour = "red", size = 2) +
  #scale_x_continuous(expand = c(0,0)) +
  #scale_y_continuous(expand = c(0,0)) +
  theme_bw()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"),
        #panel.border = element_blank(),
        plot.background = element_rect(fill = NA,colour = NA),
        panel.background = element_rect(fill = NA, colour = NA),
        plot.title = element_text(hjust = 0.5))+
  coord_map()

n1 = "collisions_map.jpg"

print(ggp)
ggsave(file=n1, units="in", width=8, height=11)
