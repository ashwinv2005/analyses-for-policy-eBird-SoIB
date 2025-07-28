require(tidyverse)
load("data.RData")

## growth in the last 6 months

data_latest = data %>% filter((eyear == 2021 & month %in% c(1,2,3)) | (eyear == 2020 & month %in% c(10,11,12)))
data_old = data %>% filter(!(eyear == 2021 & month %in% c(1,2,3)) & !(eyear == 2020 & month %in% c(10,11,12))) 

old_lists = length(unique(data_old$SAMPLING.EVENT.IDENTIFIER))
old_unique_lists = length(unique(data_old$group.id))
old_birders = length(unique(data_old$OBSERVER.ID))
old_observations = length(data_old$group.id)
old_complete = length(unique(data_old[data_latest$ALL.SPECIES.REPORTED == 1,]$group.id))

latest_lists = length(unique(data_latest$SAMPLING.EVENT.IDENTIFIER))
latest_unique_lists = length(unique(data_latest$group.id))
latest_birders = length(setdiff(unique(data_latest$OBSERVER.ID),unique(data_old$OBSERVER.ID)))
latest_observations = length(data_latest$group.id)
latest_complete = length(unique(data_latest[data_latest$ALL.SPECIES.REPORTED == 1,]$group.id))

growth_lists = latest_lists*100/(latest_lists + old_lists)
growth_unique_lists = latest_unique_lists*100/(latest_unique_lists + old_unique_lists)
growth_birders = latest_birders*100/(latest_birders + old_birders)
growth_observations = latest_observations*100/(latest_observations + old_observations)
growth_complete = latest_observations*100/(latest_observations + old_observations)


## plot graph

library(lubridate)

tr2 = data %>%
  group_by(eyear) %>% summarize(rec2 = n())

datao = data %>%
  arrange(eyear) %>%
  group_by(OBSERVER.ID) %>% slice(1)

tro = datao %>%
  group_by(eyear) %>% summarize(obs2 = n_distinct(OBSERVER.ID))

trx = data %>%
  filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(eyear) %>% summarize(coli2 = n_distinct(SAMPLING.EVENT.IDENTIFIER))

tr2 = left_join(tr2,tro)
tr2 = left_join(tr2,trx)

tr2$rec2 = cumsum(tr2$rec2)
tr2$obs2 = cumsum(tr2$obs2)
tr2$coli2 = cumsum(tr2$coli2)

editdate1 = data.frame(year = tr2$eyear)
editdate1$type = "observations"
editdate1$met = tr2$rec2
editdate2 = data.frame(year = tr2$eyear)
editdate2$type = "observers"
editdate2$met = tr2$obs2
editdate3 = data.frame(year = tr2$eyear)
editdate3$type = "complete checklists"
editdate3$met = tr2$coli2
editdate = rbind(editdate1,editdate2,editdate3)

editdate$type = factor(editdate$type, levels = c("observers","complete checklists","observations"))

library(extrafont)
library(scales)

ymeta = ggplot(editdate[editdate$type=="observers",], aes(year, met))+
  facet_wrap(. ~ type, scale="free_y", ncol = 1)+
  geom_point()+
  geom_line() +
  xlab("year of upload")+
  ylab("")+
  geom_text(aes(label=met),hjust=0, vjust=1.5)+
  theme_bw()
ymet2a = ymeta +
  theme(axis.title.x = element_blank(),
        #axis.title.x = element_text(vjust = 0.3, size = 30), 
        axis.text.x = element_text(size = 10),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        #axis.title.y = element_text(vjust = 0.3, angle = 90, size = 30), 
        axis.text.y = element_text(size = 14)) +
  theme(legend.position = "none")+
  expand_limits(y=0)+
  scale_x_continuous(expand = c(0,1), breaks = c(2014,2015,2016,2017,2018,2019,2020,2021))+
  scale_y_continuous(expand = c(0.12,0), breaks = 0, labels = 0)+
  geom_hline(yintercept = 0, linetype = "dotted", size = 0.5) +
  theme(text=element_text(family="Gill Sans MT")) +
  #scale_y_log10()+
  #scale_y_continuous(trans = log10_trans(),
  #                   breaks = trans_breaks("log10", function(x) 10^x),
  #                   labels = trans_format("log10", math_format(10^.x)))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )+
  theme(strip.text.x = element_text(size = 15))
#theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))

ymeta = ggplot(editdate[editdate$type=="complete checklists",], aes(year, met, group = type))+
  facet_wrap(. ~ type, scale="free_y", ncol = 1)+
  geom_point()+
  geom_line() +
  xlab("year of upload")+
  ylab("")+
  geom_text(aes(label=met),hjust=0, vjust=1.5)+
  theme_bw()
ymet2b = ymeta +
  theme(axis.title.x = element_blank(),
        #axis.title.x = element_text(vjust = 0.3, size = 30), 
        axis.text.x = element_text(size = 10),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        #axis.title.y = element_text(vjust = 0.3, angle = 90, size = 30), 
        axis.text.y = element_text(size = 14)) +
  theme(legend.position = "none")+
  expand_limits(y=0)+
  scale_x_continuous(expand = c(0,1.25), breaks = c(2014,2015,2016,2017,2018,2019,2020,2021))+
  scale_y_continuous(expand = c(0.12,0), breaks = 0, labels = 0)+
  geom_hline(yintercept = 0, linetype = "dotted", size = 0.5) +
  theme(text=element_text(family="Gill Sans MT")) +
  #scale_y_log10()+
  #scale_y_continuous(trans = log10_trans(),
  #                   breaks = trans_breaks("log10", function(x) 10^x),
  #                   labels = trans_format("log10", math_format(10^.x)))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )+
  theme(strip.text.x = element_text(size = 15))
#theme(plot.margin = unit(c(0.1,0.5,0.5,0.1), "cm"))

require(cowplot)
ymet2 = plot_grid(ymet2a,ymet2b,nrow=1,ncol=2,rel_widths = c(1/2, 1/2))

require(grid)
require(gridExtra)

x.grob = textGrob("year of upload", gp = gpar(fontface = "bold", fontsize = 15,
                                              fontfamily = "Gill Sans MT"))

ymet2f = grid.arrange(arrangeGrob(ymet2, bottom = x.grob))

png('growth_Mar_2021.jpg', units="in", width=10, height=7, res=1000)
grid::grid.draw(ymet2f)
dev.off()


## growth in states  


load("maps.RData")

dists = districtmap@data %>% distinct(ST_NM)

past = data %>%
  filter(cyear <= 2018, !is.na(ST_NM)) %>%
  group_by(ST_NM) %>% 
  summarize(lists1 = n_distinct(group.id))

pres = data %>%
  filter(!is.na(ST_NM)) %>%
  group_by(ST_NM) %>% 
  summarize(lists2 = n_distinct(group.id))  

comp = left_join(dists,past)
comp = left_join(comp,pres)
comp[is.na(comp)] = 0

comp1 = comp %>% ungroup %>%
  #arrange(lists1) %>%
  #mutate(rank = rank(lists1, ties.method = "min")) %>%
  mutate(growthlists = lists2-lists1, growthperc = round((lists2-lists1)*100/lists1)) %>%
  arrange(desc(growthperc))

comp2 = comp %>% ungroup %>%
  #arrange(lists1) %>%
  #mutate(rank = rank(lists1, ties.method = "min")) %>%
  mutate(growthlists = lists2-lists1, growthperc = round((lists2-lists1)*100/lists1)) %>%
  arrange(desc(growthlists))

data.frame(comp1)

write.csv(comp2, "growthbylists.csv", row.names = F)


  
