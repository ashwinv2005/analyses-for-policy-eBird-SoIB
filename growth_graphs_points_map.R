#font_import(paths = NULL, recursive = TRUE, prompt = TRUE,pattern = NULL)
#extrafont::loadfonts(device="win")

require(tidyverse)
#load("data_till_may2020.RData")
load("ebd_IN_relJun-2022.RData")


########### India map with points

load("maps.RData")

data1 = data %>% distinct(LOCALITY.ID,LATITUDE,LONGITUDE)

ggp = ggplot() +
  geom_polygon(data = indiamap, aes(x=long, y=lat, group=group), colour = NA, fill = "black")+  
  geom_point(data = data1, aes(x=LONGITUDE,y=LATITUDE), colour = "#fcfa53", size = 0.05, alpha = 1,
             stroke = 0) +
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
        plot.background = element_rect(fill = "black",colour = NA),
        panel.background = element_rect(fill = "black", colour = NA),
        plot.title = element_text(hjust = 0.5))+
  coord_map()

n1 = "pointindiamap05.jpg"

print(ggp)
ggsave(file=n1, units="in", width=8, height=11, bg = "black")
ggsave(file=n1, units="in", width=8, height=11, bg = "transparent")

ggsave(ggp, file = n1, 
       units = "in", width = 13, height = 9, bg = "transparent", dpi = 300)

datax = data %>%
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% slice(1) %>% ungroup %>%
  filter(!is.na(DURATION.MINUTES)) %>%
  summarize(hours = sum(DURATION.MINUTES/60))


##################### growth graph

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

jpeg('growth.jpg', units="in", width=10, height=7, res=1000)
grid::grid.draw(ymet2f)
dev.off()





##################### growth graph in the last 5 July-June periods

data = data %>% select(SAMPLING.EVENT.IDENTIFIER,OBSERVER.ID,emonth,eyear)
library(lubridate)

data1 = data
data1$fyear = "2012/13"
data1 = data1 %>% 
  mutate(fyear = ifelse((eyear == 2020 & emonth > 6) | (eyear == 2021 & emonth <= 6), "2020/21", fyear)) %>%
  mutate(fyear = ifelse((eyear == 2019 & emonth > 6) | (eyear == 2020 & emonth <= 6), "2019/20", fyear)) %>%
  mutate(fyear = ifelse((eyear == 2018 & emonth > 6) | (eyear == 2019 & emonth <= 6), "2018/19", fyear)) %>%
  mutate(fyear = ifelse((eyear == 2017 & emonth > 6) | (eyear == 2018 & emonth <= 6), "2017/18", fyear)) %>%
  mutate(fyear = ifelse((eyear == 2016 & emonth > 6) | (eyear == 2017 & emonth <= 6), "2016/17", fyear)) %>%
  mutate(fyear = ifelse((eyear == 2015 & emonth > 6) | (eyear == 2016 & emonth <= 6), "2015/16", fyear)) %>%
  mutate(fyear = ifelse((eyear == 2014 & emonth > 6) | (eyear == 2015 & emonth <= 6), "2014/15", fyear)) %>%
  mutate(fyear = ifelse((eyear == 2013 & emonth > 6) | (eyear == 2014 & emonth <= 6), "2013/14", fyear))


#data2 = data1 %>% filter(fyear == "2020/21")
data1$fyear = factor(data1$fyear, levels = c("2012/13","2013/14","2014/15","2015/16","2016/17","2017/18","2018/19","2019/20","2020/21"))

tr2 = data1 %>%
  group_by(fyear) %>% summarize(rec2 = n())

datao = data1 %>%
  arrange(fyear) %>%
  group_by(OBSERVER.ID) %>% slice(1)

tro = datao %>%
  group_by(fyear) %>% summarize(obs2 = n_distinct(OBSERVER.ID))

trx = data1 %>%
  #filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(fyear) %>% summarize(coli2 = n_distinct(SAMPLING.EVENT.IDENTIFIER))

tr2 = left_join(tr2,tro)
tr2 = left_join(tr2,trx)

tr2$rec2 = cumsum(tr2$rec2)
tr2$obs2 = cumsum(tr2$obs2)
tr2$coli2 = cumsum(tr2$coli2)

editdate1 = data.frame(year = tr2$fyear)
editdate1$type = "observations"
editdate1$met = tr2$rec2
editdate2 = data.frame(year = tr2$fyear)
editdate2$type = "observers"
editdate2$met = tr2$obs2
editdate3 = data.frame(year = tr2$fyear)
editdate3$type = "checklists"
editdate3$met = tr2$coli2
editdate = rbind(editdate1,editdate2,editdate3)

editdate$type = factor(editdate$type, levels = c("observers","checklists","observations"))

library(extrafont)
library(scales)

ymeta = ggplot(editdate[editdate$type=="observers",], aes(year, met, group = type))+
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
  scale_x_discrete(expand = c(0,1))+
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

ymeta = ggplot(editdate[editdate$type=="checklists",], aes(year, met, group = type))+
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
  scale_x_discrete(expand = c(0,1.25))+
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

x.grob = textGrob("July to June period of upload", gp = gpar(fontface = "bold", fontsize = 15,
                                                             fontfamily = "Gill Sans MT"))

ymet2f = grid.arrange(arrangeGrob(ymet2, bottom = x.grob))

jpeg('growthJultoJun.jpg', units="in", width=10, height=7, res=1000)
grid::grid.draw(ymet2f)
dev.off()
