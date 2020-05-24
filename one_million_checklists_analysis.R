require(tidyverse)
load("data_till_may2020.RData")
media = read.csv("ebird_media.csv")

mediad = left_join(media,data)



########### India map with points

load("maps.RData")

data1 = data %>% distinct(LOCALITY.ID,LATITUDE,LONGITUDE)

ggp = ggplot() +
  geom_polygon(data = indiamap, aes(x=long, y=lat, group=group), colour = NA, fill = "black")+  
  geom_point(data = data1, aes(x=LONGITUDE,y=LATITUDE), colour = "yellow", size = 0.2) +
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

n1 = "pointindiamap.png"

print(ggp)
ggsave(file=n1, units="in", width=8, height=11, bg = "transparent")


##################### growth graph

library(lubridate)

tr2 = data %>%
  group_by(eyear) %>% summarize(rec2 = n(),obs2 = n_distinct(OBSERVER.ID))

trx = data %>%
  filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(eyear) %>% summarize(coli2 = n_distinct(SAMPLING.EVENT.IDENTIFIER))

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
  scale_x_continuous(expand = c(0,1), breaks = c(2014,2015,2016,2017,2018,2019,2020))+
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
  scale_x_continuous(expand = c(0,1.25), breaks = c(2014,2015,2016,2017,2018,2019,2020))+
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

png('growth.png', units="in", width=10, height=7, res=1000)
grid::grid.draw(ymet2f)
dev.off()