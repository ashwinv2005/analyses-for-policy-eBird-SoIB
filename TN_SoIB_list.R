##################################

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
soib = soib %>% select(eBird.English.Name.2019,Long.Term.Status,Current.Status,
                       Range.Status,Concern.Status,IUCN,WLPA.Schedule)

data = left_join(data,soib,by = c("COMMON.NAME" = "eBird.English.Name.2019"))

tn = data %>% filter(ST_NM == "Tamil Nadu")
tnsp = tn %>%
  filter(grepl('Vedanthangal',LOCALITY) | grepl('vedanthangal',LOCALITY) | grepl('Vedantangal',LOCALITY) |
           grepl('vedantangal',LOCALITY) | grepl('Vedantangel',LOCALITY) | grepl('VEDANTHANGAL',LOCALITY) |
           grepl('Madurantakam',LOCALITY)) %>%
  distinct(COMMON.NAME,Long.Term.Status,Current.Status,Range.Status,Concern.Status,IUCN,WLPA.Schedule)

clem = read.csv("eBird-Clements-v2019-integrated-checklist-August-2019.csv")

tnsp = left_join(tnsp,clem,by = c("COMMON.NAME" = "English.name"))
tnsp = tnsp 
##%>% select(scientific.name,Concern.Status)

require(splitstackshape)
require(magrittr)
require(reshape2)

#listmap <- read.delim2("Checklist Mapper.csv", sep=',')

dat <- read.delim("ebird_IN__1900_2020_1_12_barchart.txt", 
                  na.strings = c("NA", "", "null"), 
                  as.is=TRUE, 
                  sep="\t",
                  header = FALSE,
                  quote="")

# Extract Sample Size into an array
sample_size <- dat[4,][2:49]
colnames(sample_size) <- 1:48

# Remove first four rows that has no data
dat <- dat[-c(1,2,3,4),]

# Split the species name
dat  <- cSplit(dat, 'V1', sep="=", type.convert=FALSE)
colnames(dat) <- c(1:49,"COMMON.NAME","SCIENTIFIC.NAME")

# Clean the species name
dat <- dat %>% 
  within (COMMON.NAME <- substr(COMMON.NAME,1,nchar(COMMON.NAME)-11)) %>%
  within (SCIENTIFIC.NAME <- substr(SCIENTIFIC.NAME,1,nchar(SCIENTIFIC.NAME)-6)) %>%
  within (SCIENTIFIC.NAME <- substr(SCIENTIFIC.NAME,7,nchar(SCIENTIFIC.NAME))) 

tnsp = left_join(tnsp,dat,by = c("scientific.name" = "SCIENTIFIC.NAME"))
tnsp = tnsp %>% select(family,COMMON.NAME.y,scientific.name,Long.Term.Status,Current.Status,
                       Range.Status,Concern.Status,IUCN,WLPA.Schedule)

write.csv(tnsp,"Vedanthangal_SoIB_species.csv",row.names=F)
