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

kl = data %>% filter(ST_NM == "Kerala")

pwlsp = kl %>%
  filter(CATEGORY %in% c("species","issf")) %>%
  filter(DISTRICT == "Thiruvananthapuram", grepl('Punchakkari',LOCALITY) | grepl('Vellayani',LOCALITY) |
           grepl('punchakkari',LOCALITY) | grepl('vellayani',LOCALITY)) %>%
  distinct(COMMON.NAME,Long.Term.Status,Current.Status,Range.Status,Concern.Status,IUCN,WLPA.Schedule)

pwlobs = kl %>%
  filter(DISTRICT == "Thiruvananthapuram", grepl('Punchakkari',LOCALITY) | grepl('Vellayani',LOCALITY) |
           grepl('punchakkari',LOCALITY) | grepl('vellayani',LOCALITY)) %>%
  select(group.id,SAMPLING.EVENT.IDENTIFIER,COMMON.NAME,OBSERVATION.COUNT,LOCALITY,LATITUDE,LONGITUDE,
         OBSERVER.ID,PROTOCOL.TYPE,TIME.OBSERVATIONS.STARTED,DURATION.MINUTES,EFFORT.DISTANCE.KM,
         ALL.SPECIES.REPORTED,day,daym,month,cyear)


write.csv(pwlsp,"Punchakkari_SoIB_species.csv",row.names=F)
write.csv(pwlobs,"Punchakkari_observations.csv",row.names=F)
