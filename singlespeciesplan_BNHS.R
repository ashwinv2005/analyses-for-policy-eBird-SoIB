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

datacafx = data %>% filter(COMMON.NAME %in% caf) %>% group_by(COMMON.NAME) %>% 
  summarize(n = n()) %>% ungroup

data0 = left_join(data,tax)

data1 = data0 %>% 
  filter(Family %in% c("Scolopacidae (Sandpipers and Allies)",
                       "Glareolidae (Pratincoles and Coursers)","Dromadidae (Crab-Plover)",
                       "Burhinidae (Thick-knees)","Recurvirostridae (Stilts and Avocets)",
                       "Haematopodidae (Oystercatchers)","Charadriidae (Plovers and Lapwings)"), 
         cyear > 2011) %>%
  group_by(group.id,COMMON.NAME) %>% slice(1) %>% ungroup






## CAF species

datac = data0 %>% filter(COMMON.NAME %in% caf) %>%
  group_by(group.id,COMMON.NAME) %>% slice(1) %>% ungroup



data2 = datac %>% 
  filter(OBSERVATION.COUNT != "X") %>% 
  mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
  group_by(LOCALITY,group.id,month,cyear,COMMON.NAME) %>% ungroup

data2 = left_join(data2,onepercent)

datacaf = data2 %>% filter(OBSERVATION.COUNT >= thresh1)
datacaf1 = datacaf %>% arrange(LATITUDE,LONGITUDE,LOCALITY) %>% 
  select(LOCALITY,LATITUDE,LONGITUDE,DISTRICT,ST_NM,COMMON.NAME,
         OBSERVATION.COUNT,daym,month,cyear,thresh1)

datacaf1 = datacaf1 %>%
  mutate(historical.status = ifelse(cyear > 2011, "N", "Y")) %>%
  mutate(season = ifelse(month %in% c(11,12,1,2), "Winter", "Passage/Summer"))
  
datacaf2 = datacaf1 %>%
  distinct(LOCALITY,LATITUDE,LONGITUDE,COMMON.NAME,historical.status,season,DISTRICT,ST_NM)


write.csv(datacaf2,"CAFonepercentlocations.csv",row.names = F)



## temp

a = read.csv("CAFonepercentlocations_temp.csv")

season = a %>%
  distinct(LOCALITY,COMMON.NAME,season)
season = season %>%
  mutate(ss = ifelse(season == "Winter", "-1", "1")) 
season$ss = as.numeric(season$ss)

season = season %>%
  group_by(LOCALITY,COMMON.NAME) %>% summarize(ss = sum(ss)) %>%
  mutate(status = ifelse(ss == 1, "Passage", "Winter")) %>%
  select(LOCALITY,COMMON.NAME,status)

locs = a %>%
  group_by(LOCALITY) %>% slice(1) %>% ungroup %>%
  select(LOCALITY,LATITUDE,LONGITUDE,DISTRICT,ST_NM)

a_over = a %>% distinct(LOCALITY,COMMON.NAME)
  
a_recent = a %>% filter(historical.status == "N") %>% distinct(LOCALITY,COMMON.NAME)

a_hist = anti_join(a_over,a_recent)

a_recent = left_join(a_recent,locs)
a_recent = left_join(a_recent,season)
a_recent = a_recent %>% arrange(ST_NM,DISTRICT,LOCALITY)

a_hist = left_join(a_hist,locs)
a_hist = left_join(a_hist,season)
a_hist = a_hist %>% arrange(ST_NM,DISTRICT,LOCALITY)

write.csv(a_recent,"CAFonepercentlocations_recent.csv",row.names = F)
write.csv(a_hist,"CAFonepercentlocations_historical.csv",row.names = F)


