load("data.RData")
load("maps.RData")

require(tidyverse)
library(reshape2)

awc = data %>% filter(OBSERVER.ID == "obsr1196810") %>% filter(cyear == 2021, month %in% c(12,1,2))
bws = data %>% filter(OBSERVER.ID == "obsr1927965") %>% filter(cyear == 2021, month %in% c(12,1,2))

awc = unique(awc$group.id)
bws = unique(bws$group.id)

ab = intersect(awc,bws)

data1 = data %>% filter(group.id %in% bws) %>%
  group_by(group.id,COMMON.NAME) %>% slice(1) %>% ungroup

datat = data1 %>% distinct(group.id,LOCALITY)
table(datat$LOCALITY)

data1$LOCALITY[data1$group.id == "G6217763"] = "Bortibeel (count 1)"
data1$LOCALITY[data1$group.id == "G6217764"] = "Bortibeel (count 2)"
data1$LOCALITY[data1$group.id == "G6186609"] = "Farakka IBA Panchanandapur (count 1)"
data1$LOCALITY[data1$group.id == "G6240765"] = "Farakka IBA Panchanandapur (count 2)"
data1$LOCALITY[data1$group.id == "G6215385"] = "Nabagram (count 1)"
data1$LOCALITY[data1$group.id == "G6215386"] = "Nabagram (count 2)"
data1$LOCALITY[data1$group.id == "G6213676"] = "Bakreshwar Dam (count 1)"
data1$LOCALITY[data1$group.id == "G6219203"] = "Bakreshwar Dam (count 2)"
data1$LOCALITY[data1$group.id == "G6213546"] = "Tajpur (count 1)"
data1$LOCALITY[data1$group.id == "G6213547"] = "Tajpur (count 2)"

data1$LOCALITY[data1$LOCALITY == "Binod Dighi, West Bengal, IN (24.484, 88.122)"] = "Binod Dighi"
data1$LOCALITY[data1$LOCALITY == "Bosipota , West Bengal, IN (22.686, 88.322)"] = "Bosipota"
data1$LOCALITY[data1$LOCALITY == "Central Avenue, Jagannathpur, West Bengal, IN (24.815, 87.95)"] = "Jagannathpur Wetland 1"
data1$LOCALITY[data1$LOCALITY == "Jagannathpur, West Bengal, IN (24.815, 87.943)"] = "Jagannathpur Wetland 2"
data1$LOCALITY[data1$LOCALITY == "Kamalpur, West Bengal, IN (22.214, 87.99)"] = "Kamalpur"
data1$LOCALITY[data1$LOCALITY == "Karjjana, West Bengal, IN (23.361, 87.893)"] = "Karjjana Wetland 1"
data1$LOCALITY[data1$LOCALITY == "Katwa Bardhaman Road, Karjjana, West Bengal, IN (23.363, 87.893)"] = "Karjjana Wetland 2"
data1$LOCALITY[data1$LOCALITY == "Ranchi - Purulia Road, Jaypur, West Bengal, IN (23.417, 86.133)"] = "Jaypur"
data1$LOCALITY[data1$LOCALITY == "Sahaspur, West Bengal, IN (23.113, 87.659)"] = "Sahaspur"
data1$LOCALITY[data1$LOCALITY == "Unnamed Road, Alitakhali, West Bengal, IN (22.146, 88.602)"] = "Alitakhali"
data1$LOCALITY[data1$LOCALITY == "Unnamed Road, Bayra, West Bengal, IN (24.446, 88.207) "] = "Bayra"

data2 = data1 %>% filter(CATEGORY %in% c("species","issf")) %>% 
  distinct(COMMON.NAME,LOCALITY,OBSERVATION.COUNT)

arr = pivot_wider(data2, names_from = LOCALITY, values_from = OBSERVATION.COUNT)
arr[is.na(arr)] = "0"

write.csv(arr,"BWS_AWC.csv", row.names = F)

data3 = data1 %>% group_by(LOCALITY) %>% slice(1) %>% ungroup %>%
  distinct(SAMPLING.EVENT.IDENTIFIER,LATITUDE,LONGITUDE,LOCALITY)

data3$URL = paste("https://ebird.org/checklist/",data3$SAMPLING.EVENT.IDENTIFIER, sep = "")
data3 = data3 %>% select(LOCALITY,URL,LATITUDE,LONGITUDE)

write.csv(data3,"BWS_AWC_locdetails.csv", row.names = F)
