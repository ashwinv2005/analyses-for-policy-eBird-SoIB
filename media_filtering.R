require(tidyverse)
#load("data_till_may2020.RData")
load("data.RData")
media = read.csv("ebird_media.csv")

x = data$GLOBAL.UNIQUE.IDENTIFIER
y = str_split_fixed(x,":",4)

data$OBS.ID = y[,4]
data = data %>% select(-GLOBAL.UNIQUE.IDENTIFIER)

mediad = left_join(media,data)

mediad1 = mediad %>% filter(COMMON.NAME == "White-naped Tit",ASSET.TYPE != "audio") %>%
  arrange(cyear,month,daym) %>%
  select(ASSET.ID,LONGITUDE,LATITUDE,group.id,DISTRICT,ST_NM,g2clip,g3clip,day,daym,month,cyear)

write.csv(mediad1,"WnTiMedia.csv", row.names = F)