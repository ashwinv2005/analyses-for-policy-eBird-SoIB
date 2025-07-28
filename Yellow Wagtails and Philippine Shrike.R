require(lubridate)
require(tidyverse)
media = read.csv("ebird_media.csv")
media1 = read.csv("ebird_media1.csv")
media = rbind(media,media1)

a1 = read.delim("ebd_LK_brnshr_prv_relMay-2021_provisional.txt", sep = "\t", header = T, quote = "", 
                stringsAsFactors = F, na.strings = c(""," ",NA))
a2 = read.delim("ebd_LK_brnshr_prv_relMay-2021.txt", sep = "\t", header = T, quote = "", 
                stringsAsFactors = F, na.strings = c(""," ",NA))
b1 = read.delim("ebd_IN_brnshr_prv_relMay-2021_provisional.txt", sep = "\t", header = T, quote = "", 
                stringsAsFactors = F, na.strings = c(""," ",NA))
b2 = read.delim("ebd_IN_brnshr_prv_relMay-2021.txt", sep = "\t", header = T, quote = "", 
                stringsAsFactors = F, na.strings = c(""," ",NA))
c1 = read.delim("ebd_BD_brnshr_prv_relMay-2021_provisional.txt", sep = "\t", header = T, quote = "", 
                stringsAsFactors = F, na.strings = c(""," ",NA))
c2 = read.delim("ebd_BD_brnshr_prv_relMay-2021.txt", sep = "\t", header = T, quote = "", 
                stringsAsFactors = F, na.strings = c(""," ",NA))
d1 = read.delim("ebd_BT_brnshr_prv_relMay-2021_provisional.txt", sep = "\t", header = T, quote = "", 
                stringsAsFactors = F, na.strings = c(""," ",NA))
d2 = read.delim("ebd_BT_brnshr_prv_relMay-2021.txt", sep = "\t", header = T, quote = "", 
                stringsAsFactors = F, na.strings = c(""," ",NA))
e1 = read.delim("ebd_NP_brnshr_prv_relMay-2021_provisional.txt", sep = "\t", header = T, quote = "", 
                stringsAsFactors = F, na.strings = c(""," ",NA))
e2 = read.delim("ebd_NP_brnshr_prv_relMay-2021.txt", sep = "\t", header = T, quote = "", 
                stringsAsFactors = F, na.strings = c(""," ",NA))

brsh = rbind(a1,a2,b1,b2,c1,c2,d1,d2,e1,e2)

a1 = read.delim("ebd_LK_eaywag_prv_relMay-2021_provisional.txt", sep = "\t", header = T, quote = "", 
                stringsAsFactors = F, na.strings = c(""," ",NA))
a2 = read.delim("ebd_LK_eaywag_prv_relMay-2021.txt", sep = "\t", header = T, quote = "", 
                stringsAsFactors = F, na.strings = c(""," ",NA))
b1 = read.delim("ebd_IN_eaywag_prv_relMay-2021_provisional.txt", sep = "\t", header = T, quote = "", 
                stringsAsFactors = F, na.strings = c(""," ",NA))
b2 = read.delim("ebd_IN_eaywag_prv_relMay-2021.txt", sep = "\t", header = T, quote = "", 
                stringsAsFactors = F, na.strings = c(""," ",NA))
c1 = read.delim("ebd_BD_eaywag_prv_relMay-2021_provisional.txt", sep = "\t", header = T, quote = "", 
                stringsAsFactors = F, na.strings = c(""," ",NA))
c2 = read.delim("ebd_BD_eaywag_prv_relMay-2021.txt", sep = "\t", header = T, quote = "", 
                stringsAsFactors = F, na.strings = c(""," ",NA))
d1 = read.delim("ebd_BT_eaywag_prv_relMay-2021_provisional.txt", sep = "\t", header = T, quote = "", 
                stringsAsFactors = F, na.strings = c(""," ",NA))
d2 = read.delim("ebd_BT_eaywag_prv_relMay-2021.txt", sep = "\t", header = T, quote = "", 
                stringsAsFactors = F, na.strings = c(""," ",NA))
e1 = read.delim("ebd_NP_eaywag_prv_relMay-2021_provisional.txt", sep = "\t", header = T, quote = "", 
                stringsAsFactors = F, na.strings = c(""," ",NA))
e2 = read.delim("ebd_NP_eaywag_prv_relMay-2021.txt", sep = "\t", header = T, quote = "", 
                stringsAsFactors = F, na.strings = c(""," ",NA))
f1 = read.delim("ebd_PK_eaywag_prv_relMay-2021_provisional.txt", sep = "\t", header = T, quote = "", 
                stringsAsFactors = F, na.strings = c(""," ",NA))
f2 = read.delim("ebd_PK_eaywag_prv_relMay-2021.txt", sep = "\t", header = T, quote = "", 
                stringsAsFactors = F, na.strings = c(""," ",NA))

eywa = rbind(a1,a2,b1,b2,c1,c2,d1,d2,e1,e2,f1,f2)


a1 = read.delim("ebd_LK_eaywag1_prv_relMay-2021_provisional.txt", sep = "\t", header = T, quote = "", 
                stringsAsFactors = F, na.strings = c(""," ",NA))
a2 = read.delim("ebd_LK_eaywag1_prv_relMay-2021.txt", sep = "\t", header = T, quote = "", 
                stringsAsFactors = F, na.strings = c(""," ",NA))
b1 = read.delim("ebd_IN_eaywag1_prv_relMay-2021_provisional.txt", sep = "\t", header = T, quote = "", 
                stringsAsFactors = F, na.strings = c(""," ",NA))
b2 = read.delim("ebd_IN_eaywag1_prv_relMay-2021.txt", sep = "\t", header = T, quote = "", 
                stringsAsFactors = F, na.strings = c(""," ",NA))
c1 = read.delim("ebd_BD_eaywag1_prv_relMay-2021_provisional.txt", sep = "\t", header = T, quote = "", 
                stringsAsFactors = F, na.strings = c(""," ",NA))
c2 = read.delim("ebd_BD_eaywag1_prv_relMay-2021.txt", sep = "\t", header = T, quote = "", 
                stringsAsFactors = F, na.strings = c(""," ",NA))
d1 = read.delim("ebd_BT_eaywag1_prv_relMay-2021_provisional.txt", sep = "\t", header = T, quote = "", 
                stringsAsFactors = F, na.strings = c(""," ",NA))
d2 = read.delim("ebd_BT_eaywag1_prv_relMay-2021.txt", sep = "\t", header = T, quote = "", 
                stringsAsFactors = F, na.strings = c(""," ",NA))
e1 = read.delim("ebd_NP_eaywag1_prv_relMay-2021_provisional.txt", sep = "\t", header = T, quote = "", 
                stringsAsFactors = F, na.strings = c(""," ",NA))
e2 = read.delim("ebd_NP_eaywag1_prv_relMay-2021.txt", sep = "\t", header = T, quote = "", 
                stringsAsFactors = F, na.strings = c(""," ",NA))
f1 = read.delim("ebd_PK_eaywag1_prv_relMay-2021_provisional.txt", sep = "\t", header = T, quote = "", 
                stringsAsFactors = F, na.strings = c(""," ",NA))
f2 = read.delim("ebd_PK_eaywag1_prv_relMay-2021.txt", sep = "\t", header = T, quote = "", 
                stringsAsFactors = F, na.strings = c(""," ",NA))

wywa = rbind(a1,a2,b1,b2,c1,c2,d1,d2,e1,e2,f1,f2)


data = rbind(brsh,eywa,wywa)

x = data$GLOBAL.UNIQUE.IDENTIFIER
y = str_split_fixed(x,":",4)

data$obs_id = y[,4]
data = data %>% select(-GLOBAL.UNIQUE.IDENTIFIER)

media = left_join(media,data)
media = media %>% filter(!is.na(COMMON.NAME))

media = media %>% select(asset_id,obs_id,media_type,CATEGORY,COMMON.NAME,COUNTRY,LATITUDE,LONGITUDE,
                         OBSERVATION.DATE)

days = c(31,28,31,30,31,30,31,31,30,31,30,31)
cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)

media = media %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         month = month(OBSERVATION.DATE),
         day = day(OBSERVATION.DATE) + cdays[month],
         daym = day(OBSERVATION.DATE),
         year = year(OBSERVATION.DATE)) %>%
  dplyr::select(-c("OBSERVATION.DATE"))

media = media %>%
  arrange(year,month,daym)

YWPS = write.csv(media, "yellow_wagtail_shrike.csv", row.names = F)



