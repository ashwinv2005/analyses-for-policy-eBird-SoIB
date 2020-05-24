require(lubridate)
require(tidyverse)

load("may2020.RData")
art = as.data.frame(art)

inject.dots <- function(df) {names(df) <- sub(" ", ".", names(df));df}
art = inject.dots(art)
art = inject.dots(art)

art$LAST.EDITED.DATE = "2020-05-10"

imp = c("GLOBAL.UNIQUE.IDENTIFIER","CATEGORY","COMMON.NAME","OBSERVATION.COUNT",
        "LOCALITY.ID", "REVIEWED","VALID","SAMPLING.EVENT.IDENTIFIER","LAST.EDITED.DATE",
        #"LOCALITY.TYPE",
        "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
        "PROTOCOL.TYPE",
        "DURATION.MINUTES","EFFORT.DISTANCE.KM",
        "ALL.SPECIES.REPORTED","group.id")


# no of days in every month, and cumulative number
days = c(31,28,31,30,31,30,31,31,30,31,30,31)
cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)

# create a column "group.id" which can help remove duplicate checklists
art = art %>%
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER))


art = art %>%
  #group_by(group.id,COMMON.NAME) %>% slice(1) %>% ungroup %>%
  dplyr::select(imp) %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         month = month(OBSERVATION.DATE),
         day = day(OBSERVATION.DATE) + cdays[month],
         daym = day(OBSERVATION.DATE),
         #week = week(OBSERVATION.DATE),
         #fort = ceiling(day/14),
         cyear = year(OBSERVATION.DATE)) %>%
  dplyr::select(-c("OBSERVATION.DATE")) %>%
  mutate(year = ifelse(day <= 151, cyear-1, cyear))

data = art

require(data.table)
require(sp)
require(rgeos)

## add map details to eBird data

load("maps.RData")
load("clips.RData")

# add columns with DISTRICT and ST_NM to main data 

temp = data %>% group_by(group.id) %>% slice(1) # same group ID, same grid/district/state 

rownames(temp) = temp$group.id # only to setup adding the group.id column for the future left_join
coordinates(temp) = ~LONGITUDE + LATITUDE # convert to SPDF?
#proj4string(temp) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
temp = over(temp,districtmap) # returns only ATTRIBUTES of districtmap (DISTRICT and ST_NM)
temp = data.frame(temp) # convert into data frame for left_join
temp$group.id = rownames(temp) # add column to join with the main data
data = left_join(temp,data)

# add columns with protected area name to main data 

temp = data %>% group_by(group.id) %>% slice(1) # same group ID, same grid/district/state 

rownames(temp) = temp$group.id # only to setup adding the group.id column for the future left_join
coordinates(temp) = ~LONGITUDE + LATITUDE # convert to SPDF?
proj4string(temp) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
temp = over(temp,pamap) # returns only ATTRIBUTES of districtmap (DISTRICT and ST_NM)
temp = data.frame(temp) # convert into data frame for left_join
temp$group.id = rownames(temp) # add column to join with the main data
data = left_join(temp,data)


# add columns with GRID ATTRIBUTES to main data

temp = data %>% group_by(group.id) %>% slice(1)

rownames(temp) = temp$group.id
coordinates(temp) = ~LONGITUDE + LATITUDE
temp = over(temp,gridmapg1)
temp = data.frame(temp)
temp$group.id = rownames(temp)
data = left_join(temp,data)
names(data)[1] = "gridg1"

temp = data %>% group_by(group.id) %>% slice(1)

rownames(temp) = temp$group.id
coordinates(temp) = ~LONGITUDE + LATITUDE
temp = over(temp,gridmapg2)
temp = data.frame(temp)
temp$group.id = rownames(temp)
data = left_join(temp,data)
names(data)[1] = "gridg2"

temp = data %>% group_by(group.id) %>% slice(1)

rownames(temp) = temp$group.id
coordinates(temp) = ~LONGITUDE + LATITUDE
temp = over(temp,gridmapg3)
temp = data.frame(temp)
temp$group.id = rownames(temp)
data = left_join(temp,data)
names(data)[1] = "gridg3"

temp = data %>% group_by(group.id) %>% slice(1)

rownames(temp) = temp$group.id
coordinates(temp) = ~LONGITUDE + LATITUDE
temp = over(temp,gridmapg4)
temp = data.frame(temp)
temp$group.id = rownames(temp)
data = left_join(temp,data)
names(data)[1] = "gridg4"

temp = data %>% group_by(group.id) %>% slice(1)

rownames(temp) = temp$group.id
coordinates(temp) = ~LONGITUDE + LATITUDE
temp = over(temp,g2clip)
temp = data.frame(temp)
temp$group.id = rownames(temp)
data = left_join(temp,data)
names(data)[1] = "g2clip"

temp = data %>% group_by(group.id) %>% slice(1)

rownames(temp) = temp$group.id
coordinates(temp) = ~LONGITUDE + LATITUDE
temp = over(temp,g3clip)
temp = data.frame(temp)
temp$group.id = rownames(temp)
data = left_join(temp,data)
names(data)[1] = "g3clip"

## 

regions = read.csv("districtlist.csv")
data = left_join(data,regions)

data = data %>%
  mutate(LAST.EDITED.DATE = as.Date(LAST.EDITED.DATE), 
         emonth = month(LAST.EDITED.DATE),
         eyear = year(LAST.EDITED.DATE)) %>%
  dplyr::select(-c("LAST.EDITED.DATE"))

art = data
names(art)[18] = "APPROVED"

load("data.RData")

data = data %>%
  mutate(LAST.EDITED.DATE = as.Date(LAST.EDITED.DATE), 
         emonth = month(LAST.EDITED.DATE),
         eyear = year(LAST.EDITED.DATE)) %>%
  dplyr::select(-c("LAST.EDITED.DATE"))

art = art %>% filter(!GLOBAL.UNIQUE.IDENTIFIER %in% data$GLOBAL.UNIQUE.IDENTIFIER)
art$OBSERVER.ID = gsub("[[:alpha:]]", "", art$OBSERVER.ID)
data$OBSERVER.ID = gsub("[[:alpha:]]", "", data$OBSERVER.ID)
data = rbind(data,art)

x = data$GLOBAL.UNIQUE.IDENTIFIER
y = str_split_fixed(x,":",4)

data$OBS.ID = y[,4]
data = data %>% select(-GLOBAL.UNIQUE.IDENTIFIER)

save(data,file="data_till_may2020.RData")


#############################

require(tidyverse)
load("data_till_may2020.RData")
end = read.csv("Endemicity - Endemicity.csv")
map = read.csv("Map to Other Lists - map.csv")
end = left_join(end,map,by = c("eBird.English.Name" = "eBird.English.Name.2018"))

end = end %>% select(eBird.English.Name.2019,India,Subcontinent)

ebd1 = data %>% filter(cyear == 2015, month == 5, daym == 9)
ebd2 = data %>% filter(cyear == 2016, month == 5, daym == 14)
ebd3 = data %>% filter(cyear == 2017, month == 5, daym == 13)
ebd4 = data %>% filter(cyear == 2018, month == 5, daym == 5)
ebd5 = data %>% filter(cyear == 2019, month == 5, daym == 4)
ebd6 = data %>% filter(cyear == 2020, month == 5, daym == 9)

ebd = rbind(ebd1,ebd2,ebd3,ebd4,ebd5,ebd6)

ebd = left_join(ebd,end,by = c("COMMON.NAME" = "eBird.English.Name.2019"))

sum1 = ebd %>%
  group_by(cyear) %>% summarize(lists = n_distinct(SAMPLING.EVENT.IDENTIFIER),
                                unique.lists = n_distinct(group.id),
                                participants = n_distinct(OBSERVER.ID))

sum2 = ebd %>%
  filter(CATEGORY %in% c("species","issf")) %>%
  group_by(cyear) %>% summarize(species = n_distinct(COMMON.NAME))

sum3 = ebd %>%
  filter(CATEGORY %in% c("species","issf")) %>%
  group_by(cyear,COMMON.NAME) %>% slice(1) %>%
  filter(!is.na(India)) %>%
  group_by(cyear) %>% summarize(endemics.India = n())

sum4 = ebd %>%
  filter(CATEGORY %in% c("species","issf")) %>%
  group_by(cyear,COMMON.NAME) %>% slice(1) %>%
  filter(!is.na(Subcontinent)) %>%
  group_by(cyear) %>% summarize(endemics.Sub = n())

sum = left_join(sum1,sum2)
sum = left_join(sum,sum3)
sum = left_join(sum,sum4)

write.csv(sum,"ebd_last6years.csv",row.names=F)
