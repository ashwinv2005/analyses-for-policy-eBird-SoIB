rawpath = "ebd_IN_relFeb-2023.txt"
#rawpath = "ebd_IN_relFeb-2023.RData"
sensitivepath = "ebd_sensitive_relFeb-2023_IN.txt"

require(lubridate)
require(tidyverse)

# select only necessary columns
preimp = c("CATEGORY","COMMON.NAME",
           "STATE","COUNTY",
           "LATITUDE","LONGITUDE","OBSERVATION.DATE","OBSERVER.ID",
           "LOCALITY",
           "SAMPLING.EVENT.IDENTIFIER","SPECIES.COMMENTS","GROUP.IDENTIFIER")

nms = read.delim(rawpath, nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                 na.strings = c(""," ",NA))
nms = names(nms)
nms[!(nms %in% preimp)] = "NULL"
nms[nms %in% preimp] = NA

nms1 = read.delim(sensitivepath, nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                  na.strings = c(""," ",NA))
nms1 = names(nms1)
nms1[!(nms1 %in% preimp)] = "NULL"
nms1[nms1 %in% preimp] = NA

# read data from certain columns only
data = read.delim(rawpath, colClasses = nms, sep = "\t", header = T, quote = "", 
                  stringsAsFactors = F, na.strings = c(""," ",NA))

sesp = read.delim(sensitivepath, colClasses = nms1, sep = "\t", header = T, quote = "", 
                  stringsAsFactors = F, na.strings = c(""," ",NA))

# read sensitive species data


# merge both data frames
data = rbind(data,sesp)


data1 = data %>%
  filter(((grepl('\\bhit',SPECIES.COMMENTS, ignore.case = TRUE) |
           grepl('\\bcolli',SPECIES.COMMENTS, ignore.case = TRUE) | 
           grepl('\\bunconscious',SPECIES.COMMENTS, ignore.case = TRUE)  | 
           grepl('\\bcrash',SPECIES.COMMENTS, ignore.case = TRUE)  | 
           grepl('\\bsmash',SPECIES.COMMENTS, ignore.case = TRUE)  | 
           grepl('\\bwhack',SPECIES.COMMENTS, ignore.case = TRUE)  | 
           grepl('\\bfell',SPECIES.COMMENTS, ignore.case = TRUE)  | 
           grepl('\\bshock',SPECIES.COMMENTS, ignore.case = TRUE)  | 
           grepl('\\bcollaps',SPECIES.COMMENTS, ignore.case = TRUE)  |   
           grepl('\\bbump',SPECIES.COMMENTS, ignore.case = TRUE))) &
           
         (grepl('\\bglass',SPECIES.COMMENTS, ignore.case = TRUE) |
            grepl('\\bbuild',SPECIES.COMMENTS, ignore.case = TRUE) | 
            grepl('\\bwindow',SPECIES.COMMENTS, ignore.case = TRUE)  | 
            grepl('\\bpane',SPECIES.COMMENTS, ignore.case = TRUE)  | 
            grepl('\\bfacade',SPECIES.COMMENTS, ignore.case = TRUE) | 
            grepl('\\bwall',SPECIES.COMMENTS, ignore.case = TRUE) | 
            grepl('\\broof',SPECIES.COMMENTS, ignore.case = TRUE)))

data1 = data1 %>%
  filter(!grepl('\\bcar\\b',SPECIES.COMMENTS, ignore.case = TRUE) &
           !grepl('\\bvehicle',SPECIES.COMMENTS, ignore.case = TRUE) &
           !grepl('\\bnest',SPECIES.COMMENTS, ignore.case = TRUE) &
           !grepl('\\bcollection\\b',SPECIES.COMMENTS, ignore.case = TRUE) &
           !grepl('\\balmost collide\\b',SPECIES.COMMENTS, ignore.case = TRUE) &
           !grepl('\\bdid not collide\\b',SPECIES.COMMENTS, ignore.case = TRUE) &
           !grepl('\\bfellow',SPECIES.COMMENTS, ignore.case = TRUE) &
           !grepl('\\bchased\\b',SPECIES.COMMENTS, ignore.case = TRUE) &
           !grepl('\\bbumping\\b',SPECIES.COMMENTS, ignore.case = TRUE) &
           !grepl('\\bhitting glass\\b',SPECIES.COMMENTS, ignore.case = TRUE) &
           !grepl('\\bhitting a glass\\b',SPECIES.COMMENTS, ignore.case = TRUE) &
           !grepl('\\bhitting the glass\\b',SPECIES.COMMENTS, ignore.case = TRUE) &
           !grepl('\\bhither\\b',SPECIES.COMMENTS, ignore.case = TRUE) &
           !grepl('\\bbangalow\\b',SPECIES.COMMENTS, ignore.case = TRUE) &
           !grepl('\\beach other\\b',SPECIES.COMMENTS, ignore.case = TRUE) &
           !grepl('\\bswallowed',SPECIES.COMMENTS, ignore.case = TRUE) &
           !grepl('\\bcontinuously\\b',SPECIES.COMMENTS, ignore.case = TRUE) &
           !grepl('\\bhitting against\\b',SPECIES.COMMENTS, ignore.case = TRUE))


data1 = data1 %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         year = year(OBSERVATION.DATE),
         month = month(OBSERVATION.DATE),
         daym = day(OBSERVATION.DATE))

data1$source = "eBird comments"

data2 = read.csv("collisions_data1.csv")

data2 = data2 %>% filter(!SAMPLING.EVENT.IDENTIFIER %in% data1$SAMPLING.EVENT.IDENTIFIER)
x = data2$source
data2 = data2 %>% select(-source)

data2$OBSERVATION.DATE = strftime(strptime(data2$OBSERVATION.DATE,"%m/%d/%Y"),"%Y-%m-%d")

data2 = data2 %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         year = year(OBSERVATION.DATE),
         month = month(OBSERVATION.DATE),
         daym = day(OBSERVATION.DATE))
data2$source = x

data3 = rbind(data1,data2)


data3 = data3 %>%
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER))

data3 = data3 %>%
  group_by(COMMON.NAME,group.id) %>% slice(1)

write.csv(data3,"collisions.csv",row.names = F)

