require(tidyverse)
load("data.RData")

data = data %>% filter(ALL.SPECIES.REPORTED == 1)

spec = "White-naped Tit"

cfilt = data %>% filter(COMMON.NAME == spec) %>%
  distinct(group.id)

ct = data %>% filter(group.id %in% cfilt$group.id) %>%
  distinct(group.id,COMMON.NAME)

ctl = length(unique(ct$group.id))
ctf = ct %>%
  group_by(COMMON.NAME) %>% summarize(freq = n()/ctl) %>%
  arrange(desc(freq))

ctf = ctf[1:50,]

spec = ctf$COMMON.NAME


for (i in spec[2:50])
{
  cfilt1 = data %>% filter(COMMON.NAME == i) %>%
    distinct(group.id)
  
  ct1 = data %>% filter(group.id %in% cfilt1$group.id) %>%
    distinct(group.id,COMMON.NAME)
  
  ctl1 = length(unique(ct1$group.id))
  ctf1 = ct1 %>%
    group_by(COMMON.NAME) %>% summarize(freq = n()/ctl1) %>%
    arrange(desc(freq))
  
  ctf1 = ctf1 %>% filter(COMMON.NAME %in% spec)
  names(ctf1)[2] = i
  
  ctf = left_join(ctf,ctf1)
}

names(ctf)[2] = "White-naped Tit"

x = ctf %>% select(COMMON.NAME,`White-naped Tit`)
y = ctf %>% filter(COMMON.NAME == "White-naped Tit") %>% select(-COMMON.NAME)
y = as.numeric(y[1,])

x$freq = (x$`White-naped Tit`)*y
x = x %>% arrange(desc(freq))


######################################### north


spec = "White-naped Tit"

data1 = data %>% filter(LATITUDE > 20)

cfilt = data1 %>% filter(COMMON.NAME == spec) %>%
  distinct(group.id)

ct = data1 %>% filter(group.id %in% cfilt$group.id) %>%
  distinct(group.id,COMMON.NAME)

ctl = length(unique(ct$group.id))
ctf = ct %>%
  group_by(COMMON.NAME) %>% summarize(freq = n()/ctl) %>%
  arrange(desc(freq))

ctf = ctf[1:50,]

spec = ctf$COMMON.NAME


for (i in spec[2:50])
{
  cfilt1 = data %>% filter(COMMON.NAME == i) %>%
    distinct(group.id)
  
  ct1 = data %>% filter(group.id %in% cfilt1$group.id) %>%
    distinct(group.id,COMMON.NAME)
  
  ctl1 = length(unique(ct1$group.id))
  ctf1 = ct1 %>%
    group_by(COMMON.NAME) %>% summarize(freq = n()/ctl1) %>%
    arrange(desc(freq))
  
  ctf1 = ctf1 %>% filter(COMMON.NAME %in% spec)
  names(ctf1)[2] = i
  
  ctf = left_join(ctf,ctf1)
}

names(ctf)[2] = "White-naped Tit"

x.north = ctf %>% select(COMMON.NAME,`White-naped Tit`)
y = ctf %>% filter(COMMON.NAME == "White-naped Tit") %>% select(-COMMON.NAME)
y = as.numeric(y[1,])

x.north$freq = (x.north$`White-naped Tit`)*y
x.north = x.north %>% arrange(desc(freq))


######################################### south


spec = "White-naped Tit"

data1 = data %>% filter(LATITUDE < 20)

cfilt = data1 %>% filter(COMMON.NAME == spec) %>%
  distinct(group.id)

ct = data1 %>% filter(group.id %in% cfilt$group.id) %>%
  distinct(group.id,COMMON.NAME)

ctl = length(unique(ct$group.id))
ctf = ct %>%
  group_by(COMMON.NAME) %>% summarize(freq = n()/ctl) %>%
  arrange(desc(freq))

ctf = ctf[1:50,]

spec = ctf$COMMON.NAME


for (i in spec[2:50])
{
  cfilt1 = data %>% filter(COMMON.NAME == i) %>%
    distinct(group.id)
  
  ct1 = data %>% filter(group.id %in% cfilt1$group.id) %>%
    distinct(group.id,COMMON.NAME)
  
  ctl1 = length(unique(ct1$group.id))
  ctf1 = ct1 %>%
    group_by(COMMON.NAME) %>% summarize(freq = n()/ctl1) %>%
    arrange(desc(freq))
  
  ctf1 = ctf1 %>% filter(COMMON.NAME %in% spec)
  names(ctf1)[2] = i
  
  ctf = left_join(ctf,ctf1)
}

names(ctf)[2] = "White-naped Tit"

x.south = ctf %>% select(COMMON.NAME,`White-naped Tit`)
y = ctf %>% filter(COMMON.NAME == "White-naped Tit") %>% select(-COMMON.NAME)
y = as.numeric(y[1,])

x.south$freq = (x.south$`White-naped Tit`)*y
x.south = x.south %>% arrange(desc(freq))


