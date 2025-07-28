library(tidyverse)
soib = read.csv("stateofindiasbirdsfull.csv")

soib$newcat = soib$IUCN
soib$newcat[soib$Range.Size.Upper <= 0.2] = "Vulnerable"
soib$newcat[soib$Range.Size.Upper <= 0.05] = "Endangered"
soib$newcat[soib$Range.Size.Upper <= 0.001] = "Critically Endangered"

soibn = soib %>% filter(newcat != IUCN, Migratory.Status == "R") %>% select(Common.Name,Range.Size.Upper,Concern.Status,IUCN,newcat)
soibn = soibn %>% filter(IUCN != "Critically Endangered") %>% filter(IUCN != "Endangered" | newcat != "Vulnerable")

write.csv(soibn,"IUCN_criteriaB_changes.csv",row.names = F)

