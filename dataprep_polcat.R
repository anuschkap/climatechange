#gebruik je local directories
#leer je aan om alles gewoon vanaf begin in .rmd te zetten. 

library(tidyverse)
library(haven)

pop <- read_dta("./data/expert_data_stata.dta")

popsel <- pop %>% filter(country_id==19) %>%
  select(party, lroverall) %>%
  dplyr::rename(party_name = party) 

#popsel$party_name <- revalue(popsel$party_name, c("50Plus"= "50PLUS"))
#ik ken dit niet, dus doe dit anders. 
popsel$party_name[popsel$party_name=="50Plus"] <- "50PLUS"

#ik heb het onderstaande bestand niet. 
ches <- read_dta("/Users/anuschka/Documents/gesis_dir/ches/ches2019.dta")

#gok dat het deze is
ches <- read_dta("./data/CHES2019_experts.dta") 
dutch <- 1001:1051  #Dutch party ids
ches <- ches[ches$party_id %in% dutch, ] 
ches <- ches %>% select(party_name, lrgen) 
ches$party_name[ches$party_name=="PVdD"] <- "PvdD"


#nee je kan, niet mergen by party_name omdat in beide bestanden de partij vaker voorkomt! 
#gaat het hier om verschillende jaren, of om verschillende scores van verschillende experts? 
#wat wil je eigenlijk doen. gewoon alle scores van de experts onder elkaar zetten? 

#expert <- merge(ches, popsel, by = "party_name", all = T) #Overgenomen van jouw website maar krijg veel meer observaties

names(ches) <- names(popsel) #namen gelijk maken

expert <- rbind(ches, popsel)
expert <- na.omit(expert)

save(expert, file = "./data/processed/expert.RData")

#maar is dit wat je wil? 

#means en sd per partij
expert2 <- expert %>%
  group_by(party_name) %>%
  summarise(lroverall_mean = mean(lroverall),
            lroverall_sd = sd(lroverall))

#aan dpes2021 hangen
load("./data/dpes2021sel.RData")

unique(dpes2021sel$party_name)
#ai, dit is een factor, niet een string

dpes2021sel$party_name <- as.character(dpes2021sel$party_name)
unique(dpes2021sel$party_name2)
unique(expert2$party_name)
#namen lijken nu te kloppen. 

dpes2021sel %>% 
  left_join(expert2, by="party_name") -> dpes2021seltest
  

table(dpes2021sel$party_name, useNA = "always")
table(expert$party_name)
dpes2021sel$party_name <- dpes2021sel$V071
dpes2021sel$party_name <- revalue(dpes2021sel$V071, c("GroenLinks" = "GL", "ChristenUnie" = "CU", "Forum voor Democratie" = "FvD"))

dpes2021seltest <- merge(dpes2021sel, expert, by = "party_name", all.x = TRUE)
#NEE



#Dividing in three cats left/middle/right. NOT NEEDED
expert$lr_cat <- NA
expert$lr_cat[expert$total_mean <= 4] <- 1
expert$lr_cat[expert$total_mean >4 & expert$total_mean <= 7] <- 2
expert$lr_cat[expert$total_mean > 7] <- 3 