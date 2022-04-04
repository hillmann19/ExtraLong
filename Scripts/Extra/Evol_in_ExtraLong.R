# Load in data and packages 

library(tidyverse)
library(gtsummary)
evol <- read_csv("~/Projects/ExtraLong/Data/Extra/evol_allscans.csv")
EL_2019 <- read_csv("~/Projects/ExtraLong/Data/Old/quality_2020-11-09.csv")
scan_id_map <- read_csv("~/Projects/ExtraLong/Data/Old/scanid_to_seslabel_demo_20200531.csv")


# Clean data set, remove last rows which clearly aren't correct data and remove no shows

evol_clean <- evol %>% 
  filter(str_detect(BBLID,pattern = "^[[:digit:]]{5,}")) %>% 
  filter(str_detect(SCANSTAT,pattern = "IS5",negate = T)) %>% 
  distinct(BBLID,PROTOCOL,VISITNUM,.keep_all = T) 
  
evol_clean %>% 
  group_by(BBLID) %>% 
  summarize(Num_scans = n()) %>% 
  arrange(desc(Num_scans)) %>% 
  select(-BBLID) %>% 
  mutate(Num_scans = as.factor(Num_scans)) %>% 
  tbl_summary(label = list("Num_scans" ~ "# of timepoints"))

evol_clean %>% 
  filter(BBLID == "121407") %>% 
  View()

evol_in_EL <- c()
cntr <- 1
for(id in evol_bblids){
  if(id %in% unique(EL_2019$bblid)){
    evol_in_EL[cntr] <- TRUE
  } else{
    evol_in_EL[cntr] <- FALSE
  }
  cntr <- cntr + 1
}

sum(evol_in_EL)

# Write data to file
write.csv(evol_clean,file = "/Users/hillmann/Projects/Evolution/Data/Extra/EvolutionScansOracle.csv")

