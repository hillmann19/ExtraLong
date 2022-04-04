# Read in packages and data
library(tidyverse)
Final_EL <- read_csv("~/Projects/ExtraLong/Data/FreesurferLongitudinal2021/ExtraLong_Sample/demographics+exclusion_datafreeze-2021_euler-212_minspan-180.csv")
All_EL <- read_csv("~/Projects/ExtraLong/Data/FreesurferLongitudinal2021/ExtraLong_Sample/ExtraLong-Datafreeze-2021-Updated-20210920.csv")  
EL_euler_cutoff <- read_csv("~/Projects/ExtraLong/Data/FreesurferLongitudinal2021/ExtraLong_Sample/exclusion_datafreeze-2021_cutoff-212.csv") 
Evol_df <- read_csv("~/Projects/ExtraLong/Data/FreesurferLongitudinal2021/ExtraLong_Sample/Evolution_subjects_02_28_2022.csv", 
                    col_types = cols(...1 = col_skip()))
oracle_long <- read_csv("~/Projects/ExtraLong/Data/FreesurferLongitudinal2021/ExtraLong_Sample/all_long_scans_oracle_cleaned.csv")

Evol_df <- Evol_df[!duplicated(Evol_df),] 
  




All_EL <- All_EL %>% 
  mutate(bblid = as.character(bblid),scanid = as.character(scanid))
Final_EL <- Final_EL %>% 
  mutate(subid = as.character(subid),sesid = as.character(sesid))

All_EL %>% 
  filter(sesid == "EVOL") %>% 
  nrow()

# 74 Sessions were in Extra Long prior to QC

All_EL %>% 
  mutate(bblid = ifelse(str_count(bblid,pattern = ".") == 5,paste0("0",bblid),bblid)) %>% 
  mutate(scanid = ifelse(str_count(scanid,pattern = ".") == 4,paste0("0",scanid),scanid)) %>% 
  left_join(EL_euler_cutoff,by = c('bblid' = 'subid','scanid' = 'sesid')) %>% 
  filter(sesid == "EVOL",exclude == TRUE) %>% 
  nrow()

# 0 Evolution sessions were removed for extreme euler number

Final_EL %>% 
  filter(acq == "EVOL") %>% 
  nrow()

# 56 sessions are in the final data set (18 sessions were removed for too many sessions within a small window of time)
# Below, you can see the scans that were removed for being within 6 months of another scan 

All_EL %>% 
  filter(sesid == "EVOL") %>% 
  anti_join(Final_EL[which(Final_EL$acq == "EVOL"),],by = c('bblid' = 'subid','scanid' = 'sesid'))

# Only data from before July 1st 2021 is being used in Extra Long (need to find how many Evol subjects fall outside this date range)

Evol_df %>% 
  mutate(doscan = as.Date(doscan,format = "%m/%d/%Y")) %>% 
  arrange(doscan) %>% 
  filter(doscan < "2021-07-01") %>% 
  View()

# 110 scans from Evolution took place before the 2021 data freeze cutoff 

# Below, you can see the scans which were eligible to be in the Extra Long project but are not being considered

Evol_df %>% 
  mutate(doscan = as.Date(doscan,format = "%m/%d/%Y")) %>% 
  filter(doscan < "2021-07-01") %>% 
  anti_join(All_EL[which(All_EL$sesid == 'EVOL'),],by = c('subid' = 'bblid','sesid' = 'scanid'))


prot_include <- c('808689 - AGGY',
                 '808922 - MGI2_PENN',
                 '808799 - DAY2',
                 '807360 - Olf Lifespan',
                 '810336 - Big GO',
                 '810336 - Go2 Supplement',
                 '810211 - FNDM',
                 'B10218 - MGI2_PITT',
                 '817628 - EFDO',
                 '816281 - NODRA',
                 '810336 - GO3 FOLLOW UP',
                 '810336 - Go3',
                 '816275 - ONM',
                 '815814 - Conte',
                 '822937 - HARMONY',
                 '818028 - Effort',
                 '818621 - SYRP',
                 '820690 - phASL',
                 '825940 - GluCEST in Psychosis',
                 '822831 - GRMPY',
                 '825834 - satterttPiloting',
                 '829502 - MOTIVE',
                 '834246 - 22qmidline',
                 '833922 - EvolPsy')


oracle_long_evol_bblids <- oracle_long %>%
  filter(str_detect(scan_protocol,pattern = "EvolPsy")) %>% 
  with(unique(bblid))

N_after_cleaning <- oracle_long %>%
  mutate(doscan = as.Date(doscan,format = "%m/%d/%Y")) %>%
  filter(bblid %in% oracle_long_evol_bblids,doscan < '2021-07-01',scan_protocol %in% prot_include) %>%
  group_by(bblid) %>%
  filter(n() >= 2) %>%
  ungroup() %>% 
  filter(scan_protocol == "833922 - EvolPsy")

N_after_cleaning

# 77 sessions from Evolution remained after scan date and scan protocol were used to filter the data set
  


  





