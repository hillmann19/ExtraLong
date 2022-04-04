# Read in the data and packages

library(tidyverse)
LGI_lh <- read_csv("~/Projects/ExtraLong/Data/FreesurferLongitudinal2021/TabulatedQC/lh.aparc.pial_lgi_clean_2022-02-23.csv")
LGI_rh <- read_csv("~/Projects/ExtraLong/Data/FreesurferLongitudinal2021/TabulatedQC/rh.aparc.pial_lgi_clean_2022-02-23.csv")
demo <- read_csv("~/Projects/ExtraLong/Data/FreesurferLongitudinal2021/demographics+exclusion_datafreeze-2021_euler-212_minspan-180.csv")
  
# Rename subid to bblid 

demo <- demo %>% 
  rename(bblid = subid) %>% 
  mutate(sex = case_when(sex == 1 ~ "Male",sex == 2 ~ "Female",TRUE ~ NA_character_)) %>% 
  rename(age = scanage_years) %>% 
  select(bblid,sesid,sex,age)

# Create LGI data set 
  
colnames(LGI_lh) <- paste0(colnames(LGI_lh),"_lh")
colnames(LGI_rh) <- paste0(colnames(LGI_rh),"_rh")

LGI_lh <- LGI_lh %>% 
  select(-`Unnamed: 0_lh`) %>% 
  rename(bblid = bblid_lh,sesid = sesid_lh)

LGI_rh <- LGI_rh %>% 
  select(-`Unnamed: 0_rh`) %>% 
  rename(bblid = bblid_rh,sesid = sesid_rh)

LGI <- LGI_lh %>% 
  left_join(LGI_rh)

# Create global, area-weighted calculation of LGI

LGI_mean <- LGI %>% 
  select(bblid,sesid,matches("^Mean"))

LGI_area <- LGI %>% 
  select(bblid,sesid,matches("^Area"))

LGI_mean_long <- LGI_mean %>% 
  pivot_longer(cols = Mean_unknown_lh:last_col(),names_to = "Region",values_to = "GI") %>% 
  mutate(Region = str_remove_all(Region,pattern = "^Mean_"))

LGI_area_long <- LGI_area %>% 
  pivot_longer(cols = Area_mm2_unknown_lh:last_col(),names_to = "Region",values_to = "Area_mm2") %>% 
  mutate(Region = str_remove_all(Region,pattern = "^Area_mm2_")) %>% 
  group_by(bblid,sesid) %>% 
  mutate(Weights = Area_mm2/sum(Area_mm2)) %>% 
  ungroup()

LGI_global <- LGI_mean_long %>% 
  left_join(LGI_area_long[,c("bblid","sesid","Region","Weights")]) %>% 
  group_by(bblid,sesid) %>% 
  mutate(Global_GI = sum(GI*Weights)) %>% 
  ungroup() %>% 
  select(bblid,sesid,Global_GI) %>% 
  distinct()

LGI <- LGI %>% 
  left_join(LGI_global)

lgi_long_plot <- LGI %>% 
  left_join(demo) %>% 
  ggplot(aes(x = age,y = Global_GI,color = sex)) + geom_point(size = .3) + geom_smooth() + geom_line(aes(x = age,y = Global_GI,color = sex,group = bblid),alpha = .3) + scale_color_manual(values = c("#ca0020","#0571b0")) + theme_linedraw() + labs(x = "Age",y = "GI (global)",color = "Sex")

# Create list of subjects whose LGI processing failed

LGI_failed_subjects <- demo %>% 
  left_join(LGI) %>% 
  filter(is.na(Area_mm2_unknown_rh)) %>% 
  select(bblid,sesid)

write.csv(LGI_failed_subjects,file = "/Users/hillmann/Projects/ExtraLong/Data/FreesurferLongitudinal2021/FailedLGI.csv")
  