library(tidyverse)
# Load in the necessary data, including demographic, quality control, subcortical, cortical, clinical, and manual image rating information
setwd("/Users/hillmann/Projects/ExtraLong/Scripts/QCcutoffShiny")
demo <- read_csv("ExtraLong-Datafreeze-2021-Updated-20210920.csv") 
QC.long.2021 <- read_csv("quality_2021-11-01.csv") 
asegCross.df <- read_csv("TabulatedQC/aseg_stats_2021-11-01.csv") 
aparcCross.lh <- read_csv("TabulatedQC/aparc_volume_lh_2021-11-01.csv")
aparcCross.rh <- read_csv("TabulatedQC/aparc_volume_rh_2021-11-01.csv")
aparcCross <- aparcCross.lh %>% 
  left_join(aparcCross.rh,by = c("bblid","seslabel","eTIV")) 
aparcThick.lh <- read_csv("TabulatedQC/aparc_thickness_lh_2021-11-01.csv")
aparcThick.rh <- read_csv("TabulatedQC/aparc_thickness_rh_2021-11-01.csv")
aparcThick <- aparcThick.lh %>% 
  left_join(aparcThick.rh,by = c("bblid","seslabel","eTIV")) %>% 
  mutate(Thickness.Ave = (lh_MeanThickness_thickness + rh_MeanThickness_thickness)/2)
aparcSurf.lh <- read_csv("TabulatedQC/aparc_area_lh_2021-11-01.csv")
aparcSurf.rh <- read_csv("TabulatedQC/aparc_area_rh_2021-11-01.csv")
aparcSurf <- aparcSurf.lh %>% 
  left_join(aparcSurf.rh,by = c("bblid","seslabel","eTIV")) %>% 
  mutate(Surf.Avg = (lh_WhiteSurfArea_area + rh_WhiteSurfArea_area)/2) 
ManualRating2416 <- read_csv("n2416_t1QaData_20170516.csv") 
ManualRatingGRMPY <- read_csv("n231_GRMPY_manualQA_20200728_needsSCANID.csv") 

# Used demographic dictionary to relabel data

demo <- demo %>% 
  mutate(sex = case_when(sex == 1 ~ "Male",sex == 2 ~ "Female")) %>% 
  mutate(race = case_when(race == 1 ~ "White",race == 2 ~ "Black",race == 3 ~ "Native American",race == 4 ~ "Asian",race == 5 ~ "More than one race",race == 6 ~ "Hawaiian/Pacific",race == 9 ~ "Unknown")) %>% 
  mutate(ethnic = case_when(ethnic == 1 ~ "Hispanic/Latino",ethnic == 2 ~ "Not Hispanic/Latino",ethnic == 9 ~ "Unknown")) %>% 
  rename(ethnicity = ethnic) %>% 
  relocate(bblid,sex,race,ethnicity)  

Euler.summary <- QC.long.2021 %>% 
  filter(euler_total > -6000) %>% 
  summarize(Mean = mean(euler_total),sd = sd(euler_total))

QC.long.2021 <- QC.long.2021 %>% 
  left_join(demo,by = c("bblid","seslabel" = "scanid")) %>% 
  mutate(age = scanage_months/12)

CheckManually <- QC.long.2021 %>% 
  left_join(ManualRating2416,by = c("bblid","seslabel" = "scanid")) %>% 
  left_join(ManualRatingGRMPY,by = c("bblid","seslabel" = "scanid")) %>% 
  mutate(TrueAverage = (ratingKS + ratingJB + ratingLV)/3) %>% 
  filter(euler_total < -6000 | ((euler_total > (Euler.summary$Mean - 2*Euler.summary$sd)) & (rating == 0 | TrueAverage < .66)))

#write.csv(CheckManually,file = "~/Projects/ExtraLong/Data/PoorQualityScans.csv")



