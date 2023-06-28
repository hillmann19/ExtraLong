# Load in packages and data
library(tidyverse)
library(mgcv)
library(longCombat)
library(lme4)
library(table1)
library(scales)
scanner_info <- read_csv('/Users/hillmann/Projects/ExtraLong/Data/ANTs/Tabulated/scanner_info.csv')
demo <- read_csv("/Users/hillmann/Projects/ExtraLong/Data/FreesurferLongitudinal2021/ExtraLong_Sample/demographics+exclusion_datafreeze-2021_euler-212_minspan-180.csv")
ants_Vol <- read_csv("~/Projects/ExtraLong/Data/ANTs/Tabulated/CorticalMetrics/Volume_DKT_01_25_2023.csv")
ants_CT <- read_csv("~/Projects/ExtraLong/Data/ANTs/Tabulated/CorticalMetrics/CorticalThickness_DKT_01_25_2023.csv")
ants_GMD <- read_csv("/Users/hillmann/Projects/ExtraLong/Data/ANTs/Tabulated/CorticalMetrics/GMD_DKT_04_20_2023.csv")
free_Vol_lh <- read_csv("~/Projects/ExtraLong/Data/FreesurferLongitudinal2021/TabulatedQC/Longitudinal/lh_DKTatlas_volume_2022-02-24.csv")
free_Vol_rh <- read_csv("~/Projects/ExtraLong/Data/FreesurferLongitudinal2021/TabulatedQC/Longitudinal/rh_DKTatlas_volume_2022-02-24.csv")
free_CT_lh <- read_csv("~/Projects/ExtraLong/Data/FreesurferLongitudinal2021/TabulatedQC/Longitudinal/lh_DKTatlas_thickness_2022-02-24.csv")
free_CT_rh <- read_csv("~/Projects/ExtraLong/Data/FreesurferLongitudinal2021/TabulatedQC/Longitudinal/rh_DKTatlas_thickness_2022-02-24.csv")
free_SA_lh <- read_csv("~/Projects/ExtraLong/Data/FreesurferLongitudinal2021/TabulatedQC/Longitudinal/lh_DKTatlas_area_2022-02-24.csv")
free_SA_rh <- read_csv("~/Projects/ExtraLong/Data/FreesurferLongitudinal2021/TabulatedQC/Longitudinal/rh_DKTatlas_area_2022-02-24.csv")

# Create demographic tables and plots

demo <- demo %>% 
  mutate(sex = case_when(sex == 1 ~ 'Male',sex == 2 ~ 'Female',TRUE ~ NA_character_)) %>% 
  mutate(race = case_when(race == 1 ~ 'White',race == 2 ~ 'Black/African American',race == 3 ~ 'Native American',
                          race == 4 ~ 'Asian',race == 5 ~ 'More than one race',race == 6 ~ 'Hawaiian/Pacific Islander',
                          race == 9 ~ NA_character_,TRUE ~ NA_character_)) %>% 
  mutate(ethnic = case_when(ethnic == 1 ~ 'Hispanic',ethnic == 2 ~ 'Not Hispanic',ethnic == 9 ~ 'Unknown',TRUE ~ NA_character_)) 
  
demo %>% 
  group_by(subid) %>% 
  arrange(timepoint) %>% 
  mutate(Timespan = scanage_years[n()] - scanage_years[1]) %>% 
  mutate(Timespan = ifelse(n() == 1,NA,Timespan)) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  mutate(scanner = ifelse(is.na(scanner),'TrioTim',scanner)) %>% 
  rename(`Age at T1` = scanage_years,Sex = sex,Race = race,Scanner = scanner) %>% 
  table1(~ `Age at T1` + Sex + Race,data = .)


demo_timepoint_N <- demo %>% 
  group_by(subid) %>% 
  arrange(timepoint) %>% 
  slice_tail(n = 1) %>% 
  group_by(ntimepoints) %>% 
  summarize(count = n()) %>% 
  add_row(ntimepoints = 1,count = 0) %>% 
  arrange(ntimepoints)

demo_avg_timeline <- demo %>% 
  group_by(subid) %>% 
  arrange(timepoint) %>% 
  mutate(Years_since_baseline = scanage_years - scanage_years[1]) %>% 
  ungroup() %>% 
  group_by(timepoint) %>% 
  summarize(Avg_years = mean(Years_since_baseline,na.rm = T)) %>% 
  mutate(Avg_years = round(Avg_years,digits = 1)) 

theme_set(theme_minimal())
theme_update(axis.title.x = element_text(vjust = -.5),text = element_text(size = 13))

sample_profile <- demo %>% 
  group_by(subid) %>% 
  arrange(timepoint) %>% 
  mutate(Years_since_baseline = scanage_years - scanage_years[1]) %>% 
  ungroup() %>% 
  ggplot(aes(x = factor(timepoint),y = Years_since_baseline)) + 
  geom_point(size = .05,color =  '#66c2a4') + 
  geom_line(aes(group = subid),color = '#66c2a4',alpha = .15) + 
  geom_point(data = demo_avg_timeline,aes(x = timepoint,y = Avg_years),color = '#006d2c') + 
  geom_line(data = demo_avg_timeline,aes(x = timepoint,y = Avg_years),color = '#006d2c') + 
  scale_x_discrete(labels = paste0(demo_timepoint_N$ntimepoints,'\n (N = ',demo_timepoint_N$count,')')) +
  labs(x = 'Timepoint',y = 'Years since baseline',
       subtitle = paste0('N = ',sum(demo_timepoint_N$count),', Timepoints = ',sum(demo_timepoint_N$count*demo_timepoint_N$ntimepoints)),
       caption = expression(paste('Darker line represents ',mu[Years*"|"*Timepoint == t]))) 

# Clean demographic data

demo <- demo %>% 
  rename(Subject.Id = subid,Session.Id = sesid) %>% 
  select(Subject.Id,Session.Id,sex,scanage_years) %>% 
  mutate(across(.cols = c(Subject.Id,Session.Id),.fns = ~ as.character(.x))) %>% 
  mutate(Subject.Id = ifelse(str_length(Subject.Id) == 5,paste0("0",Subject.Id),Subject.Id)) %>% 
  mutate(Session.Id = ifelse(str_length(Session.Id) == 4,paste0("0",Session.Id),Session.Id)) %>% 
  rename(AGE = scanage_years,SEX = sex) %>% 
  group_by(Subject.Id) %>% 
  arrange(AGE) %>% 
  mutate(Timepoint = row_number()) %>% 
  ungroup()

# Clean surface area data and get weights

free_SA_long <- free_SA_lh %>% 
  left_join(free_SA_rh,by = c("bblid","seslabel")) %>% 
  select(!(matches(".x|.y|WhiteSurfArea_area|aparc.DKTatlas.area|temporalpole"))) %>% 
  rename(Subject.Id = bblid,Session.Id = seslabel) %>% 
  mutate(Subject.Id = as.character(Subject.Id),Session.Id = as.character(Session.Id)) %>% 
  mutate(Subject.Id = ifelse(str_length(Subject.Id) == 5,paste0("0",Subject.Id),Subject.Id)) %>% 
  mutate(Session.Id = ifelse(str_length(Session.Id) == 4,paste0("0",Session.Id),Session.Id)) %>% 
  pivot_longer(cols = lh_caudalanteriorcingulate_area:rh_insula_area,names_to = "Region",values_to = "free_SA") %>% 
  group_by(Subject.Id,Session.Id) %>% 
  mutate(Weight = free_SA/sum(free_SA)) %>% 
  mutate(Region = str_replace_all(Region,pattern = "_area$",replacement = "")) %>% 
  mutate(Region = str_replace_all(Region,pattern = "_",replacement = "."))

# Create data set with all Volume data after applying longitudinal combat
free_Vol <- free_Vol_lh %>% 
  left_join(free_Vol_rh,by = c("bblid","seslabel")) %>% 
  select(!(matches(".x|.y|aparc.DKTatlas.volume|temporalpole"))) %>% 
  rename(Subject.Id = bblid,Session.Id = seslabel) %>% 
  mutate(Subject.Id = as.character(Subject.Id),Session.Id = as.character(Session.Id)) %>% 
  mutate(Subject.Id = ifelse(str_length(Subject.Id) == 5,paste0("0",Subject.Id),Subject.Id)) %>% 
  mutate(Session.Id = ifelse(str_length(Session.Id) == 4,paste0("0",Session.Id),Session.Id)) 

colnames(free_Vol) <- str_replace_all(colnames(free_Vol),pattern = "_volume",replacement = "")
colnames(free_Vol) <- str_replace_all(colnames(free_Vol),pattern = "_",replacement = ".")

scanner_info <- scanner_info %>% 
  mutate(subid = str_replace_all(subid,pattern = '^sub-',replacement = '')) %>% 
  mutate(sesid = str_replace_all(sesid,pattern = '^ses-',replacement = '')) %>%
  rename(Subject.Id = subid,Session.Id = sesid)

ants_Vol_for_combat <- ants_Vol %>% 
  left_join(demo) %>% 
  left_join(scanner_info) %>% 
  group_by(Subject.Id) %>% 
  arrange(AGE) %>% 
  mutate(Timepoint = row_number()) %>% 
  ungroup() %>% 
  relocate(Timepoint,AGE,SEX,scanner,.after = Session.Id) %>% 
  mutate(scanner = ifelse(is.na(scanner),'TrioTim,',scanner)) %>% 
  mutate(scanner = str_replace_all(scanner,pattern = ',$',replacement = '')) 

free_Vol_for_combat <- free_Vol %>% 
  left_join(demo) %>% 
  left_join(scanner_info) %>% 
  group_by(Subject.Id) %>% 
  arrange(AGE) %>% 
  mutate(Timepoint = row_number()) %>% 
  ungroup() %>% 
  relocate(Timepoint,AGE,SEX,scanner,.after = Session.Id) %>% 
  mutate(scanner = ifelse(is.na(scanner),'TrioTim,',scanner)) %>% 
  mutate(scanner = str_replace_all(scanner,pattern = ',$',replacement = '')) 

regions <- ants_Vol_for_combat %>% 
  select(lh.caudalanteriorcingulate:last_col()) %>% 
  colnames()


Vol_ants_combat <- longCombat(idvar='Subject.Id', 
                              timevar='Timepoint',
                              batchvar='scanner', 
                              features=regions, 
                              formula='AGE + SEX + AGE:SEX + Timepoint',
                              ranef='(1|Subject.Id)',
                              data=ants_Vol_for_combat)$data_combat

Vol_free_combat <- longCombat(idvar='Subject.Id', 
                              timevar='Timepoint',
                              batchvar='scanner', 
                              features=regions, 
                              formula='AGE + SEX + AGE:SEX + Timepoint',
                              ranef='(1|Subject.Id)',
                              data=free_Vol_for_combat)$data_combat


ants_Vol_long <- Vol_ants_combat %>% 
  pivot_longer(lh.caudalanteriorcingulate.combat:last_col(),names_to = "Region",values_to = "ants_Vol") %>% 
  mutate(Subject.Id = as.character(Subject.Id))

free_Vol_long <- Vol_free_combat %>% 
  pivot_longer(lh.caudalanteriorcingulate.combat:last_col(),names_to = "Region",values_to = "free_Vol")

Vol <- free_Vol_long %>% 
  left_join(ants_Vol_long) %>% 
  filter(!is.na(ants_Vol))

# Create data set with all cortical thickness data

free_CT <- free_CT_lh %>% 
  left_join(free_CT_rh,by = c("bblid","seslabel")) %>% 
  select(!(matches(".x|.y|aparc.DKTatlas.thickness|temporalpole"))) %>% 
  rename(Subject.Id = bblid,Session.Id = seslabel) %>% 
  mutate(Subject.Id = as.character(Subject.Id),Session.Id = as.character(Session.Id)) %>% 
  mutate(Subject.Id = ifelse(str_length(Subject.Id) == 5,paste0("0",Subject.Id),Subject.Id)) %>% 
  mutate(Session.Id = ifelse(str_length(Session.Id) == 4,paste0("0",Session.Id),Session.Id)) 

colnames(free_CT) <- str_replace_all(colnames(free_CT),pattern = "_thickness",replacement = "")
colnames(free_CT) <- str_replace_all(colnames(free_CT),pattern = "_",replacement = ".")


ants_CT_for_combat <- ants_CT %>% 
  left_join(demo) %>% 
  left_join(scanner_info) %>% 
  group_by(Subject.Id) %>% 
  arrange(AGE) %>% 
  mutate(Timepoint = row_number()) %>% 
  ungroup() %>% 
  relocate(Timepoint,AGE,SEX,scanner,.after = Session.Id) %>% 
  mutate(scanner = ifelse(is.na(scanner),'TrioTim,',scanner)) %>% 
  mutate(scanner = str_replace_all(scanner,pattern = ',$',replacement = '')) 

free_CT_for_combat <- free_CT %>% 
  left_join(demo) %>% 
  left_join(scanner_info) %>% 
  group_by(Subject.Id) %>% 
  arrange(AGE) %>% 
  mutate(Timepoint = row_number()) %>% 
  ungroup() %>% 
  relocate(Timepoint,AGE,SEX,scanner,.after = Session.Id) %>% 
  mutate(scanner = ifelse(is.na(scanner),'TrioTim,',scanner)) %>% 
  mutate(scanner = str_replace_all(scanner,pattern = ',$',replacement = '')) 

regions <- ants_CT_for_combat %>% 
  select(lh.caudalanteriorcingulate:last_col()) %>% 
  colnames()


CT_ants_combat <- longCombat(idvar='Subject.Id', 
                              timevar='Timepoint',
                              batchvar='scanner', 
                              features=regions, 
                              formula='AGE + SEX + Timepoint',
                              ranef='(1|Subject.Id)',
                              data=ants_CT_for_combat)$data_combat

CT_free_combat <- longCombat(idvar='Subject.Id', 
                              timevar='Timepoint',
                              batchvar='scanner', 
                              features=regions, 
                              formula='AGE + SEX + Timepoint',
                              ranef='(1|Subject.Id)',
                              data=free_CT_for_combat)$data_combat


ants_CT_long <- CT_ants_combat %>% 
  pivot_longer(lh.caudalanteriorcingulate.combat:last_col(),names_to = "Region",values_to = "ants_CT")

free_CT_long <- CT_free_combat %>% 
  pivot_longer(lh.caudalanteriorcingulate.combat:last_col(),names_to = "Region",values_to = "free_CT")

CT <- ants_CT_long %>% 
  left_join(free_CT_long) %>% 
  filter(!is.na(ants_CT))

# Run longitudianl combat on GMD data

ants_GMD_for_combat <- ants_GMD %>% 
  left_join(demo) %>% 
  left_join(scanner_info) %>% 
  group_by(Subject.Id) %>% 
  arrange(AGE) %>% 
  mutate(Timepoint = row_number()) %>% 
  ungroup() %>% 
  relocate(Timepoint,AGE,SEX,scanner,.after = Session.Id) %>% 
  mutate(scanner = ifelse(is.na(scanner),'TrioTim,',scanner)) %>% 
  mutate(scanner = str_replace_all(scanner,pattern = ',$',replacement = ''))

GMD_ants_combat <- longCombat(idvar='Subject.Id', 
                              timevar='Timepoint',
                              batchvar='scanner', 
                              features=regions, 
                              formula='AGE + SEX + Timepoint',
                              ranef='(1|Subject.Id)',
                              data=ants_GMD_for_combat)$data_combat


# Subjects/Sessions that failed to run
failed_IDs <- free_CT_long %>%
  left_join(ants_CT_long) %>%
  filter(is.na(ants_CT)) %>%
  filter(!str_detect(Region,pattern = "MeanThickness")) %>%
  distinct(Subject.Id,Timepoint) %>%
  with(unique(Subject.Id))

# Make plots comparing ANTs and Freesurfer output (ExtraLong)

clean_Region_names <- function(df){
  df <- df %>% 
    mutate(Hemisphere = case_when(str_detect(Region,pattern = "^rh\\.") ~ "Right",str_detect(Region,pattern = "^lh\\.") ~ "Left",TRUE ~ NA_character_)) %>% 
    mutate(Region = str_replace_all(Region,pattern = '.combat$',replacement = '')) %>% 
    mutate(Region = str_replace_all(Region,pattern = "^[lr]h\\.",replacement = "")) %>% 
    mutate(Region = str_replace_all(Region,pattern = "middle(.*)",replacement = "middle \\1")) %>%
    mutate(Region = str_replace_all(Region,pattern = "(.*)anterior(.*)",replacement = "\\1 anterior \\2")) %>% 
    mutate(Region = str_replace_all(Region,pattern = "(.*)middle(.*)$",replacement = "\\1 middle \\2")) %>% 
    mutate(Region = str_replace_all(Region,pattern = "(.*)orbitofrontal$",replacement = "\\1 orbitofrontal")) %>% 
    mutate(Region = str_replace_all(Region,pattern = "superior(.*)",replacement = "superior \\1")) %>% 
    mutate(Region = str_replace_all(Region,pattern = "posterior(.*)",replacement = "posterior \\1")) %>% 
    mutate(Region = str_replace_all(Region,pattern = "inferior(.*)",replacement = "inferior \\1")) %>% 
    mutate(Region = str_replace_all(Region,pattern = "lateral(.*)",replacement = "lateral \\1")) %>% 
    mutate(Region = str_replace_all(Region,pattern = "pars(.*)",replacement = "pars \\1")) %>% 
    mutate(Region = str_replace_all(Region,pattern = "(.*)temporal$",replacement = "\\1 temporal")) %>%
    mutate(Region = str_replace_all(Region,pattern = " {2,}",replacement = " ")) %>% 
    mutate(Region = str_to_title(Region)) %>% 
    mutate(Region = str_trim(Region,side = "both"))
  return(df)
}

Vol <- clean_Region_names(Vol)
CT <- clean_Region_names(CT)
GMD <- GMD_ants_combat %>% 
  pivot_longer(cols  = matches('.combat$'),names_to = 'Region',values_to = 'GMD') %>% 
  clean_Region_names()

Cor_Vol <- Vol %>%
  filter(!(Subject.Id %in% failed_IDs)) %>% 
  group_by(Subject.Id,Timepoint,Region) %>%
  mutate(free_Vol = mean(free_Vol,na.rm = T),ants_Vol = mean(ants_Vol,na.rm = T)) %>% 
  ungroup() %>% 
  select(-Hemisphere) %>% 
  group_by(Region) %>% 
  mutate(Cor = cor(free_Vol,ants_Vol)) %>%
  ungroup() %>% 
  distinct(Region,.keep_all = T) %>%
  select(Region,Cor) %>% 
  mutate(Cor = round(Cor,2)) 

Cor_CT <- CT %>%
  filter(!(Subject.Id %in% failed_IDs)) %>% 
  group_by(Subject.Id,Timepoint,Region) %>%
  mutate(free_CT = mean(free_CT,na.rm = T),ants_Vol = mean(ants_CT,na.rm = T)) %>% 
  ungroup() %>% 
  select(-Hemisphere) %>% 
  group_by(Region) %>% 
  mutate(Cor = cor(free_CT,ants_CT)) %>%
  distinct(Region,.keep_all = T) %>%
  select(Region,Cor) %>% 
  mutate(Cor = round(Cor,2)) %>% 
  ungroup()

theme_set(theme_minimal())
theme_update(text = element_text(size = 12))

EL_freeVol_vs_EL_antsVol <- Vol %>%
  filter(!(Subject.Id %in% failed_IDs)) %>% 
  group_by(Subject.Id,Timepoint,Region) %>% 
  mutate(free_Vol = mean(free_Vol,na.rm = T),ants_Vol = mean(ants_Vol,na.rm = T)) %>% 
  ungroup() %>% 
  distinct(Subject.Id,Timepoint,Region,.keep_all = T) %>% 
  select(-Hemisphere) %>% 
  ggplot(aes(x = free_Vol,y = ants_Vol)) + facet_wrap(~Region,scales = "free") + 
  geom_text(data = Cor_Vol,aes(x = Inf,y = -Inf,label = paste("r =",Cor),hjust = 1,vjust = -1)) + 
  geom_point(size = .5,color = "grey") + geom_smooth(method = "lm",color = "black") + 
  scale_x_continuous(breaks = pretty_breaks(n = 4)) + scale_y_continuous(breaks = pretty_breaks(n = 4)) +  
  labs(x = "Freesurfer Cortical Gray Volume",y = "ANTs Cortical Gray Volume",title = "Cortical Gray Volume by Pipeline (mean across hemispheres)")

EL_freeCT_vs_EL_antsCT <- CT %>%
  filter(!(Subject.Id %in% failed_IDs)) %>%
  group_by(Subject.Id,Timepoint,Region) %>% 
  mutate(free_CT = mean(free_CT,na.rm = T),ants_CT = mean(ants_CT,na.rm = T)) %>% 
  ungroup() %>% 
  distinct(Subject.Id,Timepoint,Region,.keep_all = T) %>% 
  select(-Hemisphere) %>% 
  ggplot(aes(x = free_CT,y = ants_CT)) + geom_text(data = Cor_CT,aes(x = Inf,y = -Inf,label = paste("r =",Cor),hjust = 1,vjust = -1)) + geom_point(size = .5,color = "grey") + geom_smooth(method = "lm",color = "black") + facet_wrap(~Region,scales = "free") + labs(x = "Freesurfer Cortical Thickness",y = "ANTs Cortical Thickness",title = "Cortical Thickness by Pipeline (mean across hemispheres)")

# Compare total cortical gray matter volume between freesurfer and ants (Extra Long)

All_EL_freeVol_vs_EL_antsVol <- Vol %>%
  filter(!(Subject.Id %in% failed_IDs)) %>%
  group_by(Subject.Id,Timepoint) %>%
  mutate(across(.cols = c(free_Vol,ants_Vol),.fns = ~ sum(.x))) %>%
  ungroup() %>%
  distinct(Subject.Id,Timepoint,.keep_all = T) %>%
  ggplot(aes(x = free_Vol,y = ants_Vol)) + geom_point() + geom_smooth(method = "lm") + labs(x = "Freesurfer Gray Matter Cortical Volume",y = "ANTs Gray Matter Cortical Volume")

# Plot metrics by demographic data
  
EL_freeVol_antsVol_vs_age <- Vol %>%
  filter(!(Subject.Id %in% failed_IDs)) %>%
  left_join(demo) %>%
  pivot_longer(cols = c(free_Vol,ants_Vol),names_to = "Pipeline",values_to = "Volume") %>%
  mutate(Pipeline = case_when(Pipeline == "free_Vol" ~ "Freesurfer",Pipeline == "ants_Vol" ~ "ANTs",TRUE ~ NA_character_)) %>%
  group_by(Subject.Id,Session.Id,Region,Pipeline) %>% 
  mutate(Volume = mean(Volume,na.rm = T)) %>% 
  ungroup() %>% 
  distinct(Subject.Id,Session.Id,Region,Pipeline,.keep_all = T) %>% 
  select(-Hemisphere) %>% 
  ggplot(aes(x = AGE,y = Volume,color = Pipeline,group = interaction(Subject.Id,Pipeline))) + 
  geom_line(alpha = .1) + geom_smooth(aes(group = Pipeline)) + 
  labs(x = "Age",y = "Volume \n (mean across hemispheres)") + 
  facet_wrap(~Region,scales = "free_y")

# Cortical thickness: Freesurfer and ANTs Extra Long vs Age
EL_freeCT_antsCT_vs_age <- CT %>%
  filter(!(Subject.Id %in% failed_IDs)) %>%
  left_join(demo) %>%
  pivot_longer(cols = c(free_CT,ants_CT),names_to = "Pipeline",values_to = "CT") %>%
  mutate(Pipeline = case_when(Pipeline == "free_CT" ~ "Freesurfer",Pipeline == "ants_CT" ~ "ANTs",TRUE ~ NA_character_)) %>%
  group_by(Subject.Id,Session.Id,Region,Pipeline) %>% 
  mutate(CT = mean(CT,na.rm = T)) %>% 
  ungroup() %>% 
  distinct(Subject.Id,Session.Id,Region,Pipeline,.keep_all = T) %>% 
  select(-Hemisphere) %>% 
  ggplot(aes(x = AGE,y = CT,color = Pipeline,group = interaction(Subject.Id,Pipeline))) + 
  geom_smooth(aes(group = Pipeline)) + geom_line(alpha = .1) + 
  facet_wrap(~Region,scales = "free") + 
  labs(x = "Age",y = "Cortical Thickness \n (mean across hemispheres)")


antsGMD_vs_age <- GMD %>% 
  left_join(demo) %>% 
  group_by(Subject.Id,Timepoint,Region) %>% 
  mutate(GMD = mean(GMD,na.rm = T)) %>% 
  select(-Hemisphere) %>% 
  ungroup() %>% 
  distinct(Subject.Id,Timepoint,Region,.keep_all = T) %>% 
  ggplot(aes(x = AGE,y = GMD)) + geom_line(aes(group = Subject.Id),color = 'grey',alpha = .25,size = .5) +
  geom_smooth(color = 'black') + facet_wrap(~Region,scales = 'free_y') + 
  labs(x = 'Age',y = 'GMD \n (average across hemispheres)') 


antsVol_vs_age_by_sex <- Vol %>%
  filter(!(Subject.Id %in% failed_IDs)) %>%
  left_join(demo) %>%
  pivot_longer(cols = c(free_Vol,ants_Vol),names_to = "Pipeline",values_to = "Volume") %>%
  mutate(Pipeline = case_when(Pipeline == "free_Vol" ~ "Freesurfer",Pipeline == "ants_Vol" ~ "ANTs",TRUE ~ NA_character_)) %>%
  group_by(Subject.Id,Session.Id,Region,Pipeline) %>% 
  mutate(Volume = mean(Volume,na.rm = T)) %>% 
  ungroup() %>% 
  distinct(Subject.Id,Session.Id,Region,Pipeline,.keep_all = T) %>% 
  select(-Hemisphere) %>% 
  filter(Pipeline == 'ANTs') %>% 
  ggplot(aes(x = AGE,y = Volume,color = SEX,group = interaction(Subject.Id,SEX))) + geom_line(alpha = .1) + 
  geom_smooth(aes(group = SEX),method = 'gam',formula = y ~ s(x,bs = 'tp')) + 
  labs(x = "Age",y = "Volume \n (mean across hemispheres)",color = 'Sex') + 
  facet_wrap(~Region,scales = "free_y")

antsCT_vs_age_by_sex <- CT %>%
  filter(!(Subject.Id %in% failed_IDs)) %>%
  left_join(demo) %>%
  pivot_longer(cols = c(free_CT,ants_CT),names_to = "Pipeline",values_to = "CT") %>%
  mutate(Pipeline = case_when(Pipeline == "free_CT" ~ "Freesurfer",Pipeline == "ants_CT" ~ "ANTs",TRUE ~ NA_character_)) %>%
  group_by(Subject.Id,Session.Id,Region,Pipeline) %>% 
  mutate(CT = mean(CT,na.rm = T)) %>% 
  ungroup() %>% 
  distinct(Subject.Id,Session.Id,Region,Pipeline,.keep_all = T) %>% 
  select(-Hemisphere) %>% 
  filter(Pipeline == 'ANTs') %>% 
  ggplot(aes(x = AGE,y = CT,color = SEX,group = interaction(Subject.Id,SEX))) + 
  geom_smooth(aes(group = SEX),method = 'gam',formula = y ~ s(x,bs = 'tp')) + 
  geom_line(alpha = .1) + facet_wrap(~Region,scales = "free") + 
  labs(x = "Age",y = "Cortical Thickness \n (mean across hemispheres)",color = 'Sex')

antsGMD_vs_age_by_sex <- GMD %>% 
  left_join(demo) %>% 
  group_by(Subject.Id,Timepoint,Region) %>% 
  mutate(GMD = mean(GMD,na.rm = T)) %>% 
  select(-Hemisphere) %>% 
  ungroup() %>% 
  distinct(Subject.Id,Timepoint,Region,.keep_all = T) %>% 
  ggplot(aes(x = AGE,y = GMD,color = SEX)) + geom_line(aes(group = Subject.Id),alpha = .1,size = .5) +
  geom_smooth(method = 'gam',formula = y ~ s(x,bs = 'tp')) + facet_wrap(~Region,scales = 'free') + 
  labs(x = 'Age',y = 'GMD \n (average across hemispheres)',color = 'Sex') 

GMD %>% 
  left_join(demo) %>% 
  group_by(Subject.Id,Timepoint,Region) %>% 
  mutate(GMD = mean(GMD,na.rm = T)) %>% 
  select(-Hemisphere) %>% 
  ungroup() %>% 
  distinct(Subject.Id,Timepoint,Region,.keep_all = T) %>% 
  filter(Timepoint == 1) %>% 
  ggplot(aes(x = AGE,y = GMD,color = SEX)) + geom_point(size = .1,alpha = .3) + 
  geom_smooth(method = 'gam',formula = y ~ s(x,bs = 'tp')) + facet_wrap(~Region,scales = 'free') + 
  labs(x = 'Age',y = 'GMD \n (average across hemispheres)',color = 'Sex') 


ants_CT %>% 
  left_join(demo) %>% 
  left_join(scanner_info) %>% 
  pivot_longer(matches('^lh\\.|^rh\\.'),names_to = 'Region',values_to = 'CT') %>% 
  clean_Region_names() %>% 
  group_by(Subject.Id,Session.Id,Region) %>% 
  mutate(CT = mean(CT)) %>% 
  distinct(Subject.Id,Session.Id,Region,.keep_all = T) %>% 
  select(-Hemisphere) %>% 
  ggplot(aes(x = AGE,y = CT,color = SEX,group = interaction(Subject.Id,SEX))) + geom_line(alpha = .1) + 
  geom_smooth(aes(group = SEX),method = 'gam',formula = y ~ s(x,bs = 'tp')) + 
  labs(x = "Age",y = "Cortical Thickness \n (mean across hemispheres)",color = 'Sex') + 
  facet_wrap(~Region,scales = "free_y")

ants_GMD %>% 
  left_join(demo) %>% 
  left_join(scanner_info) %>% 
  pivot_longer(matches('^lh\\.|^rh\\.'),names_to = 'Region',values_to = 'GMD') %>% 
  clean_Region_names() %>% 
  group_by(Subject.Id,Session.Id,Region) %>% 
  mutate(GMD = mean(GMD)) %>% 
  distinct(Subject.Id,Session.Id,Region,.keep_all = T) %>% 
  select(-Hemisphere) %>% 
  mutate(Subject.Id = str_remove_all(Subject.Id,pattern = '^0')) %>% 
  #filter(Subject.Id %in% as.character(pnc_gmd$bblid)) %>% 
  ggplot(aes(x = AGE,y = GMD,color = SEX,group = interaction(Subject.Id,SEX))) + geom_line(alpha = .1) + 
  geom_smooth(aes(group = SEX),method = 'gam',formula = y ~ s(x,bs = 'tp')) + 
  labs(x = "Age",y = "GMD \n (mean across hemispheres)",color = 'Sex') + 
  facet_wrap(~Region,scales = "free_y")

#pnc = readRDS('/Users/hillmann/Downloads/pnc1601.egenn.rds')
#
#pnc_gmd <-  pnc[[1]] %>% 
#  select(bblid,ageAtGo1Scan,sex,race,ethnicity,matches('mprage_mars_gmd')) 
#
#pnc_gmd %>% 
#  mutate(bblid = as.character(bblid)) %>% 
#  #left_join(scanner_info_cs,by = c('bblid' = 'Subject.Id')) %>% 
#  pivot_longer(matches('gmd'),names_to = 'Region',values_to = 'GMD') %>% 
#  filter(!is.na(GMD)) %>% 
#  ggplot(aes(x = ageAtGo1Scan,y = GMD)) + geom_point(size = .3) + geom_smooth() + 
#  facet_wrap(~Region)

  