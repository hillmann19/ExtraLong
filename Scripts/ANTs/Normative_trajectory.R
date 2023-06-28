# Load in packages and data
library(tidyverse)
library(longCombat)
library(missForest)
library(gamm4)
library(ggseg)
library(broom.mixed)
library(parallel)
library(ggpubr)
files.sources = list.files(path = '/Users/hillmann/Projects/visreg/R',full.names = T)
sapply(files.sources, source)
df <- read_csv('/Users/hillmann/Projects/ExtraLong/Data/ANTs/Tabulated/img_data_matched_with_freesurfer_2023_05_01.csv')
go1_diag <- read_csv('/Users/hillmann/Projects/ExtraLong/Data/Go1/n9498_diagnosis_dxpmr7_20170509.csv')
go1_cnb <- read_csv('/Users/hillmann/Projects/ExtraLong/Data/Go1/n9498_cnb_factor_scores_frar_20170202.csv')
demo <- read_csv('/Users/hillmann/Projects/ExtraLong/Data/ANTs/Tabulated/demographics+exclusion_datafreeze-2021_euler-212_minspan-180.csv')
eTIV <- read_csv('/Users/hillmann/Projects/ExtraLong/Data/FreesurferLongitudinal2021/TabulatedQC/Longitudinal/aparc_volume_rh_2022-02-24.csv') %>% 
  select(bblid,seslabel,eTIV) %>% 
  left_join(demo,by = c('bblid' = 'subid','seslabel' = 'sesid')) %>% 
  select(bblid,doscan,eTIV) 
mean_thickness_rh <- read_csv('/Users/hillmann/Projects/ExtraLong/Data/FreesurferLongitudinal2021/TabulatedQC/Longitudinal/aparc_thickness_rh_2022-02-24.csv') %>% 
  select(bblid,seslabel,rh_MeanThickness_thickness) %>% 
  left_join(demo,by = c('bblid' = 'subid','seslabel' = 'sesid')) %>% 
  select(bblid,doscan,rh_MeanThickness_thickness) 

mean_thickness_lh <- read_csv('/Users/hillmann/Projects/ExtraLong/Data/FreesurferLongitudinal2021/TabulatedQC/Longitudinal/aparc_thickness_lh_2022-02-24.csv') %>% 
  select(bblid,seslabel,lh_MeanThickness_thickness) %>% 
  left_join(demo,by = c('bblid' = 'subid','seslabel' = 'sesid')) %>% 
  select(bblid,doscan,lh_MeanThickness_thickness) 

# Run longitudinal combat to remove scanner effects

df_with_age <- df %>% 
  left_join(eTIV) %>% 
  left_join(mean_thickness_lh) %>% 
  left_join(mean_thickness_rh) %>% 
  group_by(bblid) %>% 
  arrange(doscan) %>% 
  mutate(n_timepoints = n()) %>% 
  mutate(age_at_scan = as.numeric(doscan - DOBIRTH)/365.25) %>% 
  mutate(age_at_baseline = age_at_scan[1]) %>% 
  mutate(time_since_baseline = age_at_scan - age_at_baseline) %>% 
  ungroup() %>% 
  mutate(sex = case_when(sex == 1 ~ 'Male',sex == 2 ~ 'Female',TRUE ~ NA_character_)) %>% 
  mutate(race = case_when(race == 1 ~ 'White',race == 2 ~ 'Black/African American',race == 3 ~ 'Native American',
                          race == 4 ~ 'Asian',race == 5 ~ 'More than one race',race == 6 ~ 'Hawaiian/Pacific Islander',
                          race == 9 ~ NA_character_,TRUE ~ NA_character_)) %>% 
  mutate(ethnic = case_when(ethnic == 1 ~ 'Hispanic',ethnic == 2 ~ 'Not Hispanic',ethnic == 9 ~ 'Unknown',TRUE ~ NA_character_)) %>%  
  mutate(race3 = case_when(race %in% c('Native American','More than one race','Hawaiian/Pacific Islander','Asian') ~ 'Other',
                                   TRUE ~ race)) %>% 
  mutate(sex = factor(sex,levels = c('Female','Male')),
         race3 = factor(race3,levels = c('White','Black/African American','Other'))) %>% 
  mutate(scanner = ifelse(is.na(scanner),'TrioTim',scanner)) %>% 
  left_join(go1_diag) %>% 
  left_join(go1_cnb) 

vol_for_combat_ants <- df_with_age %>% 
  select(bblid,scanner,age_at_scan,sex,matches('_vol_ants$'))
  
vol_for_combat_free <- df_with_age %>% 
  select(bblid,scanner,age_at_scan,sex,matches('_vol_free$'),eTIV)

regions_vol_ants <- vol_for_combat_ants %>% 
  select(matches('_vol_ants$')) %>% 
  colnames()

regions_vol_free <- vol_for_combat_free %>% 
  select(matches('_vol_free$'),eTIV) %>% 
  colnames()

ct_for_combat_ants <- df_with_age %>% 
  select(bblid,scanner,age_at_scan,sex,matches('_ct_ants$'))

ct_for_combat_free <- df_with_age %>% 
  select(bblid,scanner,age_at_scan,sex,matches('_ct_free$'),matches('MeanThickness'))

regions_ct_ants <- ct_for_combat_ants %>% 
  select(matches('_ct_ants$')) %>% 
  colnames()

regions_ct_free <- ct_for_combat_free %>% 
  select(matches('_ct_free$'),matches('MeanThickness')) %>% 
  colnames()

gmd_for_combat <- df_with_age %>% 
  select(bblid,scanner,age_at_scan,sex,matches('_gmd_ants$')) 

regions_gmd <- gmd_for_combat %>% 
  select(matches('_gmd_ants$')) %>% 
  colnames()

vol_combat_ants <- longCombat(idvar='bblid', 
                             timevar='age_at_scan',
                             batchvar='scanner', 
                             features=regions_vol_ants, 
                             formula='age_at_scan + I(age_at_scan^2) + I(age_at_scan^3) + 
                         sex + age_at_scan:sex',
                             ranef='(1|bblid)',
                             data=vol_for_combat_ants)$data_combat

vol_combat_free <- longCombat(idvar='bblid', 
                              timevar='age_at_scan',
                              batchvar='scanner', 
                              features=regions_vol_free, 
                              formula='age_at_scan + I(age_at_scan^2) + I(age_at_scan^3) + 
                         sex + age_at_scan:sex',
                              ranef='(1|bblid)',
                              data=vol_for_combat_free)$data_combat

ct_combat_ants <- longCombat(idvar='bblid', 
                         timevar='age_at_scan',
                         batchvar='scanner', 
                         features=regions_ct_ants, 
                         formula='age_at_scan + I(age_at_scan^2) + I(age_at_scan^3) + 
                         sex + age_at_scan:sex',
                         ranef='(1|bblid)',
                         data=ct_for_combat_ants)$data_combat

ct_combat_free <- longCombat(idvar='bblid', 
                             timevar='age_at_scan',
                             batchvar='scanner', 
                             features=regions_ct_free, 
                             formula='age_at_scan + I(age_at_scan^2) + I(age_at_scan^3) + 
                         sex + age_at_scan:sex',
                             ranef = '(1|bblid)',
                             data=ct_for_combat_free)$data_combat

gmd_combat <- longCombat(idvar='bblid', 
                         timevar='age_at_scan',
                         batchvar='scanner', 
                         features=regions_gmd, 
                         formula='age_at_scan + I(age_at_scan^2) + I(age_at_scan^3) + 
                         sex + age_at_scan:sex',
                         ranef='(1|bblid)',
                         data=gmd_for_combat)$data_combat


clean_Region_names <- function(df){
  df <- df %>% 
    mutate(Hemisphere = case_when(str_detect(Region,pattern = "^rh_") ~ "Right",str_detect(Region,pattern = "^lh_") ~ "Left",TRUE ~ NA_character_)) %>% 
    mutate(Region = str_replace_all(Region,pattern = '_free\\.combat$',replacement = '')) %>% 
    mutate(Region = str_replace_all(Region,pattern = '_ants\\.combat$',replacement = '')) %>% 
    mutate(Region = str_replace_all(Region,pattern = '_vol$|_ct$|_gmd$',replacement = '')) %>% 
    mutate(Region = str_replace_all(Region,pattern = "^[lr]h_",replacement = "")) %>% 
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

df_for_mods <- df_with_age %>% 
  left_join(vol_combat_ants) %>% 
  left_join(ct_combat_ants) %>% 
  left_join(gmd_combat) %>% 
  left_join(vol_combat_free) %>% 
  left_join(ct_combat_free)

1 - mean(is.na(df_for_mods$CNB_img_date_diff))
1 - mean(is.na(df_for_mods$Diag_img_date_diff))
1 - mean(is.na(df_for_mods$SIPS_img_date_diff))
1 - mean(is.na(df_for_mods$Ind_ses_img_date_diff))
1 - mean(is.na(df_for_mods$Env_img_date_diff))

theme_update(legend.position = 'bottom')

df_env_start <- df_for_mods %>% 
  filter(!is.na(Wealth_perm)) %>% 
  group_by(bblid) %>% 
  arrange(doscan) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  mutate(Wealth_ext = case_when(Wealth_perm > 1 ~ 'High',
                                Wealth_perm <= 1 & Wealth_perm >= -1 ~ 'Moderate',
                                Wealth_perm < -1 ~ 'Low')) %>% 
  mutate(Wealth_ext = factor(Wealth_ext,levels = c('Low','Moderate','High'))) %>% 
  ungroup() %>% 
  rename(Wealth_t1 = Wealth_perm,Wealth_t1_discrete = Wealth_ext) %>% 
  select(bblid,Wealth_t1,Wealth_t1_discrete)

df_env_end <- df_for_mods %>% 
  filter(!is.na(Wealth_perm)) %>% 
  group_by(bblid) %>% 
  arrange(doscan) %>% 
  slice_tail(n = 1) %>% 
  ungroup() %>% 
  mutate(Wealth_ext = case_when(Wealth_perm > 1 ~ 'High',
                                Wealth_perm <= 1 & Wealth_perm >= -1 ~ 'Moderate',
                                Wealth_perm < -1 ~ 'Low')) %>% 
  mutate(Wealth_ext = factor(Wealth_ext,levels = c('Low','Moderate','High'))) %>% 
  ungroup() %>% 
  rename(Wealth_last = Wealth_perm,Wealth_last_discrete = Wealth_ext) %>% 
  select(bblid,Wealth_last,Wealth_last_discrete)

df_cnb_start <- df_for_mods %>% 
  mutate(across(.cols = Overall_Accuracy:F4_Executive_Efficiency,.fns = ~case_when(.x >= 1 ~ 'High',
                                                                                   .x < 1 & .x >= -1 ~ 'Moderate',
                                                                                   .x <= -1 ~ 'Low',
                                                                                   TRUE ~ NA_character_),.names = '{.col}_grouped')) %>% 
  mutate(across(.cols = Overall_Accuracy_grouped:F4_Executive_Efficiency_grouped,.fns = ~ factor(.x,levels = c('Low','Moderate','High')))) %>% 
  mutate(across(.cols = Overall_Accuracy_grouped:F4_Executive_Efficiency_grouped,.fns = ~ factor(.x,levels = c('Low','Moderate','High'),ordered = T),
                .names = '{.col}_or')) %>% 
  mutate(across(.cols = Overall_Accuracy_grouped:F4_Executive_Efficiency_grouped,.fns = ~ factor(.x,levels = c('Low','High')),.names = '{.col}_hiLo')) %>% 
  mutate(across(.cols = Overall_Accuracy_grouped:F4_Executive_Efficiency_grouped,.fns = ~ factor(.x,levels = c('Low','High'),ordered = T),
                .names = '{.col}_hiLo_or')) 
  

df_for_gams <- df_for_mods %>% 
  mutate(across(.cols = Overall_Accuracy:F4_Executive_Efficiency,.fns = ~ case_when(.x >= 1 ~ 'High',
                                                                                  .x < 1 & .x >= -1 ~ 'Moderate',
                                                                                  .x <= -1 ~ 'Low',
                                                                                  TRUE ~ NA_character_),.names = '{.col}_grouped')) %>% 
  mutate(across(.cols = Overall_Accuracy_grouped:F4_Executive_Efficiency_grouped,.fns = ~ factor(.x,levels = c('Low','Moderate','High')))) %>% 
  mutate(across(.cols = Overall_Accuracy_grouped:F4_Executive_Efficiency_grouped,.fns = ~ factor(.x,levels = c('Low','Moderate','High'),ordered = T),
                .names = '{.col}_or')) %>% 
  mutate(across(.cols = Overall_Accuracy_grouped:F4_Executive_Efficiency_grouped,.fns = ~ factor(.x,levels = c('Low','High')),.names = '{.col}_hiLo')) %>% 
  mutate(across(.cols = Overall_Accuracy_grouped:F4_Executive_Efficiency_grouped,.fns = ~ factor(.x,levels = c('Low','High'),ordered = T),
                .names = '{.col}_hiLo_or')) %>% 
  mutate(race2 = case_when(race == 'White' ~ 'White',
                           race != 'White' ~ 'Non-White',
                           TRUE ~ NA_character_)) %>% 
  mutate(race2 = factor(race2,c('White','Non-White'))) %>% 
  mutate(race2_or = factor(race2,c('White','Non-White'),ordered = T)) %>% 
  mutate(race3 = factor(race3,levels = c('White','Other','Black/African American'))) %>%
  mutate(race3_or = factor(race3,levels = c('White','Other','Black/African American'),ordered = T)) %>%
  mutate(sex = factor(sex,levels = c('Male','Female'))) %>% 
  mutate(sex_or = factor(sex,levels = c('Male','Female'),ordered = T)) %>% 
  mutate(goassessDxpmr7 = factor(goassessDxpmr7,levels = c('TD','OP','PS'))) %>% 
  mutate(goassessDxpmr7_or = factor(goassessDxpmr7,levels = c('TD','OP','PS'),ordered = T)) %>% 
  mutate(PS = factor(goassessDxpmr7,levels = c('TD','PS'))) %>% 
  mutate(PS_or = factor(goassessDxpmr7,levels = c('TD','PS'),ordered = T)) %>% 
  left_join(df_env_start) %>% 
  left_join(df_env_end) %>% 
  mutate(Wealth_t1_discrete_hiLo = factor(Wealth_t1_discrete,levels = c('Low','High'))) %>% 
  mutate(Wealth_t1_discrete_or = factor(Wealth_t1_discrete,levels = c('Low','Moderate','High'),ordered = T)) %>% 
  mutate(Wealth_t1_discrete_hiLo_or = factor(Wealth_t1_discrete,levels = c('Low','High'),ordered = T)) %>% 
  mutate(Wealth_last_discrete_hiLo = factor(Wealth_last_discrete,levels = c('Low','High'))) %>% 
  mutate(Wealth_last_discrete_or = factor(Wealth_last_discrete,levels = c('Low','Moderate','High'),ordered = T)) %>% 
  mutate(Wealth_last_discrete_hiLo_or = factor(Wealth_last_discrete,levels = c('Low','High'),ordered = T)) 

run_mods <- function(region,eq,data){
  df_region <- data %>% 
    filter(Region == region)
  
  f <- as.formula(eq)
  
  m <- gamm4(f,random = ~(1|bblid),data = df_region)
  sum_m <- summary(m$gam)
  m_output_df <- tibble(Region = region,Terms = c(rownames(sum_m$p.table),rownames(sum_m$s.table)),
                        statistic = as.numeric(c(sum_m$p.table[,3],sum_m$s.table[,3])),
                        p_val = as.numeric(c(sum_m$p.table[,4],sum_m$s.table[,4])))
  return(m_output_df)
}

get_effects_on_ggseg <- function(eq,effect_of_interest,metric,pipeline,adjusted = T){
  
  df_for_gams_long <- df_for_gams %>% 
    pivot_longer(cols  = matches(paste0(metric,'_',pipeline,'.combat$')),names_to = 'Region',values_to = 'Vals') 
  
  output_df <- parallel::mclapply(unique(df_for_gams_long$Region),run_mods,eq = eq,data = df_for_gams_long,mc.cores = 8) %>% 
    bind_rows()
  
  theme_update(legend.position = 'bottom')
  
  if(adjusted == TRUE){
    p <- output_df %>% 
      mutate(Region = str_remove_all(Region,pattern = paste0('_',metric,'_',pipeline,'.combat$'))) %>% 
      filter(Terms == effect_of_interest) %>% 
      mutate(p_adjusted = p.adjust(p_val,method = 'fdr')) %>% 
      filter(p_adjusted < .05) %>% 
      rename(label = Region) %>% 
      ggseg(atlas = dk, mapping = aes(fill = statistic), color = "grey75") +
              scale_fill_gradientn(
                colors = c("dodgerblue4", "white", "tomato3"),
                na.value = "grey80", 
                limits = c(-5, 5),
                breaks = seq(-5,5,by = 5)) +
              labs(title = "", fill = "Statistic")
  } else{
    p <- output_df %>% 
      mutate(Region = str_remove_all(Region,pattern = paste0('_',metric,'_',pipeline,'.combat$'))) %>% 
      filter(Terms == effect_of_interest) %>% 
      filter(p_val < .05) %>% 
      rename(label = Region) %>% 
      ggseg(atlas = dk, mapping = aes(fill = statistic), color = "grey75") +
      scale_fill_gradientn(
        colors = c("dodgerblue4", "white", "tomato3"),
        na.value = "grey80",
        limits = c(-5, 5),
       breaks = seq(-5,5,by = 5)) +
      labs(title = "", fill = "Statistic")
  }
  return(p)
}

unadjusted_ants_main <- get_effects_on_ggseg(eq = 'Vals ~ s(age_at_scan) + sex + goassessDxpmr7 + n_timepoints + s(age_at_scan,by = goassessDxpmr7_or)',
                     effect_of_interest = 'goassessDxpmr7PS',metric = 'gmd',pipeline = 'ants',adjusted = F) + labs(title = 'Unadjusted')

adjusted_ants_main <- get_effects_on_ggseg(eq = 'Vals ~ s(age_at_scan) + sex + goassessDxpmr7 + n_timepoints + s(age_at_scan,by = goassessDxpmr7_or)',
                                 effect_of_interest = 'goassessDxpmr7PS',metric = 'gmd',pipeline = 'ants',adjusted = T) + labs(title = 'FDR-Corrected')

unadjusted_ants_int <- get_effects_on_ggseg(eq = 'Vals ~ s(age_at_scan) + sex + goassessDxpmr7 + n_timepoints + s(age_at_scan,by = goassessDxpmr7_or)',
                                        effect_of_interest = 's(age_at_scan):goassessDxpmr7_orPS',metric = 'gmd',pipeline = 'ants',adjusted = F) + labs(title = 'Unadjusted')

adjusted_ants_int <- get_effects_on_ggseg(eq = 'Vals ~ s(age_at_scan) + sex + goassessDxpmr7 + n_timepoints + s(age_at_scan,by = goassessDxpmr7_or)',
                                      effect_of_interest = 's(age_at_scan):goassessDxpmr7_orPS',metric = 'gmd',pipeline = 'ants',adjusted = T) + labs(title = 'FDR-Corrected')

ggarrange(unadjusted_ants_main,adjusted_ants_main,unadjusted_ants_int,adjusted_ants_int)


visualize_effect <- function(eq,x_axis,region,interact_with = NULL,t,metric,pipeline){
  
  df_for_gams_long <- df_for_gams %>% 
    pivot_longer(cols  = matches(paste0(metric,'_',pipeline,'.combat$')),names_to = 'Region',values_to = 'Vals') 
  df_region <- df_for_gams_long %>% 
    mutate(Region = str_remove_all(Region,pattern = paste0('_',metric,'_',pipeline,'.combat$'))) %>% 
    filter(Region == region) 
  f <- as.formula(eq)
  
  m <- gamm4(f,random = ~(1|bblid),data = df_region)
  m$gam$data <- df_region
  if(is.null(interact_with)){
    visreg(m$gam,xvar = x_axis,overlay = T,gg = T) + theme_minimal() + labs(title = t)
  } else{
    visreg(m$gam,xvar = x_axis,by = interact_with,overlay = T,gg = T) + theme_minimal() + 
      labs(color = '',fill = '',title = t,x = 'Age')
  }
}

visualize_effect(eq = 'Vals ~ s(age_at_scan,k = 4,fx = T) + sex + goassessDxpmr7_or + n_timepoints + s(age_at_scan,by = goassessDxpmr7_or,k = 4,fx = T)',
                 x_axis = 'age_at_scan',interact_with = 'goassessDxpmr7_or',region = 'lh_supramarginal',
                 t = 'Left Hemisphere: Supramarginal',metric = 'vol',pipeline = 'free')


# Plots comparing those in study vs those out of study




# Exploratory Plots

#theme_set(theme_minimal())
#theme_update(text = element_text(size = 14),
#             legend.key.size = unit(1, 'cm'), #change legend key size
#             legend.key.height = unit(.8, 'cm'), #change legend key height
#             legend.key.width = unit(.8, 'cm'), #change legend key width
#             legend.title = element_text(size=14), #change legend title font size
#             legend.text = element_text(size=14),
#             legend.position = 'bottom')
#  
#df_for_mods %>% 
#  left_join(ps_by_ind) %>% 
#  filter(!is.na(ps_last)) %>% 
#  left_join(df_ps_N) %>% 
#  mutate(ps_last_n = paste0(ps_last,' (N = ',N,')')) %>% 
#  mutate(ps_last_n = factor(ps_last_n,levels = c('TD (N = 281)','OP (N = 154)',
#                                                 'PS (N = 221)'))) %>% 
#  pivot_longer(cols  = matches('vol_free.combat$'),names_to = 'Region',values_to = 'Volume') %>% 
#  clean_Region_names() %>% 
#  group_by(bblid,time_since_baseline,Region) %>% 
#  mutate(Vol = mean(Volume)) %>% 
#  distinct(bblid,time_since_baseline,Region,.keep_all = T) %>% 
#  select(-Hemisphere) %>% 
#  ggplot(aes(x = age_at_scan,y = Vol,color = ps_last_n,group = interaction(bblid,ps_last_n))) + 
#  # geom_line(alpha = .1) + 
#  geom_smooth(aes(group = ps_last_n),method = 'gam',formula = y ~ s(x,bs = 'tp')) + 
#  labs(x = "Age",y = "Volume \n (mean across hemispheres)",color = '') + 
#  facet_wrap(~Region,scales = "free_y") + scale_color_brewer(palette = 'Set1') + 
#  scale_y_continuous(breaks = pretty_breaks(n = 3))
#
#df_for_mods %>% 
#  left_join(df_env_start) %>% 
#  filter(!is.na(Wealth_ext)) %>% 
#  pivot_longer(cols  = matches('vol.combat$'),names_to = 'Region',values_to = 'Volume') %>% 
#  clean_Region_names() %>% 
#  group_by(bblid,time_since_baseline,Region) %>% 
#  mutate(Vol = mean(Volume)) %>% 
#  distinct(bblid,time_since_baseline,Region,.keep_all = T) %>% 
#  select(-Hemisphere) %>% 
#  ggplot(aes(x = age_at_scan,y = Vol,color = Wealth_ext_n,group = interaction(bblid,Wealth_ext_n))) + 
#  #geom_line(alpha = .1) + 
#  geom_smooth(aes(group = Wealth_ext_n),method = 'gam',formula = y ~ s(x,bs = 'tp')) + 
#  labs(x = "Age",y = "Volume \n (mean across hemispheres)",color = '') + 
#  facet_wrap(~Region,scales = "free_y") + scale_color_brewer(palette = 'Set1') + 
#  scale_y_continuous(breaks = pretty_breaks(n = 3))

#df_for_mods %>% 
#  pivot_longer(cols = c(CNB_img_date_diff,Diag_img_date_diff,SIPS_img_date_diff,
#                        Ind_ses_img_date_diff,Env_img_date_diff),names_to = 'Data_type',values_to = 'Days') %>% 
#  mutate(Data_type = str_remove_all(Data_type,pattern = '_img_date_diff')) %>% 
#  mutate(Data_type = case_when(Data_type == 'Diag' ~ 'Diagnosis',
#                               Data_type == 'Ind_ses' ~ 'Individual SES',
#                               Data_type == 'Env' ~ 'Neighborhood \n Environment',
#                               TRUE ~ Data_type)) %>% 
#  ggplot(aes(x = abs(Days),fill = Data_type),color = 'black') + geom_density() + 
#  facet_wrap(~Data_type) + labs(x = 'Days',fill = 'Data Type') + 
#  guides(fill = 'none')

