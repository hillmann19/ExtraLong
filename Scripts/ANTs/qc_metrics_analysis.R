# Read in packages and data
library(tidyverse)
library(table1)
qc <- read_csv('/Users/hillmann/Projects/ExtraLong/Data/Old/quality_2021-11-01.csv')
demo_el <- read_csv('/Users/hillmann/Projects/ExtraLong/Data/ANTs/Tabulated/demographics+exclusion_datafreeze-2021_euler-212_minspan-180.csv')
go1_diag <- read_csv('/Users/hillmann/Projects/ExtraLong/Data/Go1/n9498_diagnosis_dxpmr7_20170509.csv')
diag_all <- read_csv("/Users/hillmann/Projects/ExtraLong/Data/oracle/diagnosis_wsmryvars_20220920.csv") %>% 
  rename(bblid = BBLID) %>% 
  select(bblid,DODIAGNOSIS,matches('^dx',ignore.case = F)) %>% 
  mutate(DODIAGNOSIS = as.Date(DODIAGNOSIS,format = '%d-%b-%y')) 
removed_euler_ses_ids <- read_csv('/Users/hillmann/Projects/ExtraLong/Data/exclusion_datafreeze-2021_cutoff-212.csv') %>% 
  mutate(across(.cols = c(subid,sesid),.fns = ~ as.numeric(.x))) %>% 
  filter(exclude == T) %>% 
  pull(sesid)
  


# Acquisition metadata was not completely copied over into the Extra Long project for two scans, 
# so date of scan is hard coded in from the scans in their original project directories

scan_metadata <- read_csv('~/Projects/ExtraLong/Data/scan_metadata.csv') %>% 
  mutate(across(.cols = c(bblid,sesid),.fns = ~ as.numeric(.x))) %>% 
  mutate(AcquisitionDateTime = case_when(sesid == 10211 ~ as_datetime('2016-06-26T15:54:39.270000'),
                                         sesid == 5539 ~ as_datetime('2011-08-11T19:18:28.797500'),
                                         TRUE ~ AcquisitionDateTime)) %>% 
  mutate(AcquisitionDateTime = as.Date(AcquisitionDateTime)) %>% 
  rename(seslabel = sesid)

demo_bbl <- read_csv('/Users/hillmann/Projects/ExtraLong/Data/oracle/subjectvisitsall_v.csv') %>% 
  filter(BBLID >= 1000) %>% 
  select(BBLID,DOVISIT,DOBIRTH,SEX,RACE) %>% 
  mutate(DOBIRTH = as.Date(DOBIRTH,format = '%d-%b-%Y'),
         DOVISIT = as.Date(DOVISIT,format = '%d-%b-%Y')) %>% 
  rename_with(.fn = ~ str_to_lower(.x)) %>% 
  mutate(sex = factor(sex,levels = c('1','2'),labels = c('Male','Female')),
         race = factor(race,levels = c('1','2','3','4','5','6','9'),
                                       labels = c('White','Black/African American','Native American',
                                                  'Asian','More than one race','Hawaiian/Pacific Islander',
                                                  NA_character_))) %>% 
  mutate(race = fct_collapse(race,Other = c('Native American','Asian','More than one race','Hawaiian/Pacific Islander'))) %>% 
  distinct(bblid,.keep_all = T)


qc_cutoff <- qc %>% 
  filter(euler_total >= -2000) %>% 
  with((mean(euler_total) - 2*sd(euler_total)))

qc_with_demo <- qc %>% 
  mutate(exclude = case_when(euler_total <= qc_cutoff ~ 'Exclude: Euler',
                             seslabel %in% removed_euler_ses_ids ~ 'Exclude: Not Enough Timepoints',
                             seslabel %in% demo_el$sesid ~ 'Include',
                             TRUE ~ 'Exclude: Proximity')) %>% 
  left_join(scan_metadata[,c('bblid','seslabel','AcquisitionDateTime')]) %>% 
  rename(doscan = AcquisitionDateTime) %>% 
  left_join(demo_bbl) %>% 
  left_join(go1_diag) %>% 
  mutate(Age = as.numeric((doscan - dobirth)/365.25))

match_data <- function(input_df,match_df,input_date_col,match_date_col,date_diff_col,allow_duplicates = FALSE){
  input_bblid <- unique(input_df$bblid)
  
  if(!(input_bblid %in% match_df$bblid)){
    empty_match <- match_df %>% 
      slice_head(n = 1) %>% 
      mutate(across(.cols = 1:ncol(.),.fns = ~ NA))
    
    all_matches <- input_df %>% 
      left_join(empty_match)
    
    return(all_matches)
  } else{
    
    match_df_trim <- match_df %>% 
      filter(bblid == input_bblid)
    cntr <- 1
    all_matches <- data.frame()
    match_avail <- match_df_trim
    input_df_avail <- input_df
    
    while(nrow(input_df_avail) > 0){
      
      if(nrow(match_avail) == 0){
        matched_df <- input_df_avail %>% 
          slice_head(n = 1)
        all_matches <- bind_rows(all_matches,matched_df)
        input_df_avail <- input_df_avail %>% 
          slice(-1)
      } else{
        
        diff_mat <- abs(sapply(input_df_avail[[input_date_col]],'-',match_avail[[match_date_col]]))
        if(nrow(match_avail) == 1){
          diff_mat <- t(as.matrix(diff_mat))
        }
        best_match_loc <- as.vector(which(diff_mat == min(diff_mat),arr.ind = TRUE)[1,])
        matched_df <- bind_cols(input_df_avail %>% slice(best_match_loc[2]),
                                match_avail %>% select(-bblid) %>% slice(best_match_loc[1])) 
        
        all_matches <- bind_rows(all_matches,matched_df)
        
        if(allow_duplicates == FALSE){
          match_avail <- match_avail %>% 
            slice(-best_match_loc[1])
        }
        input_df_avail <- input_df_avail %>% 
          slice(-best_match_loc[2])
      }
    }
    
    all_matches <- tibble(all_matches)  
    all_matches[[date_diff_col]] <- as.numeric(all_matches[[input_date_col]] - all_matches[[match_date_col]])
    
    return(all_matches)
  }
}

qc_list <- qc_with_demo %>% 
  group_split(bblid)

diag_qc <- map_dfr(qc_list,match_data,match_df = diag_all,input_date_col = 'doscan',match_date_col = "DODIAGNOSIS",date_diff_col = "diag_date_diff")

diag_qc_clean <- diag_qc %>% 
  mutate(across(.cols = dx_none:last_col(),.fns = ~ ifelse(abs(diag_date_diff) < 365.25,.x,NA))) %>% 
  mutate(DODIAGNOSIS = as.Date(ifelse(is.na(DODIAGNOSIS),NA,DODIAGNOSIS),origin = "1970-01-01"))

diag_qc_clean %>% 
  mutate(dx_pscat2 = case_when(dx_pscat == 'noDSMdx' ~ 'TD',
                               dx_pscat == 'other' ~ 'OP',
                               dx_pscat == 'pro'|dx_pscat == 'psy' ~ 'PS',
                               TRUE ~ NA_character_)) %>% 
  mutate(diag_comb = case_when(!is.na(goassessDxpmr7) & !is.na(dx_pscat2) ~ dx_pscat2,
                               !is.na(goassessDxpmr7) & is.na(dx_pscat2) ~ goassessDxpmr7,
                               is.na(goassessDxpmr7) & !is.na(dx_pscat2) ~ dx_pscat2,
                               is.na(goassessDxpmr7) & is.na(dx_pscat2) ~ NA_character_)) %>% 
  mutate(diag_comb = factor(diag_comb,levels = c('TD','OP','PS'))) %>% 
  rename(Sex = sex,Race = race,Diagnosis = diag_comb,Euler = euler_total) %>% 
  table1(~ Age + Sex + Race + Diagnosis|exclude,data = .)


theme_update(legend.position = 'bottom')

diag_qc_clean %>% 
  filter(euler_total >= -2000) %>% 
  pivot_longer(matches('^cnr|euler_total'),names_to = 'Metric',values_to = 'Value') %>% 
  mutate(Metric = case_when(Metric == 'cnr_graycsf_lh' ~ 'Left Hemisphere: CNR Gray-CSF',
                            Metric == 'cnr_graycsf_rh' ~ 'Right Hemisphere: CNR Gray-CSF',
                            Metric == 'cnr_graywhite_lh' ~ 'Left Hemisphere: CNR Gray-White',
                            Metric == 'cnr_graywhite_rh' ~ 'Right Hemisphere: CNR Gray-White',
                            Metric == 'euler_total' ~ 'Euler Total')) %>% 
  mutate(Metric = factor(Metric,levels = c('Left Hemisphere: CNR Gray-CSF','Right Hemisphere: CNR Gray-CSF',
                                           'Left Hemisphere: CNR Gray-White','Right Hemisphere: CNR Gray-White',
                                           'Euler Total'))) %>% 
  ggplot(aes(x = Value,fill = exclude)) + geom_density(alpha = .5) + 
  labs(fill = '') + facet_wrap(~Metric,scale = 'free')

diag_qc_clean %>% 
  group_by(exclude) %>% 
  summarize(max = max(euler_total),min = min(euler_total))

