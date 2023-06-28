# Load in necessary data and packages
library(tidyverse)
cnb <- read_csv("/Users/hillmann/Projects/ExtraLong/Data/oracle/cnb_merged_webcnp_surveys_smryscores_allbbl_longform.csv")
sips <- read_csv("/Users/hillmann/Projects/ExtraLong/Data/oracle/sips.csv")
env <- read_csv("/Users/hillmann/Projects/Geocoding/Data/Feb2023/census_scores_16february2023_4-factor.csv")
go1_diag <- read_csv('/Users/hillmann/Projects/Geocoding/Data/Extra/go1_dx.csv')
diag <- read_csv("/Users/hillmann/Projects/ExtraLong/Data/oracle/diagnosis_wsmryvars_20220920.csv")
ind_ses <- read_csv("/Users/hillmann/Projects/Geocoding/Data/Individual_SES/SES_scores.csv")
demo_for_dob <- read_csv('/Users/hillmann/Projects/ExtraLong/Data/oracle/subject.csv')
img_demo <- read_csv('/Users/hillmann/Projects/ExtraLong/Data/ANTs/Tabulated/demographics+exclusion_datafreeze-2021_euler-212_minspan-180.csv')
vol_ants <- read_csv('~/Projects/ExtraLong/Data/ANTs/Tabulated/CorticalMetrics/Volume_DKT_01_25_2023.csv')
ct_ants <- read_csv('~/Projects/ExtraLong/Data/ANTs/Tabulated/CorticalMetrics/CorticalThickness_DKT_01_25_2023.csv')
gmd_ants <- read_csv('/Users/hillmann/Projects/ExtraLong/Data/ANTs/Tabulated/CorticalMetrics/GMD_DKT_01_25_2023.csv')
free_vol_lh <- read_csv("~/Projects/ExtraLong/Data/FreesurferLongitudinal2021/TabulatedQC/Longitudinal/lh_DKTatlas_volume_2022-02-24.csv")
free_vol_rh <- read_csv("~/Projects/ExtraLong/Data/FreesurferLongitudinal2021/TabulatedQC/Longitudinal/rh_DKTatlas_volume_2022-02-24.csv")
free_ct_lh <- read_csv("~/Projects/ExtraLong/Data/FreesurferLongitudinal2021/TabulatedQC/Longitudinal/lh_DKTatlas_thickness_2022-02-24.csv")
free_ct_rh <- read_csv("~/Projects/ExtraLong/Data/FreesurferLongitudinal2021/TabulatedQC/Longitudinal/rh_DKTatlas_thickness_2022-02-24.csv")
all_visits <- read_csv('/Users/hillmann/Projects/ExtraLong/Data/oracle/subjectvisitsall_v.csv')
screen <- read_csv('/Users/hillmann/Projects/ExtraLong/Data/oracle/fullscreen.csv')
img_look <- read_csv('/Users/hillmann/Projects/ExtraLong/Data/oracle/imglook.csv')

# Clean data to prepare for merges
all_visits_clean <- all_visits %>% 
  rename(bblid = BBLID) %>% 
  mutate(DOVISIT = str_replace_all(DOVISIT,pattern = '-00',replacement = '-20')) %>% 
  mutate(DOVISIT = as.Date(DOVISIT,format = '%d-%b-%Y'),
         DOINTAKE = as.Date(DOVISIT,format = '%d-%b-%Y')) %>% 
  select(bblid,DOVISIT,DOINTAKE,matches('^PERM'),matches('^CURRENT')) %>% 
  distinct(bblid,DOVISIT,DOINTAKE,.keep_all = T)

ind_ses_clean <- ind_ses %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(bblid = as.character(bblid)) %>% 
  filter(!is.na(date)) %>% 
  group_by(bblid,date) %>% 
  arrange(interview_id) %>% 
  slice_tail(n = 1) %>% 
  ungroup()

sips_trim <- sips %>% 
  filter(if_any(.cols = P1:GAF_C,.fns = ~ !is.na(.x))) %>% 
  rename(bblid = BBLID) %>% 
  mutate(DOSIPS = as.Date(DOSIPS,format = "%d-%b-%Y")) %>% 
  mutate(bblid = as.character(bblid)) %>% 
  rowwise() %>% 
  mutate(Vars_non_na = sum(!is.na(c_across(cols = P1:GAF_C)))) %>% 
  ungroup() %>% 
  group_by(bblid,DOSIPS) %>% 
  arrange(Vars_non_na) %>% 
  slice_tail(n = 1) %>% 
  ungroup() %>% 
  select(-Vars_non_na)

env_perm_trim <- env %>% 
  filter(Address_Type == "Permanent",!is.na(Wealth),BBLID != 12345) %>% 
  select(BBLID,date,Address_Type,Wealth,University_YUP,Polluted_Urban,Retired) %>% 
  mutate(date = as.Date(date,format = '%m/%d/%Y')) %>% 
  rename(bblid = BBLID,date_perm_address = date,Wealth_perm = Wealth,University_YUP_perm = University_YUP,Polluted_Urban_perm = Polluted_Urban,Retired_perm = Retired) %>% 
  mutate(bblid = as.character(bblid)) %>% 
  select(-Address_Type)

cnb_trim <- cnb %>% 
  select(test_sessions.bblid,test_sessions_v.dotest,test_sessions_v.dob,platform,mpraxis_rtcr,pcet_acc2,pcet_rtcr,cpt_ptp,cpt_tprt,lnb_mcr,lnb_mrtc,er40_cr,er40_rtcr,pvrt_cr,pvrt_rtcr,pmat_pc,pmat_rtcr,volt_cr,volt_rtcr,cpf_cr,cpf_rtcr,medf_pc,medf_rtcr,adt_pc,adt_rtcr,plot_pc,plot_rtcr,cpw_cr,cpw_rtcr,tap_tot,matches("_valid")) %>% 
  rename(bblid = test_sessions.bblid) %>% 
  filter(!is.na(bblid)) %>% 
  filter(if_any(.cols = mpraxis_rtcr:tap_tot,.fns = ~ !is.na(.x))) %>% 
  rowwise() %>% 
  mutate(Vars_non_na = sum(!is.na(c_across(cols = mpraxis_rtcr:tap_tot)))) %>% 
  ungroup() %>% 
  group_by(bblid,test_sessions_v.dotest) %>% 
  arrange(Vars_non_na) %>% 
  slice_tail(n = 1) %>% 
  ungroup() %>% 
  select(-Vars_non_na)

diag_trim <- diag %>% 
  rename(bblid = BBLID,PROTOCOL_diag = PROTOCOL,VISITNUM_diag = VISITNUM,TYPE_diag = TYPE) %>% 
  mutate(bblid = as.character(bblid)) %>% 
  mutate(DODIAGNOSIS = as.Date(DODIAGNOSIS,format = '%d-%b-%y')) 

go1_diag_trim <- go1_diag %>% 
  mutate(Date = as.Date(Date,format = '%m/%d/%Y')) %>% 
  filter(!is.na(Date))

img_demo <- img_demo %>% 
  mutate(subid = as.character(subid),sesid = as.character(sesid))

demo_for_dob <- demo_for_dob %>% 
  mutate(BBLID = as.character(BBLID))

vol_ants_clean <- vol_ants %>% 
  rename_with(.fn = ~ paste(.x,'vol_ants',sep = '_'),.cols = matches('^lh|^rh')) 

ct_ants_clean <- ct_ants %>% 
  rename_with(.fn = ~ paste(.x,'ct_ants',sep = '_'),.cols = matches('^lh|^rh'))

gmd_ants_clean <- gmd_ants %>% 
  rename_with(.fn = ~ paste(.x,'gmd_ants',sep = '_'),.cols = matches('^lh|^rh'))

colnames(vol_ants_clean) <- str_replace_all(colnames(vol_ants_clean),pattern = "\\.",replacement = "_")
colnames(ct_ants_clean) <- str_replace_all(colnames(ct_ants_clean),pattern = "\\.",replacement = "_")
colnames(gmd_ants_clean) <- str_replace_all(colnames(gmd_ants_clean),pattern = "\\.",replacement = "_")

vol_free_clean <- free_vol_lh %>% 
  left_join(free_vol_rh,by = c("bblid","seslabel")) %>% 
  select(!(matches(".x|.y|aparc.DKTatlas.volume|temporalpole|MeanThickness"))) %>% 
  rename(Subject.Id = bblid,Session.Id = seslabel) %>% 
  mutate(Subject.Id = as.character(Subject.Id),Session.Id = as.character(Session.Id)) %>% 
  mutate(Subject.Id = ifelse(str_length(Subject.Id) == 5,paste0("0",Subject.Id),Subject.Id)) %>% 
  mutate(Session.Id = ifelse(str_length(Session.Id) == 4,paste0("0",Session.Id),Session.Id)) %>% 
  rename_with(.fn = ~ paste(.x,'vol_free',sep = '_'),.cols = matches('^lh|^rh'))

ct_free_clean <- free_ct_lh %>% 
  left_join(free_ct_rh,by = c("bblid","seslabel")) %>% 
  select(!(matches(".x|.y|aparc.DKTatlas.thickness|temporalpole|MeanThickness"))) %>% 
  rename(Subject.Id = bblid,Session.Id = seslabel) %>% 
  mutate(Subject.Id = as.character(Subject.Id),Session.Id = as.character(Session.Id)) %>% 
  mutate(Subject.Id = ifelse(str_length(Subject.Id) == 5,paste0("0",Subject.Id),Subject.Id)) %>% 
  mutate(Session.Id = ifelse(str_length(Session.Id) == 4,paste0("0",Session.Id),Session.Id)) %>% 
  rename_with(.fn = ~ paste(.x,'ct_free',sep = '_'),.cols = matches('^lh|^rh'))

colnames(vol_free_clean) <- str_replace_all(colnames(vol_free_clean),pattern = "_volume",replacement = "")
colnames(ct_free_clean) <- str_replace_all(colnames(ct_free_clean),pattern = "_thickness",replacement = "")
colnames(vol_free_clean) <- str_replace_all(colnames(vol_free_clean),pattern = "\\.",replacement = "_")
colnames(ct_free_clean) <- str_replace_all(colnames(ct_free_clean),pattern = "\\.",replacement = "_")

all_img <- vol_ants_clean %>% 
  left_join(ct_ants_clean) %>% 
  left_join(gmd_ants_clean) %>% 
  left_join(vol_free_clean) %>% 
  left_join(ct_free_clean) %>% 
  mutate(across(.cols = c(Subject_Id,Session_Id),.fns = ~ str_remove_all(.x,pattern = '^0'))) %>% 
  left_join(img_demo,by = c('Subject_Id' = 'subid','Session_Id' = 'sesid')) %>% 
  left_join(demo_for_dob[,c('BBLID','DOBIRTH')],by = c('Subject_Id' = 'BBLID')) %>% 
  rename(bblid = Subject_Id) %>% 
  relocate(c(DOBIRTH,acq:exclude),.after = Session_Id) %>% 
  mutate(DOBIRTH = as.Date(DOBIRTH,format = '%d-%b-%Y')) %>% 
  select(-Session_Id)

# Match data to diagnosis data set -- each row in match_df is mapped to a max of 1 diagnosis

#input_df <- env_list[[1235]]
#match_df <- cnb_trim
#input_date_col <- 'date_perm_address'
#match_date_col <- 'test_sessions_v.dotest'
#date_diff_col <- 'CNB_env_date_diff'

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

# Troubleshooting addresses that weren't matched

#img_list <- all_img %>% 
#  group_split(bblid)
#
#visit_img <- map_dfr(img_list,match_data,match_df = all_visits_clean,input_date_col = 'doscan',match_date_col = "DOVISIT",date_diff_col = "Visit_img_date_diff")
#
#visit_img %>% 
#  relocate(DOVISIT:Visit_img_date_diff,.after = bblid) %>% 
#  relocate(c(doscan,Visit_img_date_diff),.after = DOVISIT) %>% 
#  filter(abs(Visit_img_date_diff) > 365.25|is.na(Visit_img_date_diff)) %>% 
#  View(title = 'Visits Matched')
#
#img_look %>% 
#  filter(BBLID == 107060) %>% 
#  View(title = 'IMG Look')
#
#all_visits_clean %>% 
#  filter(bblid == '107060') %>% 
#  arrange(DOVISIT) %>% 
#  View(title = 'Visits')
#
#all_img %>% 
#  filter(bblid == '104235') %>% 
#  arrange(doscan) %>% 
#  View(title = 'Imaging')
#
#screen %>% 
#  filter(BBLID == 100508) %>% 
#  View(title = 'Screen')

img_list <- all_img %>% 
  group_split(bblid)

cnb_img <- map_dfr(img_list,match_data,match_df = cnb_trim,input_date_col = 'doscan',match_date_col = "test_sessions_v.dotest",date_diff_col = "CNB_img_date_diff")

# Remove any CNBs that weren't within a year of the clinical assessment

cnb_img_clean <- cnb_img %>% 
  mutate(across(.cols = platform:last_col(),.fns = ~ ifelse(abs(CNB_img_date_diff) < 365.25,.x,NA))) %>% 
  mutate(test_sessions_v.dotest = as.Date(ifelse(is.na(CNB_img_date_diff),NA,test_sessions_v.dotest),origin = "1970-01-01")) %>% 
  relocate(CNB_img_date_diff,.after = test_sessions_v.dotest) 

# Create summary variables for CNB performance 

cnb_img_clean <- cnb_img_clean %>% 
  mutate(across(.cols = mpraxis_rtcr:tap_tot,.fns = ~ as.numeric(scale(.x)),.names = "{.col}_z"))

# Map sips data to individual SES and CNB 

cnb_img_clean_list <- cnb_img_clean %>% 
  group_split(bblid)

img_cnb_sips <- map_dfr(cnb_img_clean_list,match_data,match_df = sips_trim,input_date_col = 'doscan',match_date_col = "DOSIPS",date_diff_col = "SIPS_img_date_diff")

img_cnb_sips_clean <- img_cnb_sips %>% 
  relocate(DOSIPS,.before = PROTOCOL) %>% 
  mutate(across(.cols = PROTOCOL:last_col(),.fns = ~ ifelse(abs(SIPS_img_date_diff) < 365.25,.x,NA))) %>% 
  mutate(DOSIPS = as.Date(ifelse(abs(SIPS_img_date_diff) < 365.25,DOSIPS,NA),origin = "1970-01-01")) %>% 
  relocate(SIPS_img_date_diff,.after = DOSIPS) 

# Map neighborhood environment data

img_cnb_sips_clean_list <- img_cnb_sips_clean %>% 
  group_split(bblid)

img_cnb_sips_env <- map_dfr(img_cnb_sips_clean_list,match_data,match_df = env_perm_trim,input_date_col = 'doscan',match_date_col = "date_perm_address",date_diff_col = "Env_img_date_diff")

img_cnb_sips_env_clean <- img_cnb_sips_env %>% 
  mutate(across(.cols = Wealth_perm:last_col(),.fns = ~ ifelse(abs(Env_img_date_diff) < 365.25,.x,NA))) %>% 
  mutate(date_perm_address = as.Date(ifelse(abs(Env_img_date_diff) < 365.25,date_perm_address,NA),origin = "1970-01-01")) %>% 
  relocate(Env_img_date_diff,.after = date_perm_address) 

# Map diagnosis data to individual SES, IMG, and SIPS

add_diag_list <- img_cnb_sips_env_clean %>% 
  group_split(bblid)

diag_added <- map_dfr(add_diag_list,match_data,match_df = diag_trim,input_date_col = 'doscan',match_date_col = "DODIAGNOSIS",date_diff_col = "Diag_img_date_diff")

diag_added_clean <- diag_added %>% 
  relocate(DODIAGNOSIS,.before = CONSENSUS_TYPE) %>% 
  mutate(across(.cols = CONSENSUS_TYPE:last_col(),.fns = ~ ifelse(abs(Diag_img_date_diff) < 365.25,.x,NA))) %>% 
  mutate(DODIAGNOSIS = as.Date(ifelse(abs(Diag_img_date_diff) < 365.25,DODIAGNOSIS,NA),origin = "1970-01-01")) %>% 
  relocate(Diag_img_date_diff,.after = DODIAGNOSIS) 


# Same thing for individual SES

add_ind_ses_list <- diag_added_clean %>% 
  group_split(bblid)

add_ind_ses <- map_dfr(add_ind_ses_list,match_data,match_df = ind_ses_clean,input_date_col = 'doscan',match_date_col = "date",date_diff_col = "Ind_ses_img_date_diff")

add_ind_ses_clean <- add_ind_ses %>% 
  mutate(across(.cols = redcapid:last_col(),.fns = ~ ifelse(abs(Ind_ses_img_date_diff) < 365.25,.x,NA))) %>% 
  rename(date_ind_ses = date) %>% 
  mutate(date_ind_ses = as.Date(ifelse(abs(Ind_ses_img_date_diff) < 365.25,date_ind_ses,NA),origin = "1970-01-01")) %>% 
  relocate(Ind_ses_img_date_diff,.after = date_ind_ses) 

# Add in GO1 diagnosis data 

add_go1_diag_list <- add_ind_ses_clean %>% 
  group_split(bblid)

add_go1_diag <- map_dfr(add_go1_diag_list,match_data,match_df = go1_diag_trim,input_date_col = 'doscan',match_date_col = "Date",date_diff_col = "go1_diag_img_date_diff") 

add_go1_diag_clean <- add_go1_diag %>% 
  mutate(across(.cols = smry_mood_cat:last_col(),.fns = ~ ifelse(abs(go1_diag_img_date_diff) < 365.25,.x,NA))) %>% 
  rename(go1_diagnosis_date = Date) %>% 
  mutate(go1_diagnosis_date = as.Date(ifelse(is.na(go1_diag_img_date_diff),NA,go1_diagnosis_date),origin = "1970-01-01")) %>% 
  relocate(go1_diag_img_date_diff,.after = go1_diagnosis_date) 

write_csv(add_go1_diag_clean,file = '/Users/hillmann/Projects/ExtraLong/Data/ANTs/Tabulated/img_data_matched_with_freesurfer_2023_05_01.csv')
