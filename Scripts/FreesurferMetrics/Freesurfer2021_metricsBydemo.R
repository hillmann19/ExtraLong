# Read in the data and necessary packages 
library(tidyverse)
library(gtsummary)
library(ggpubr)
SA_lh <- read_csv("~/Projects/ExtraLong/Data/FreesurferLongitudinal2021/TabulatedQC/Longitudinal/aparc_area_lh_2022-02-24.csv")
SA_rh <- read_csv("~/Projects/ExtraLong/Data/FreesurferLongitudinal2021/TabulatedQC/Longitudinal/aparc_area_rh_2022-02-24.csv")
Vol_lh <- read_csv("~/Projects/ExtraLong/Data/FreesurferLongitudinal2021/TabulatedQC/Longitudinal/aparc_volume_lh_2022-02-24.csv")
Vol_rh <- read_csv("~/Projects/ExtraLong/Data/FreesurferLongitudinal2021/TabulatedQC/Longitudinal/aparc_volume_rh_2022-02-24.csv")
CT_lh <- read_csv("~/Projects/ExtraLong/Data/FreesurferLongitudinal2021/TabulatedQC/Longitudinal/aparc_thickness_lh_2022-02-24.csv")
CT_rh <- read_csv("~/Projects/ExtraLong/Data/FreesurferLongitudinal2021/TabulatedQC/Longitudinal/aparc_thickness_rh_2022-02-24.csv")
aseg <- read_csv("~/Projects/ExtraLong/Data/FreesurferLongitudinal2021/TabulatedQC/Longitudinal/aseg_stats_2022-02-24.csv")
demo <- read_csv("~/Projects/ExtraLong/Data/FreesurferLongitudinal2021/demographics+exclusion_datafreeze-2021_euler-212_minspan-180.csv")
quality2021 <- read_csv("~/Projects/ExtraLong/Data/quality_2021-11-01.csv")

demo <- demo %>% 
  rename(bblid = "subid",seslabel = "sesid") %>% 
  mutate(scanner = ifelse(str_detect(scanner,pattern = "Prisma"),"Prisma",scanner))




# Join hemispheres into one dataset, add in demographic information
SA <- SA_lh %>% 
  left_join(SA_rh) %>% 
  left_join(demo)

Vol <- Vol_lh %>% 
  left_join(Vol_rh) %>% 
  left_join(demo)

CT <- CT_lh %>% 
  left_join(CT_rh) %>% 
  left_join(demo)

# Create data set with all measurements that are needed

All <- SA %>% 
  left_join(Vol) %>% 
  left_join(CT) %>% 
  select(bblid,seslabel,scanage_years,sex,race,ethnic,contains("lh"),contains("rh")) %>% 
  select(!(contains("Mean") | contains("aparc") | contains("White"))) 

# Put into long format

All.long <- All %>% 
  pivot_longer(cols = lh_bankssts_area:rh_insula_thickness,names_to = "Region",values_to = "Measurement") %>% 
  separate(Region,into = c("hemi","Region","Metric"),sep = "_") 

# Create vectors to label which regions go into which lobes/sections

Frontal <-c("superiorfrontal","caudalmiddlefrontal","parsopercularis", "parsorbitalis","parstriangularis", "lateralorbitofrontal","medialorbitofrontal","precentral","paracentral","frontalpole")
  
Parietal <- c("superiorparietal","inferiorparietal","supramarginal","postcentral","precuneus")
  
Temporal <- c("superiortemporal","middletemporal", "inferiortemporal", "bankssts", "fusiform","transversetemporal","entorhinal","temporalpole","parahippocampal")
  
Occipital <- c("lateraloccipital","lingual","cuneus","pericalcarine")

All.long <- All.long %>% 
  mutate(Lobe = case_when(Region %in% Frontal ~ "Frontal", Region %in% Parietal ~ "Parietal",Region %in% Temporal ~ "Temporal",Region %in% Occipital ~ "Occipital",TRUE ~ NA_character_))

# Calculate SA weights for CT measurements

SA.weights <- SA %>% 
  select(bblid,seslabel,contains("lh"),contains("rh")) %>% 
  select(!(contains("Mean") | contains("aparc") | contains("White"))) %>% 
  pivot_longer(cols = c(contains("rh"),contains("lh")),names_to = "Region",values_to = "Measurement") %>% 
  separate(Region,into = c("hemi","Region","Metric"),sep = "_") %>% 
  mutate(Lobe = case_when(Region %in% Frontal ~ "Frontal", Region %in% Parietal ~ "Parietal",Region %in% Temporal ~ "Temporal",Region %in% Occipital ~ "Occipital",TRUE ~ NA_character_)) %>% 
  group_by(bblid,seslabel,Lobe,hemi) %>% 
  mutate(Percent_SA = Measurement/sum(Measurement)) %>% 
  ungroup() %>% 
  select(-Measurement) %>% 
  mutate(Metric = "thickness") %>% 
  filter(!is.na(Lobe))

# Add SA weights to the long All data set 

All.long <- All.long %>% 
  left_join(SA.weights)

# Volume by Lobe data set

Vol.by.lobe <- All.long %>% 
  filter(Metric == "volume") %>% 
  group_by(bblid,seslabel,hemi,Lobe) %>% 
  mutate(Volume = sum(Measurement)) %>% 
  ungroup() %>% 
  distinct(bblid,seslabel,hemi,Lobe,.keep_all = TRUE) %>% 
  select(-Measurement,-Percent_SA) %>% 
  group_by(bblid,seslabel,Lobe) %>% 
  mutate(Avg_by_hemi_measurement = mean(Volume)) %>% 
  ungroup() %>% 
  distinct(bblid,seslabel,Lobe,.keep_all = TRUE) %>% 
  filter(!is.na(Lobe)) %>% 
  select(-hemi,-Region,-Volume)

# Surface area by Lobe data set 

SA.by.lobe <- All.long %>% 
  filter(Metric == "area") %>% 
  group_by(bblid,seslabel,hemi,Lobe) %>% 
  mutate(SA = sum(Measurement)) %>% 
  ungroup() %>% 
  distinct(bblid,seslabel,hemi,Lobe,.keep_all = TRUE) %>% 
  select(-Measurement,-Percent_SA) %>% 
  group_by(bblid,seslabel,Lobe) %>% 
  mutate(Avg_by_hemi_measurement = mean(SA)) %>% 
  ungroup() %>% 
  distinct(bblid,seslabel,Lobe,.keep_all = TRUE) %>% 
  filter(!is.na(Lobe)) %>% 
  select(-hemi,-Region,-SA)

# CT by Lobe data set, with the Lobe CT weighted by surface area of the regions

CT.by.lobe <- All.long %>% 
  filter(Metric == "thickness",!is.na(Lobe)) %>% 
  group_by(bblid,seslabel,Lobe,hemi) %>% 
  mutate(CorticalThick = sum(Measurement*Percent_SA)) %>% 
  ungroup() %>% 
  distinct(bblid,seslabel,hemi,Lobe,.keep_all = TRUE) %>% 
  group_by(bblid,seslabel,Lobe) %>% 
  mutate(Avg_by_hemi_measurement = mean(CorticalThick)) %>% 
  ungroup() %>% 
  distinct(bblid,seslabel,Lobe,.keep_all = TRUE) %>% 
  select(-hemi,-Region,-CorticalThick,-Percent_SA,-Measurement)

### Combine CT, volume, and SA lobular totals back into one data set

All.by.lobe <- bind_rows(Vol.by.lobe,SA.by.lobe,CT.by.lobe)

### Relabel demographic data using key to map from numbers to useful sex/race/ethnicity information

All.by.lobe <- All.by.lobe %>% 
  mutate(sex = case_when(sex == 1 ~ "Male",sex == 2 ~ "Female",TRUE ~ NA_character_)) %>% 
  mutate(race = case_when(race == 1 ~ "White",race == 2 ~ "Black",race == 3 ~ "Native American",race == 4 ~ "Asian",race == 5 ~ "Biracial",race == 6 ~ "Hawaiian/Pacific Islander",race == 9 ~ "Unknown",TRUE ~ "Unknown")) %>% 
  mutate(ethnic = case_when(ethnic == 1 ~ "Hispanic",ethnic == 2 ~ "Not Hispanic",ethnic == 9 ~ "Unknown")) %>% 
  rename(age = "scanage_years")

# Split the All.by.lobe data set into a list for the createPlots function to be applied over 
All.by.lobe.split <- All.by.lobe %>% 
  group_split(Metric,Lobe)

createPlots <- function(df){
  metric <- unique(df$Metric)
  y_label<- case_when(metric == "thickness" ~ "Cortical Thickness",metric == "area" ~ "Surface Area",metric == "volume" ~ "Volume")
  Lobe <- unique(df$Lobe)
  LongitudinalPlot <- df %>% 
    mutate(Avg_by_hemi_measurement = scale(Avg_by_hemi_measurement)) %>% 
    ggplot(aes(x = age,y = Avg_by_hemi_measurement,color = sex,group = bblid)) + geom_smooth(aes(x = age,y = Avg_by_hemi_measurement,color = sex,group = sex)) + geom_point(size = .1) + geom_line(alpha = .25) + labs(x = "Age",y = paste(y_label,"Z-Score"),title = paste(Lobe,"Lobe",y_label,"by Sex and Age")) + theme_minimal() + scale_color_manual(values = c("#d7191c","#2b83ba")) 
  return(LongitudinalPlot)
}

# Generate all plots and write them to file
BrainMeasures.LongitudinalPlots <- map(All.by.lobe.split,createPlots)
  
  
# Create plots/summary statistics of 'Longitudinal profile'

demo <- demo %>% 
  mutate(sex = case_when(sex == 1 ~ "Male",sex == 2 ~ "Female",TRUE ~ NA_character_)) %>% 
  mutate(race = case_when(race == 1 ~ "White",race == 2 ~ "Black",race == 3 ~ "Native American",race == 4 ~ "Asian",race == 5 ~ "Multiracial",race == 6 ~ "Hawaiian/Pacific Islander",race == 9 ~ "Unknown",TRUE ~ "Unknown")) %>% 
  mutate(ethnic = case_when(ethnic == 1 ~ "Hispanic",ethnic == 2 ~ "Not Hispanic",ethnic == 9 ~ "Unknown")) %>% 
  rename(age = "scanage_years") %>% 
  mutate(scanner = ifelse(is.na(scanner),"TrioTim",scanner)) # The research subject with an NA for scanner is from the PNC, where every other scan was done on the TrioTrim scanner

StudyN <- demo %>% 
  group_by(acq) %>% 
  summarize(N_study = n())

theme_set(theme_classic())
Longitudinal_study <- demo %>% 
  left_join(StudyN) %>% 
  mutate(N_acq = paste0(acq," (Timepoints = ",N_study,")")) %>% 
  mutate(bblid = factor(bblid)) %>% 
  mutate(bblid = fct_reorder(bblid,age,min)) %>% 
  ggplot(aes(x = bblid,y = age,color = N_acq,group = bblid)) + geom_point(size = .75) + geom_line(alpha = .5) + labs(y = "Age",x = "bblid",color = "Study") + coord_flip() + theme(axis.text.y = element_blank())

SexN <- demo %>% 
  group_by(sex) %>% 
  summarize(N_by_sex = n())

Longitudinal_Sex <- demo %>% 
  left_join(SexN) %>% 
  mutate(N_sex = paste0(sex," (Timepoints = ",N_by_sex,")")) %>% 
  mutate(bblid = factor(bblid)) %>% 
  mutate(bblid = fct_reorder(bblid,age,min)) %>% 
  ggplot(aes(x = bblid,y = age,color = N_sex,group = bblid)) + geom_point(size = .75) + geom_line(alpha = .5) + labs(y = "Age",x = "bblid",color = "Sex") + coord_flip() + theme(axis.ticks.x = element_blank(),axis.text.y = element_blank()) + scale_color_manual(values = c("#ef8a62","#67a9cf"))
  
ScannerN <- demo %>% 
  group_by(scanner) %>% 
  summarize(N_by_scanner = n())

Longitudinal_Scanner <- demo %>% 
  left_join(ScannerN) %>% 
  mutate(N_scanner = paste0(scanner," (Timepoints = ",N_by_scanner,")")) %>% 
  mutate(bblid = factor(bblid)) %>% 
  mutate(bblid = fct_reorder(bblid,age,min)) %>% 
  ggplot(aes(x = bblid,y = age,color = N_scanner,group = bblid)) + geom_point(size = .75) + geom_line(alpha = .5) + labs(y = "Age",x = "bblid",color = "Scanner") + coord_flip() + theme(axis.ticks.x = element_blank(),axis.text.y = element_blank()) + scale_color_brewer(palette = "Dark2")

RaceN <- demo %>% 
  group_by(race) %>% 
  summarize(N_by_race = n())

Longitudinal_Race <- demo %>% 
  left_join(RaceN) %>% 
  mutate(N_race = paste0(race," (Timepoints = ",N_by_race,")")) %>% 
  mutate(bblid = factor(bblid)) %>% 
  mutate(bblid = fct_reorder(bblid,age,min)) %>% 
  ggplot(aes(x = bblid,y = age,color = N_race,group = bblid)) + geom_point(size = .75) + geom_line(alpha = .5) + labs(y = "Age",x = "bblid",color = "Race") + coord_flip() + theme(axis.ticks.x = element_blank(),axis.text.y = element_blank()) + scale_color_brewer(palette = "Paired")


demo_list <- demo %>% 
  group_split(bblid) 

create_ind_df <- function(bblid_df){
  num_timepoints <- unique(bblid_df$ntimepoints)
  bblid <- unique(bblid_df$bblid)
  sex <- unique(bblid_df$sex)
  race <- unique(bblid_df$race)
  ethnic <- unique(bblid_df$ethnic)
  age1 <- bblid_df %>% 
    filter(timepoint == 1) %>% 
    pull(age)
  ageLast <- bblid_df %>% 
    filter(timepoint == max(timepoint)) %>% 
    pull(age)
  
  ind_df <- data.frame("bblid" = bblid,"sex" = sex,"race" = race,"ethnic" = ethnic,"Age at first timepoint" = age1,"Age at last timepoint" = ageLast,"Number of Timepoints" = num_timepoints)
  return(ind_df)
}

ind_demo <- map_dfr(demo_list,create_ind_df)

# Tables representing demographis and some timepoint data at the individual level

ind_demo %>% 
  select(-bblid,-race,-ethnic,-sex) %>% 
  tbl_summary(label = list(Age.at.first.timepoint ~ "Age at first timepoint",Age.at.last.timepoint ~ "Age at last timepoint",Number.of.Timepoints = "# of timepoints",sex = "Sex")) 

ind_demo %>% 
  select(sex,race,ethnic) %>% 
  tbl_summary(label = list(sex ~ "Sex",race ~ "Race",ethnic ~ "Ethnicity")) 

# Time between scans plots

max_timepoint <- max(demo$timepoint)
Time_between_scans_df <- data.frame("bblid" = unique(demo$bblid))
for(scan_num in 1:(max_timepoint-1)){
Timepoint.df <- demo %>% 
    filter(timepoint == scan_num | timepoint == scan_num + 1) %>% 
    group_by(bblid) %>% 
    arrange(timepoint) %>% 
    summarize(time_between_scans = age[2] - age[1]) %>% 
    ungroup()
colnames(Timepoint.df) <- ifelse(colnames(Timepoint.df) == "time_between_scans",paste0("time_between_scans",scan_num),colnames(Timepoint.df))
Time_between_scans_df <- Time_between_scans_df %>% 
    left_join(Timepoint.df)
}

Time_between_scans_long <- Time_between_scans_df %>% 
  pivot_longer(cols = time_between_scans1:last_col(),names_to = "timepoint",values_to = "Years_until_next_scan") %>% 
  mutate(timepoint = str_replace_all(timepoint,pattern = "time_between_scans",replacement = "")) %>% 
  mutate(timepoint = as.numeric(timepoint)) %>% 
  left_join(demo[,c("bblid","timepoint","age")])

# Years to next scan 
age_Longitudinal <- Time_between_scans_long %>% 
  ggplot(aes(x = age,y = Years_until_next_scan)) + geom_point(size = .8,color = "royalblue") + geom_smooth(color = "gray50") + labs(x = "Age",y = "Years until next scan") + theme_minimal()
  
Time_between_scans_N <- Time_between_scans_long %>% 
  group_by(timepoint) %>% 
  summarize(N = sum(!is.na(Years_until_next_scan))) %>% 
  mutate(timepoint = case_when(timepoint == 1 ~ "1-2",timepoint == 2 ~ "2-3",timepoint == 3 ~ "3-4",timepoint == 4 ~ "4-5",timepoint == 5 ~ "5-6",timepoint == 6 ~ "6-7")) 

timepoint_Longitudinal <- Time_between_scans_long %>% 
  filter(!is.na(Years_until_next_scan) & !is.na(age)) %>% 
  mutate(timepoint = case_when(timepoint == 1 ~ "1-2",timepoint == 2 ~ "2-3",timepoint == 3 ~ "3-4",timepoint == 4 ~ "4-5",timepoint == 5 ~ "5-6",timepoint == 6 ~ "6-7")) %>% 
  ggplot(aes(x = factor(timepoint),y = Years_until_next_scan,color = factor(timepoint),fill = factor(timepoint))) + geom_point(size = 1.3,alpha = .4,position = position_jitter(seed = 1,width = .1)) + 
  geom_boxplot(alpha = .1,width = .3,outlier.shape = NA) + labs(x = "Timepoints",y = "Years between scans") + 
  scale_color_manual(values = c("#fdae6b","#fd8d3c","#f16913","#d94801","#a63603","#7f2704")) + scale_fill_manual(values = c("#fdae6b","#fd8d3c","#f16913","#d94801","#a63603","#7f2704")) + theme(legend.position = "none") + 
  ggdist::stat_halfeye(justification = 1.25,.width = 0,point_colour = NA,side = "left",adjust = .6) + geom_label(data = Time_between_scans_N,aes(x = factor(timepoint),y = 11.5,label = paste("N = ",N)),color = "black",size = 3,alpha = .5)
  
Time_between_scans_summary <- Time_between_scans_long %>% 
  filter(!is.na(Years_until_next_scan)) %>% 
  summarize(Avg = mean(Years_until_next_scan),SD = sd(Years_until_next_scan))

Time_Consecutive_scans <- Time_between_scans_long %>% 
  filter(!is.na(Years_until_next_scan)) %>% 
  ggplot(aes(x = Years_until_next_scan)) + geom_density(fill = "royalblue",color = "royalblue") + labs(x = "Years between consecutive scans") + geom_vline(aes(xintercept = mean(Years_until_next_scan))) + geom_label(x = 2.5,y = .42,label = paste("Mean:",round(Time_between_scans_summary$Avg,2))) + ylim(c(0,.45))

Time_total_scans <- Time_between_scans_long %>% 
  group_by(bblid) %>% 
  summarize(Span = max(age,na.rm = TRUE) - min(age,na.rm = TRUE)) %>% 
  ggplot(aes(x = Span)) + geom_density(fill = "firebrick",color = "firebrick") + geom_vline(aes(xintercept = mean(Span,na.rm = T))) + geom_label(aes(x = mean(Span,na.rm = T),y = .17,label = paste("Mean:",round(mean(Span,na.rm = T),2)))) + labs(x = "Years between first and last scan")

# Create additional plots for ventricle, CSF, and white matter

Vent_long_plot <- aseg %>% 
  left_join(demo) %>% 
  mutate(All_vent = rowSums(across(.cols = matches("-Ventricle$|-Vent$")))) %>% 
  mutate(All_vent_z = as.numeric(scale(All_vent))) %>% 
  ggplot(aes(x = age,y = All_vent_z,color = sex,group = bblid)) + geom_smooth(aes(x = age,y = All_vent_z,color = sex,group = sex)) + geom_point(size = .1) + geom_line(alpha = .25) + labs(x = "Age",y = "Ventricle Volume (Z-score)",title = "Ventricle Volume by Age and Sex") + theme_minimal() + scale_color_manual(values = c("#d7191c","#2b83ba")) 


CSF_long_plot <- aseg %>% 
  left_join(demo) %>% 
  mutate(CSF_z = as.numeric(scale(CSF))) %>% 
  ggplot(aes(x = age,y = CSF_z,color = sex,group = bblid)) + geom_smooth(aes(x = age,y = CSF_z,color = sex,group = sex)) + geom_point(size = .1) + geom_line(alpha = .25) + labs(x = "Age",y = "CSF Volume (Z-score)",title = "CSF Volume by Age and Sex") + theme_minimal() + scale_color_manual(values = c("#d7191c","#2b83ba")) 

WM_long_plot <- aseg %>% 
  left_join(demo) %>% 
  mutate(WM_z = as.numeric(scale(CerebralWhiteMatterVol))) %>% 
  ggplot(aes(x = age,y = WM_z,color = sex,group = bblid)) + geom_smooth(aes(x = age,y = WM_z,color = sex,group = sex)) + geom_point(size = .1) + geom_line(alpha = .25) + labs(x = "Age",y = "WM Volume (Z-score)",title = "White Matter by Age and Sex") + theme_minimal() + scale_color_manual(values = c("#d7191c","#2b83ba")) 



# Info for presentations

ggarrange(Vent_long_plot,CSF_long_plot,WM_long_plot,common.legend = T,labels = c("A","B","C"),legend = "bottom")

ggarrange(Longitudinal_study,Longitudinal_Scanner,labels = c("A","B"))

time_between_scans <- ggarrange(Time_Consecutive_scans,Time_total_scans,age_Longitudinal,timepoint_Longitudinal,labels = c("A","B","C","D"))




#annotate_figure(time_between_scans,top = text_grob(label = "Time Between Scans \n",face = "bold",size = 36))

# Plots by Lobe
FL <- ggarrange(BrainMeasures.LongitudinalPlots[[9]] + labs(title = ""),BrainMeasures.LongitudinalPlots[[5]] + labs(title = ""),BrainMeasures.LongitudinalPlots[[1]] + labs(title = ""),labels = c("A","B","C"),common.legend = T,legend = "right")
OL <- ggarrange(BrainMeasures.LongitudinalPlots[[10]] + labs(title = ""),BrainMeasures.LongitudinalPlots[[6]] + labs(title = ""),BrainMeasures.LongitudinalPlots[[2]] + labs(title = ""),labels = c("A","B","C"),common.legend = T,legend = "right")
PL <- ggarrange(BrainMeasures.LongitudinalPlots[[11]] + labs(title = ""),BrainMeasures.LongitudinalPlots[[7]] + labs(title = ""),BrainMeasures.LongitudinalPlots[[3]] + labs(title = ""),labels = c("A","B","C"),common.legend = T,legend = "right")
TL <- ggarrange(BrainMeasures.LongitudinalPlots[[12]] + labs(title = ""),BrainMeasures.LongitudinalPlots[[8]] + labs(title = ""),BrainMeasures.LongitudinalPlots[[4]] + labs(title = ""),labels = c("A","B","C"),common.legend = T,legend = "right")


pdf(file = "/Users/hillmann/Projects/ExtraLong/Plots:Results/FreesurferLongitudinal2021CTvolSA_byAge+Sex",width = 14,height = 8)

BrainMeasures.LongitudinalPlots[[1]]
BrainMeasures.LongitudinalPlots[[2]]
BrainMeasures.LongitudinalPlots[[3]]
BrainMeasures.LongitudinalPlots[[4]]
BrainMeasures.LongitudinalPlots[[5]]
BrainMeasures.LongitudinalPlots[[6]]
BrainMeasures.LongitudinalPlots[[7]]
BrainMeasures.LongitudinalPlots[[8]]
BrainMeasures.LongitudinalPlots[[9]]
BrainMeasures.LongitudinalPlots[[10]]
BrainMeasures.LongitudinalPlots[[11]]
BrainMeasures.LongitudinalPlots[[12]]

dev.off()




