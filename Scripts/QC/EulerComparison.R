# Read in data and packages. Comparing Free QC from 2019 data freeze, Free QC from 2021 data freeze, and QC from separate script on 2019 data freeze
library(mosaic)
library(tidyverse)
library(lme4)
library(gridExtra)

quality2021 <- read_csv("~/Projects/ExtraLong/Data/quality_2021-11-01.csv") %>% 
  rename(scanid = seslabel)
quality2019 <- read_table2("~/Projects/ExtraLong/Data/AllSessions_quality_cross.csv")
FreeQC2019 <- read_csv("~/Projects/ExtraLong/Data/quality_2020-11-09.csv") %>% 
  mutate(seslabel = paste0("ses-",seslabel)) %>% 
  rename(Session = seslabel)
demo <- read_csv("/Users/hillmann/Projects/ExtraLong/Data/scanid_to_seslabel_demo_20200531.csv") %>% 
  mutate(seslabel = paste0("ses-",seslabel)) %>% 
  rename(Session = seslabel)

# Some studies were mislabelled in the demo file, the following code fixes that
MislabeledSubjects <- demo %>% 
  group_by(bblid,Session) %>% 
  summarize(n = n()) %>% 
  filter(n > 1) %>% 
  ungroup() %>% 
  pull(bblid)

correctSessionLabel <- function(df){
  subjID <- df %>% 
    slice(1) %>% 
    pull(bblid)
  if(subjID %in% MislabeledSubjects){
    mislabeledSession <- df %>% 
      filter(Session == "ses-CONTE1") %>% 
      arrange(timepoint) %>% 
      mutate(Session = ifelse(timepoint == max(timepoint),"ses-CONTE2","ses-CONTE1"))
    
    other.df <- df %>% 
      filter(Session != "ses-CONTE1")
    
    df <- rbind(mislabeledSession,other.df)
  }
  return(df)
}

demo.ID <- demo %>% 
  group_split(bblid)
demo <- map_dfr(demo.ID,correctSessionLabel)

Vol2019_lh <- read_csv("~/Projects/ExtraLong/Data/aparc_volume_lh_2020-11-09.csv") 
Vol2019_rh <- read_csv("~/Projects/ExtraLong/Data/aparc_volume_rh_2020-11-09.csv") 
Vol2021_lh <- read_csv("~/Projects/ExtraLong/Data/aparc_volume_lh_2021-11-01.csv") 
Vol2021_rh <- read_csv("~/Projects/ExtraLong/Data/aparc_volume_rh_2021-11-01.csv") 

Vol.2019 <- Vol2019_lh %>% 
  left_join(Vol2019_rh) %>% 
  mutate(seslabel = paste0("ses-",seslabel)) %>% 
  rename(Session = seslabel) %>% 
  left_join(demo[,c("bblid","Session","scanid")]) %>% 
  select(-Session) %>% 
  rename(seslabel = scanid) %>% 
  relocate(bblid,seslabel) %>% 
  rename_with(.fn = ~ paste0(.x,"_2019"),.cols = lh.aparc.volume:rh_insula_volume)

Vol.2021 <- Vol2021_lh %>% 
  left_join(Vol2021_rh) %>% 
  rename_with(.fn = ~ paste0(.x,"_2021"),.cols = lh.aparc.volume:rh_insula_volume)

Vol.all <- Vol.2019 %>% 
  left_join(Vol.2021)
  

# Combine all quality control data into one data set

qualityAll <- quality2019 %>% 
  rename(bblid = fsid) %>% 
  left_join(demo) %>% 
  select(bblid,Session,sex,scanage_years,scanid,euler_total) %>% 
  mutate(sex = ifelse(sex == 1,"Male","Female")) %>% 
  rename(age = scanage_years) %>% 
  rename(euler_Noah_2019 = euler_total) %>% 
  left_join(quality2021[,c("bblid","scanid","euler_total")]) %>% 
  rename(euler_FreeQC_2021 = euler_total) %>% 
  left_join(FreeQC2019[,c("bblid","Session","euler_total")]) %>% 
  rename(euler_FreeQC_2019 = euler_total) %>% 
  pivot_longer(cols = euler_Noah_2019:euler_FreeQC_2019,names_to = "Year",values_to = "euler_total") %>% 
  mutate(Year = str_remove_all(Year,pattern = "euler_"))  %>% 
  separate(Year,into = c("Method","Year"),sep = "_") %>% 
  rename(Freeze = Year) 

FreeQC.Time <- qualityAll %>% 
  drop_na() %>% 
  filter(Method == "FreeQC") %>% 
  group_by(bblid,Session,age) %>% 
  arrange(Freeze) %>% 
  summarize(Euler_diff = diff(euler_total)) %>% 
  ungroup() %>% 
  ggplot(aes(x = Euler_diff)) + geom_histogram(bins = 50,color = "peru",fill = "peru") + labs(x = "FreeQC Euler 2021 - FreeQC Euler 2019") + theme_minimal()

theme_set(theme_minimal())

QC.2019 <- qualityAll %>% 
  drop_na() %>% 
  filter(Freeze == "2019") %>% 
  group_by(bblid,Session,age) %>% 
  arrange(Method) %>% 
  summarize(Euler_diff = diff(euler_total)) %>% 
  ungroup() %>% 
  ggplot(aes(x = Euler_diff)) + geom_density(color = "peru",fill = "peru",alpha = .5) + labs(x = "Noah Euler 2019 - FreeQC Euler 2019")

qualityFreeQC <- qualityAll %>% 
  filter(Method == "FreeQC") 

euler.cor <- qualityFreeQC %>% 
  pivot_wider(names_from = Freeze,values_from = euler_total,names_prefix = "euler_total_") %>% 
  with(round(cor(euler_total_2019,euler_total_2021,use = "pairwise.complete.obs"),3))
  
ByFreesurferRun <- qualityFreeQC %>% 
  pivot_wider(names_from = Freeze,values_from = euler_total,names_prefix = "euler_total_") %>% 
  ggplot(aes(x = euler_total_2019,y = euler_total_2021)) + labs(x = "Euler 2019",y = "Euler 2021",title = "Euler Total by Freesurfer Run",caption = paste("r = ",euler.cor)) + geom_point(size = .75,color = "slategray") + geom_smooth(method = "lm",color = "black") + theme_minimal()

age_eulerDiff <- qualityFreeQC %>% 
  pivot_wider(names_from = Freeze,values_from = euler_total,names_prefix = "euler_total_") %>% 
  mutate(euler_diff = euler_total_2021 - euler_total_2019) %>% 
  filter(!is.na(euler_diff)) %>% 
  ggplot(aes(x = age,y=euler_diff)) + geom_point(size = .5,color = "steelblue1") + geom_smooth(color = "black") + theme_minimal() + labs(x = "Age",y = "Euler 2021 - Euler 2019",title = "Effect of Age on Euler Reliability")

eulerDiff_sex <- qualityFreeQC %>% 
  pivot_wider(names_from = Freeze,values_from = euler_total,names_prefix = "euler_total_") %>% 
  mutate(euler_diff = euler_total_2021 - euler_total_2019) %>%
  filter(!is.na(euler_diff)) %>% 
  ggplot(aes(x = euler_diff,color = sex,fill = sex)) + geom_density(alpha = .5) + theme_minimal() + scale_color_manual(values = c("#d7191c","#2b83ba")) + scale_fill_manual(values = c("#d7191c","#2b83ba")) + labs(x = "Euler 2021 - Euler 2019",title = "Effect of Sex on Euler Reliability") + xlim(c(0,500))

eulerDiff_ageSex <- qualityFreeQC %>% 
  filter(!is.na(sex)) %>% 
  pivot_wider(names_from = Freeze,values_from = euler_total,names_prefix = "euler_total_") %>% 
  mutate(euler_diff = euler_total_2021 - euler_total_2019) %>% 
  filter(!is.na(euler_diff)) %>% 
  ggplot(aes(x = age,y=euler_diff,color = sex)) + geom_point(size = .25) + geom_smooth() + theme_minimal() + labs(x = "Age",y = "Euler 2021 - Euler 2019",title = "Effect of Age and Sex on Euler Reliability") + scale_color_manual(values = c("#d7191c","#2b83ba"))

Quality_eulerDiff <- qualityFreeQC %>% 
  pivot_wider(names_from = Freeze,values_from = euler_total,names_prefix = "euler_total_") %>% 
  mutate(euler_diff = euler_total_2021 - euler_total_2019) %>% 
  ggplot(aes(x = euler_total_2019,y=euler_diff)) + geom_point(size = .75,color = "sienna1") + geom_smooth(color = "black") + theme_minimal() + labs(x = "Euler 2019",y = "Euler Increase",title = "Effect of Scan Quality on Euler Reliability") 

qualityFreeQC.Mod <- qualityFreeQC %>% 
  pivot_wider(names_from = Freeze,values_from = euler_total,names_prefix = "euler_total_") %>% 
  mutate(euler_diff = euler_total_2021 - euler_total_2019)  

ageSex_Euler <- lmer(euler_diff ~ age + sex + age:sex + euler_total_2019 + (1|bblid),data = qualityFreeQC.Mod)  
summary(ageSex_Euler)

# Percent Increase as Response

age_eulerDiff_Percent <- qualityFreeQC %>% 
  pivot_wider(names_from = Freeze,values_from = euler_total,names_prefix = "euler_total_") %>% 
  mutate(euler_diff = (euler_total_2021 - euler_total_2019)/abs(euler_total_2019)*100) %>% 
  ggplot(aes(x = age,y=euler_diff)) + geom_point(size = .5) + geom_smooth() + theme_minimal() + labs(x = "Age",y = "Euler Increase (%)",title = "Effect of Age on Euler Reliability (%)")

eulerDiff_sex_Percent <- qualityFreeQC %>% 
  pivot_wider(names_from = Freeze,values_from = euler_total,names_prefix = "euler_total_") %>% 
  mutate(euler_diff = (euler_total_2021 - euler_total_2019)/abs(euler_total_2019)*100) %>%
  ggplot(aes(x = euler_diff,color = sex,fill = sex)) + geom_density(alpha = .5) + theme_minimal() + scale_color_manual(values = c("#d7191c","#2b83ba")) + scale_fill_manual(values = c("#d7191c","#2b83ba")) + labs(x = "Euler Increase (%)",title = "Effect of Sex on Euler Reliability (%)")

eulerDiff_ageSex_Percent <- qualityFreeQC %>% 
  filter(!is.na(sex)) %>% 
  pivot_wider(names_from = Freeze,values_from = euler_total,names_prefix = "euler_total_") %>% 
  mutate(euler_diff = (euler_total_2021 - euler_total_2019)/abs(euler_total_2019)*100) %>% 
  ggplot(aes(x = age,y=euler_diff,color = sex)) + geom_point(size = .25) + geom_smooth() + theme_minimal() + labs(x = "Age",y = "Euler Increase (%)",title = "Effect of Age and Sex on Euler Reliability (%)") + scale_color_manual(values = c("#d7191c","#2b83ba"))

Quality_eulerDiff_Percent <- qualityFreeQC %>% 
  pivot_wider(names_from = Freeze,values_from = euler_total,names_prefix = "euler_total_") %>% 
  mutate(euler_diff = (euler_total_2021 - euler_total_2019)/abs(euler_total_2019)*100) %>% 
  ggplot(aes(x = euler_total_2019,y=euler_diff)) + geom_point(size = .5,color = "palevioletred4") + geom_smooth(color = "black") + theme_minimal() + labs(x = "Euler 2019",y = "Euler Increase (%)",title = "Effect of Scan Quality on Euler Reliability (%)")

qualityFreeQC.Mod.Percent <- qualityFreeQC %>% 
  pivot_wider(names_from = Freeze,values_from = euler_total,names_prefix = "euler_total_") %>% 
  mutate(euler_diff = (euler_total_2021 - euler_total_2019)/abs(euler_total_2019)*100)  

ageSex_Euler_Percent <- lmer(euler_diff ~ age + sex + age:sex + euler_total_2019 + (1|bblid),data = qualityFreeQC.Mod.Percent)  
summary(ageSex_Euler_Percent)

qualityFreeQC %>% 
  pivot_wider(names_from = Freeze,values_from = euler_total,names_prefix = "euler_total_") %>% 
  mutate(euler_diff = euler_total_2021 - euler_total_2019) %>% 
  filter(!is.na(euler_total_2021),!is.na(euler_total_2019)) %>% 
  with(favstats(~euler_diff))

### CNR by Freesurfer Run

qualityFreeQC_CNR <- FreeQC2019 %>% 
  left_join(demo) %>% 
  select(bblid,Session,sex,scanage_years,scanid,contains("euler"),contains("cnr")) %>% 
  mutate(sex = ifelse(sex == 1,"Male","Female")) %>% 
  rename(age = scanage_years) %>% 
  rename_with(.cols = euler_lh:cnr_graywhite_rh,.fn = ~ paste0(.x,"_2019")) %>% 
  left_join(quality2021,by = c("bblid","scanid")) %>% 
  rename_with(.cols = cnr_graycsf_lh:euler_total,.fn = ~ paste0(.x,"_2021")) %>% 
  select(!contains("holes"))

QC.cor <- qualityFreeQC_CNR %>% 
  pivot_longer(euler_lh_2019:euler_total_2021,names_to = c("QC.Measure","Year"),names_sep = "(?=[[:digit:]])(?<![[:digit:]])",values_to = "QC.value") %>% 
  mutate(QC.Measure = str_remove(QC.Measure,pattern = "_$")) %>% 
  relocate(Year,QC.Measure,QC.value) %>% 
  pivot_wider(names_from = Year,values_from = QC.value) %>% 
  group_by(QC.Measure) %>% 
  summarize(Cor = cor(`2019`,`2021`,use = "pairwise.complete.obs")) %>% 
  ungroup()
 
labels.QC <- paste0(QC.cor$QC.Measure,": r = ",round(QC.cor$Cor,3)) 
names(labels.QC) <- QC.cor$QC.Measure

CNREuler_byRun <- qualityFreeQC_CNR %>% 
  pivot_longer(euler_lh_2019:euler_total_2021,names_to = c("QC.Measure","Year"),names_sep = "(?=[[:digit:]])(?<![[:digit:]])",values_to = "QC.value") %>% 
  mutate(QC.Measure = str_remove(QC.Measure,pattern = "_$")) %>% 
  relocate(Year,QC.Measure,QC.value) %>% 
  pivot_wider(names_from = Year,values_from = QC.value) %>% 
  filter(!is.na(`2019`),!is.na(`2021`)) %>% 
  ggplot(aes(x = `2019`,y = `2021`)) + geom_point(size = .5) + geom_smooth(method = "lm") + facet_wrap(~QC.Measure,scales = "free",labeller = labeller(QC.Measure = labels.QC)) + labs(x = "2019",y = "2021") + theme_minimal() 

EulerChange_CNR <- qualityFreeQC_CNR %>% 
  mutate(euler_diff = euler_total_2021 - euler_total_2019) %>% 
  pivot_longer(cols = contains("cnr"),names_to = "Measure_year",values_to = "QC.value") %>% 
  filter(str_detect(Measure_year,pattern = "2019")) %>% 
  relocate(Measure_year,QC.value,euler_diff) %>% 
  ggplot(aes(x = QC.value,y = euler_diff)) + facet_wrap(~Measure_year) + geom_point(size = .5) + geom_smooth() + theme_minimal() + labs(x = "CNR 2019",y = "Euler Increase")


EulerChange_CNR_Percent <- qualityFreeQC_CNR %>% 
  mutate(euler_diff = (euler_total_2021 - euler_total_2019)/abs(euler_total_2019)*100) %>% 
  pivot_longer(cols = contains("cnr"),names_to = "Measure_year",values_to = "QC.value") %>% 
  filter(str_detect(Measure_year,pattern = "2019")) %>% 
  relocate(Measure_year,QC.value,euler_diff) %>% 
  ggplot(aes(x = QC.value,y = euler_diff)) + facet_wrap(~Measure_year) + geom_point(size = .5) + geom_smooth() + theme_minimal() + labs(x = "CNR 2019",y = "Euler Increase (%)")

### Comparing Volume between Freesurfer Runs
cor.eTIV <- round(with(Vol.all,cor(eTIV_2019,eTIV_2021,use = "pairwise.complete.obs")),3)
eTIV.ByRun <- Vol.all %>% 
  filter(!is.na(eTIV_2019),!is.na(eTIV_2021)) %>% 
  ggplot(aes(x = eTIV_2019,y = eTIV_2021)) + geom_point(size = .5,color = "tomato") + geom_smooth(method = "lm",color = "black") + theme_minimal() + labs(x = "eTIV 2019",y = "eTIV 2021",title = "Effect of Freesurfer Run on Volume",caption = paste0("r = ",cor.eTIV))

cor.FP.lh <- round(with(Vol.all,cor(lh_frontalpole_volume_2019,lh_frontalpole_volume_2021,use = "pairwise.complete.obs")),3)
FP.lh.ByRun <- Vol.all %>% 
  filter(!is.na(lh_frontalpole_volume_2019),!is.na(lh_frontalpole_volume_2021)) %>% 
  ggplot(aes(x = lh_frontalpole_volume_2019,y = lh_frontalpole_volume_2021)) + geom_point(size = .5,color = "lightblue") + geom_smooth(method = "lm",color = "black") + theme_minimal() + labs(x = "Volume 2019",y = "Volume 2021",title = "Effect of Freesurfer Run \n on Frontal Pole Volume (lh)",caption = paste0("r = ",cor.FP.lh))
 
cor.FP.rh <- round(with(Vol.all,cor(rh_frontalpole_volume_2019,rh_frontalpole_volume_2021,use = "pairwise.complete.obs")),3)
FP.rh.ByRun <- Vol.all %>% 
  filter(!is.na(rh_frontalpole_volume_2019),!is.na(rh_frontalpole_volume_2021)) %>% 
  ggplot(aes(x = rh_frontalpole_volume_2019,y = rh_frontalpole_volume_2021)) + geom_point(size = .5,color = "lightblue4") + geom_smooth(method = "lm",color = "black") + theme_minimal() + labs(x = "Volume 2019",y = "Volume 2021",title = "Effect of Freesurfer Run \n on Frontal Pole Volume (rh)",caption = paste0("r = ",cor.FP.rh))

cor.TMP.lh <- round(with(Vol.all,cor(lh_inferiortemporal_volume_2019,lh_inferiortemporal_volume_2021,use = "pairwise.complete.obs")),3)
TMP.lh.ByRun <- Vol.all %>% 
  filter(!is.na(lh_inferiortemporal_volume_2019),!is.na(lh_inferiortemporal_volume_2021)) %>% 
  ggplot(aes(x = lh_inferiortemporal_volume_2019,y = lh_inferiortemporal_volume_2021)) + geom_point(size = .5,color = "lavender") + geom_smooth(method = "lm",color = "black") + theme_minimal() + labs(x = "Volume 2019",y = "Volume 2021",title = "Effect of Freesurfer Run \n on Inferior Temporal Volume (lh)",caption = paste0("r = ",cor.TMP.lh))

cor.TMP.rh <- round(with(Vol.all,cor(rh_inferiortemporal_volume_2019,rh_inferiortemporal_volume_2021,use = "pairwise.complete.obs")),3)
TMP.rh.ByRun <- Vol.all %>% 
  filter(!is.na(lh_inferiortemporal_volume_2019),!is.na(rh_inferiortemporal_volume_2021)) %>% 
  ggplot(aes(x = rh_inferiortemporal_volume_2019,y = rh_inferiortemporal_volume_2021)) + geom_point(size = .5,color = "lavenderblush4") + geom_smooth(method = "lm",color = "black") + theme_minimal() + labs(x = "Volume 2019",y = "Volume 2021",title = "Effect of Freesurfer Run \n on Inferior Temporal Volume (rh)",caption = paste0("r = ",cor.TMP.rh))

grid.arrange(FP.lh.ByRun,FP.rh.ByRun,TMP.lh.ByRun,TMP.rh.ByRun)
