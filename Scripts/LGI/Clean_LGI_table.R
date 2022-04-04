#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

# Read in data and add column names
library(tidyverse)
input_file <- args[1]
output_file <- args[2]
df <- read.table(input_file, quote="\"")
colnames(df) <- c("Index","SegId","NVertices","Area_mm2","StructName","Mean","Stddev","Min","Max","Range")

# Add bblid and session label
file_name <- str_remove_all(input_file,pattern = ".*\\/")
subj <- str_replace_all(file_name,pattern = "_.*","")
bblid <- str_remove_all(subj,pattern = "sub-")
session <- str_extract(file_name,pattern = "ses.*")
session_num <- str_extract(session,pattern = "[0-9]+")
df$bblid <- as.character(bblid)
df$sesid <- as.character(session_num)

df <- df %>% 
  select(bblid,sesid,StructName,Area_mm2,Mean) %>% 
  pivot_wider(names_from = StructName,values_from = c(Area_mm2,Mean)) 

write.csv(df,file = output_file)
