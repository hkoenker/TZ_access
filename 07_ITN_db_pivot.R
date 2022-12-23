## Pivot Longer Script for TZ ITN Database 

library(tidyverse)
library(janitor)
library(haven)
library(writexl) 

df <- read_dta("/Users/hannahkoenker/Dropbox/A DHS MIS Datasets/Analysis/TVCA/Access Estimates/output/itndb_forpivot.dta") %>% 
  clean_names() %>% 
  pivot_longer(starts_with("chan_"), names_to = "channel", values_to = "nets", names_prefix="chan_") %>% 
  filter(nets>0) %>% 
  arrange(year, region, council) %>% 
  mutate(nettype=case_when(grepl("pbo",  channel) ~ "PBO",
                        TRUE ~ "Std"),
         zone=case_when(zone==1 ~ "Mainland",
                        zone==2 ~ "Zanzibar"),
         strategy=case_when(grepl("snp", channel) ~ "SNP",
                            grepl("rch", channel) ~ "RCH",
                            grepl("anc", channel) ~ "ANC",
                            grepl("ivd", channel) ~ "IVD",
                            grepl("com", channel) ~ "Community",
                            grepl("tnvs", channel) ~ "TNVS",
                            grepl("cc", channel) ~ "Campaign",
                            grepl("rc", channel) ~ "Campaign",
                            TRUE ~ as.character(channel)),
         type=case_when(strategy=="SNP" | strategy=="Community" ~ "Continuous",
                        strategy=="Campaign" ~ "Campaign",
                        strategy=="RCH" | strategy=="ANC" | strategy=="TNVS" | strategy=="IVD" ~ "Routine"))

write_xlsx(df,"/Users/hannahkoenker/Dropbox/RMEL/Objective 1/NMCP MTR VECTOR CONTROL/LLIN Data/itn_database_2022_12_15.xlsx")
