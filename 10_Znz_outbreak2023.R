

library(tidyverse)
library(haven)
library(janitor)
library(sf)
library(ggrepel)
library(readxl)
library(stringr)
library(patchwork)

z <- st_read("/Users/hannahkoenker/Dropbox/R Directory/TZ_access/Unguja_Shehia_Corrected_Update")

z <- z %>% 
  mutate(outbreak=if_else(Ward_Name %in% c("Paje","Kizimkazi Mkunguni", "Kivunge", "Daraja Bovu", "Kwamtipura", "Tomondo", "Chumbuni", "Fuoni Kibondeni", "Mbweni", "Chukwani"), "outbreak", NA_character_))

shlist <- read_excel("znz_shehias.xlsx")
sh52 <- read_excel("/Users/hannahkoenker/Dropbox/A DHS MIS Datasets/Analysis/TVCA/Access Estimates/data/ZNZ Final LLIN quantification for 2020 mini Mass distribution.xlsx", sheet="Sheet1") %>% 
  mutate(campaign2023=1) %>% 
  mutate(Ward_Name=str_to_title(Ward_Name))

sh <- shlist %>% 
  full_join(sh52)


z <- z %>% 
  left_join(sh)

z %>% 
ggplot() +
  geom_sf(aes(fill=as.factor(outbreak)), color="darkgrey") +
  theme_void() +
  scale_fill_hue(na.value="whitesmoke", labels=c("outbreak shehia","NA")) +
  # geom_sf_text(data=filter(z, outbreak==1), aes(label=Ward_Name))
  geom_label_repel(data=filter(z, outbreak=="outbreak"), aes(label=Ward_Name, geometry=geometry), fill="whitesmoke", label.padding=0, label.size = NA, color="black", stat="sf_coordinates", force=8, box.padding=.75) +
  labs(fill="")


z %>% 
  filter(district=="Mjini") %>% 
  ggplot() +
  geom_sf() +
  geom_sf_text(aes(label=Ward_Name), size=2)

z %>% 
  ggplot() +
  geom_sf(aes(fill=as.factor(campaign2023)), color="darkgrey") +
  theme_void() +
  scale_fill_hue(na.value="whitesmoke", labels=c("in 2020, 2023","in 2021, 2024")) + #, na.translate = FALSE
  labs(fill="shehias receiving mass campaign") +
  theme(legend.position="left")


