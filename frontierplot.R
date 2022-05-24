##########
## Frontier Plot from TZ scenario data
## Hannah Koenker
## May 13 2022
##########

library(tidyverse)
library(haven)
library(ggrepel)

df <- read_dta("/Users/hannahkoenker/Dropbox/A DHS MIS Datasets/Analysis/TVCA/Access Estimates/output/Appends/allscenarios.dta") %>% 
  mutate(scentype=case_when(grepl("_cd_",scen) ~ "CD",
                            grepl("_ucc_",scen) ~ "UCC3",
                            grepl("_ucc5_",scen) ~ "UCC5",
                            grepl("_uccd_",scen) ~ "UCC+CD",
                            TRUE ~ as.character(scen)),
         optimal=case_when(grepl("_ucc5_ 7",scen) ~ 1,
                           grepl("_cd_15",scen) ~ 1,
                           grepl("_cd_22",scen) ~ 1,
                           grepl("_ucc_ 7",scen) ~ 1,
                           grepl("_uccd_ 5",scen) ~ 1,
                           TRUE ~ 0),
         label=case_when(grepl("_ucc5_ 7",scen) ~ "5yr UCC + RCH 7% - 35% access",
                         grepl("_cd_15",scen) ~ "CD 15% + RCH 7% - 80% access",
                         grepl("_cd_22",scen) ~ "CD 22% + RCH 7% - 90% access",
                         grepl("_ucc_ 7",scen) ~ "UCC + RCH 7% - 70% access",
                         grepl("_uccd_ 5",scen) ~ "UCC + RCH 7% + CD 5% - 80% access",
                           TRUE ~ ""))

df %>% 
  filter(grepl("Mainland", scen),
         totalnets>100) %>% 
ggplot() +
  geom_point(aes(y=totalnets, x=pyp, color=scentype, shape=scentype, alpha=optimal, size=optimal)) +
  # geom_text(aes(y=totalnets, x=pyp, label=label), size=3, hjust=1.5, vjust=.5, color="dimgrey") +
  ggrepel::geom_text_repel(aes(y=totalnets, x=pyp, label=label), size=3, vjust=0.5, hjust=0.5, box.padding = 1, color="dimgrey") +
  geom_errorbarh(aes(xmin=pyplb, xmax=pypub, y=totalnets, color=scentype, alpha=optimal, size=optimal)) +
  # scale_linetype_manual(values = c("TRUE" = "solid", "FALSE" = "dotted")) + # scale_size(range = c(1, 2))
  scale_size(range = c(.5, 1.0)) +
  scale_shape_manual(values=c(19, 18, 17, 15)) +
  theme_minimal() +
  scale_color_brewer(palette="Set2") +
  scale_alpha(range=c(0.4,1)) +
  # scale_colour_brewer(palette="Set2", labels = c("CD", "UCC+CD", "UCC3", "UCC5")) +
  # scale_shape_manual(labels = c("CD", "UCC+CD", "UCC3", "UCC5"), values=c(19, 20, 17, 18)) +
  labs(x="Person-years of ITN access",
       y = "Total ITNs distributed 2022-2030",
       shape = "Distribution Strategy", colour = "Distribution Strategy") +
  guides(alpha="none", size = "none")

ggsave("/Users/hannahkoenker/Dropbox/A DHS MIS Datasets/Analysis/TVCA/Access Estimates/figs/frontierplot_all", device = "png")
