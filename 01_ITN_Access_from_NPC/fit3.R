library(haven) # for reading dta file
library(tidyverse)
library(cowplot)

setwd("/Users/hannahkoenker/Dropbox/R Directory/TZ_access/01_ITN_Access_from_NPC")

Ddta <- read_dta("regional_ITN_indicators_access.dta")
D <- with(Ddta, data.frame(y = access_merg, x = itnpers, z = meanhh))

A <- as.matrix(D)
B <- as.data.frame(A)

# First plot the dots:

pdf(
  "Access_NPC.pdf",
  height = 6,
  width = 6,
  )
with(B,
     plot(
       x,
       y,
       cex = .5,
       col = "grey",
       ylim = c(0, 105),
       xlab = "ITNs per capita",
       ylab = "Population ITN Access (percent)")
     )
# title(main = "Title")

taus <- 1:3/4
library(quantreg)
library(splines)
xx <- 1:99/100
colors <- c(2,1,2)

G <- matrix(0,99,length(taus))
for(i in 1:length(taus)){
    f <- rqss(y ~ qss(x, constraint = "I", lambda = .5), tau = taus[i], data = B)
    G[,i] <- predict(f, newdata = list(x = xx))
    lines(xx,G[,i], col = colors[i], lwd = 2)
}
dev.off()

### Make G into a dataframe and rename the variables for ggplot later: 
gg <- as.data.frame(G) %>% 
  mutate(access=V2,
         lb=V1,
         ub=V3) %>% 
  arrange(access) %>% 
  mutate(npc=1:n()/100) # make a new variable which is the row count and also NPC because Dad did not include this!

## try to load in the PDF as a ggplot object. Does not work when you combine with cowplot.
# npc <- ggplot() +
#   annotation_custom(grid::rasterGrob("Access_NPC.pdf", 
#                                      width=unit(1,"npc"),
#                                      height=unit(1,"npc")),
#                     -Inf, Inf, -Inf, Inf)


### Graphing Stata Decay Curves in GGPLOT ####

tz <- read_dta("/Users/hannahkoenker/Dropbox/A DHS MIS Datasets/Analysis/Decay Curves/tz_decay_only.dta")
curves <- read_dta("/Users/hannahkoenker/Dropbox/A DHS MIS Datasets/Analysis/Decay Curves/curves_only.dta") %>% 
  filter(lifespan<5.5)

decay <- ggplot() +
  geom_line(
    data = curves,
    aes(
      x = tdist,
      y = percsurv,
      color = as.factor(lifespan),
      linetype = as.factor(lifespan)
    ),
    alpha = 0.4
  ) +
  # geom_ribbon(
  #   data = tz,
  #   aes(x = tdist, ymin = lower, ymax = upper, fill="95% CI"),
  #   # fill = "lightgrey",
  #   alpha = 0.7
  # ) +
  # geom_line(data = tz, aes(x = tdist, y = percsurv), color = "orange") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank()
  ) +
  # scale_fill_manual(name="Tanzania \n estimate", values="lightgrey") +
  scale_color_manual(
    name = "Median \n Lifespan",
    values = c(
      "dodgerblue",
      "dodgerblue",
      "red",
      "red",
      "seagreen",
      "seagreen",
      "darkorchid",
      "darkorchid",
      "pink"
    )
  ) +
  scale_linetype_manual(name = "Median \n Lifespan", values = c(1, 2, 1, 2, 1, 2, 1, 2, 1)) +
  scale_x_continuous(breaks=seq(0,9,1)) +
  labs(
    y = "Percent of nets surviving",
    x = "Years since distribution",
    title = "Net decay curves for \n varying median lifespans",
    color = "",
    fill=""
  )

### HK Try again recreate plot
## load in points from the regional data
## load in grid from what Dad sent and plot as geom_lines 

npc <- ggplot() +
  geom_point(data=B, aes(x,y), alpha=0.3, shape=1, size=0.5) +
  geom_line(data=gg, aes(npc, access)) +
  geom_line(data=gg, aes(npc, lb, color="50% confidence bounds")) +
  geom_line(data=gg, aes(npc, ub, color="50% confidence bounds")) +
  theme_minimal() +
  theme(legend.position="bottom") +
  labs(x="ITNs per capita (NPC)",
       y="Population ITN Access (percent)",
       color="",
       title="ITN Access as a function \n of nets per capita")


plot_grid(decay, npc, labels="AUTO", align="h", axis="bt")
ggsave("decay_npc_notz.png", width=8, height=5, units="in")
