library(haven) # for reading dta file
library(tidyverse)

Ddta <- read_dta("regional_ITN_indicators_access.dta")
D <- with(Ddta, data.frame(y = access_merg, x = itnpers, z = meanhh))

A <- as.matrix(D)
B <- as.data.frame(A)

# First plot the dots:

pdf(
  "Access_NPC.pdf",
  height = 6,
  width = 8,
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
       
# this works but we don't need it

# Hannah tries to reproduce in ggplot:

# B %>% 
#   ggplot(aes(x, y),
#              size = 2,
#              color = "grey") +
#   geom_point() +
#   theme_minimal() 
#   
# ggsave("HK_plot.png")

# Then add the fit and the bound lines

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

