rm(list=ls())
load("code/Elie/results/kappabeta.fit.rda")
require(ggplot2)

p1 <- kappabeta.fit %>% ggplot(aes(kappa, kappa.fit, ymin = kappa.CI.low, ymax = kappa.CI.high)) + 
  geom_abline(slope = 1, intercept = 0, col = "darkgrey") + geom_point() + geom_linerange() + ggthemes::theme_few() + ylim(c(0,2)) + 
  ggtitle(expression("Permeability estimates"~kappa)) + 
  ylab("estimate") + xlab(expression(kappa~"true value"))  + 
  facet_grid(.~beta, labeller = labeller(.cols = label_both))

p2 <- kappabeta.fit %>% ggplot(aes(beta, beta.fit, ymin = beta.CI.low, ymax = beta.CI.high)) + 
  geom_abline(slope = 1, intercept = 0, col = "darkgrey") + geom_point() + geom_linerange() + ggthemes::theme_few() + 
  ggtitle(expression("Resource selection estimates"~beta)) + 
  ylab("estimate") + xlab(expression(beta~"true value")) + 
  facet_grid(~kappa, labeller = labeller(.cols = label_both)) 

png("presentation/plots/KappaBetaEstimates.png", width = 1600, height = 1200, res = 200)
gridExtra::grid.arrange(p1,p2, ncol = 1)
dev.off()

# consequences of permeability

load("code/Elie/results/BCRWwithBarrier.rda")

kappa.names <- names(Z.sims)
tmax <- ncol(Z.sims[[1]])
tmax <- 130

png("presentation/plots/Consequences1.png", 
    width = 1600, height = 800, res = 200)
par(mfrow = c(1,4), mar = c(1,1,3,1), tck = 0.01, cex.main = 2)

for(k in as.character(c(0, 0.025, 0.1, 1))){
  image(r, main = bquote(kappa == .(k)), 
        col = topo.colors(100))
  apply(Z.sims[[k]][,1:tmax], 1, lines, col = scales::alpha(1, .2) )
  points(Z.sims[[k]][,1], pch = 19, cex = 0.5, col = "green")
  points(Z.sims[[k]][,tmax], pch = 19, cex = 0.5, col = "red")
  lines(barrier.short, lwd = 2, col = "white")
}
dev.off()
