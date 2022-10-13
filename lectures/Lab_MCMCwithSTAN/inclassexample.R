load("mystanmodels.stanmodel")

# compiling (SLOW)
hockeystick_stan <- stan_model("stanmodels/hockeystick.stan")

mystations <- c(456624, 426357, 354147, 456974)
lilacs <- lp %>% subset(STATION %in% mystations)

l1 <- subset(lilacs, STATION == 456624)
l1.data <- list(n = nrow(l1), x = l1$YEAR, y = l1$BLOOM)

# running the MCMC
lilac_hockeystick.fit <- sampling(hockeystick_stan, 
                                  l1.data, cores = 4)
lilac_hockeystick.fit

# Diagnostic plots using the MCMC.list object of coda package
hs_chains <- As.mcmc.list(lilac_hockeystick.fit, 
                          pars = c("beta0","beta1","tau", "sigma"))
plot(hs_chains)

ggplot(lilacs, aes(YEAR, BLOOM, col = STATION)) + 
    geom_point() + geom_smooth() + facet_wrap(.~STATION)




hockeystick.chains <- as.array(hs_chains)
years <- 1950:2000

getX.hat <- function(T, beta0, beta1, tau)
    ifelse(T < tau, beta0, beta0 + beta1 * (T-tau))


plot(BLOOM ~ YEAR, pch=19, col=rgb(0,0,0,.5), cex=2, data = l1)
(p.hat <- summary(hs_chains)$quantiles[,"50%"])
lines(1950:2000, getX.hat(1950:2000, p.hat['beta0'], p.hat['beta1'], p.hat['tau']), lwd=2, col="darkred")


hockey.sims <- matrix(0, nrow=length(years), ncol=1e4)
for(i in 1:1e4)
{
    b0 <- sample(hockeystick.chains[,"beta0",], 1)
    b1 <- sample(hockeystick.chains[,"beta1",], 1)
    tau <- sample(hockeystick.chains[,"tau",], 1)
    hockey.sims[,i] <- getX.hat(years, b0, b1, tau)
}

CIs <- apply(hockey.sims, 1, quantile, p=c(0.025, 0.5, 0.975))

plot(BLOOM ~ YEAR, pch=19, col=rgb(0,0,0,.5), cex=2, data = l1)
lines(years, CIs[1,], col="red", lwd=2, lty=3)
lines(years, CIs[2,], col="red", lwd=2)
lines(years, CIs[3,], col="red", lwd=2, lty=3)







