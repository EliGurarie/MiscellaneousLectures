## ---- echo=FALSE, eval=FALSE----------------------------------------------------------------------
## require(knitr)
## purl("MCMCwithSTAN.Rmd")

## ----setup, include=FALSE-------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, cache=TRUE, message=FALSE, warning=FALSE)


## ---- message=FALSE, warning=FALSE----------------------------------------------------------------
packages <- c("ggplot2", "ggmap", "plyr", "magrittr", "rstan", "coda", "lattice", "mapview")
sapply(packages, require, char = TRUE)


## -------------------------------------------------------------------------------------------------
ll <- read.csv("data/LilacLocations.csv")
lp <- read.csv("data/LilacPhenology.csv")


## -------------------------------------------------------------------------------------------------
head(ll)

## -------------------------------------------------------------------------------------------------
head(lp)


## ----GGmapping, message=FALSE, eval=TRUE, cache=TRUE----------------------------------------------
require(mapview); require(sf)
ll.sf <- st_as_sf(ll, coords = c("Long","Lat"), crs = st_crs(4326))
mapview(ll.sf)

#basemap1 <- get_map(location = c(-112, 38), maptype = "terrain", zoom=4)
#ggmap(basemap1) + geom_point(data = ll, mapping = aes(x = Long, y = Lat, colour = Elev, size = Nobs))


## ----SomeStations---------------------------------------------------------------------------------
Stations <- c(456624, 426357, 354147, 456974)
subset(ll, ID %in% Stations)


## -------------------------------------------------------------------------------------------------
lilacs <- subset(lp, STATION %in% Stations)
lilacs <- lilacs[!is.na(lilacs$BLOOM),]


## ----GGplotLoess, fig.height=4, fig.width=10------------------------------------------------------
require(ggplot2) 
ggplot(lilacs, aes(x=YEAR, y=BLOOM)) + 
  geom_point() + 
  facet_grid(.~STATION) + 
  stat_smooth()


## ----BaseLoess, echo=-1---------------------------------------------------------------------------
par(mfrow=c(2,2), bty="l")
for(s in Stations)
{
  years <- 1955:2000
  mydata <- subset(lilacs, STATION==s)
  with(mydata, plot(YEAR, BLOOM, main=s, xlim=range(years)))
  myloess <- loess(BLOOM~YEAR, data=mydata)
  myloess.predict <- predict(myloess, data.frame(YEAR = years), se=TRUE)
  with(myloess.predict, lines(years, fit)) 
  with(myloess.predict, lines(years, fit)) 
  with(myloess.predict, lines(years, fit - 2*se.fit, col="grey")) 
  with(myloess.predict, lines(years, fit + 2*se.fit, col="grey")) 
}


## ----SomeStationGGmap, eval=TRUE------------------------------------------------------------------
ll.mystations <- (ll.sf %>% subset(ID %in% Stations))[,"STATION"]
mapview(ll.mystations, map.types = "Esri.WorldPhysical")


## ----BrokenStick----------------------------------------------------------------------------------
getX.hat <- function(T, beta0, beta1, tau)
  ifelse(T < tau, beta0, beta0 + beta1 * (T-tau))


## ----BrokenStickIllustration, echo = -1-----------------------------------------------------------
par(bty="l")
l1 <- subset(lilacs, STATION == Stations[1])
p <- c(120, -2, 1980, 1)
plot(BLOOM~YEAR, pch = 19, col="grey", data = l1)
lines(l1$YEAR, getX.hat(l1$YEAR, beta0=120, beta1 = -2, tau = 1980), col=2, lwd=2)


## data {

##   int<lower=0> n; //

##   vector[n] y; // Y Vector

##   vector[n] x; // X Vector

## }

## 

## parameters {

##   real beta0;  					// intercept

##   real beta1;  					// slope

##   real<lower=0> sigma;	// standard deviation

## }

## 

## model {

##   //Declare variable

##   vector[n] yhat;

## 

##   //Priors

##   sigma ~ inv_gamma(0.001, 0.001);

##   beta0 ~ normal(150,150);

##   beta1 ~ normal(0, 10);

## 

##   //model

##   for(i in 1:n){

##    yhat[i] = beta0 + beta1 * (x[i] - mean(x));}

##   y ~ normal(yhat, sigma);

## }


## ---- eval=FALSE----------------------------------------------------------------------------------
## yhat <- beta0 + beta1 * (x - mean(x));


## ----loadlm.stan, eval=FALSE----------------------------------------------------------------------
## require(rstan)
## 

lm_stan <- stan_model(file = "lm.stan", auto_write = TRUE)


## ---- echo=FALSE, eval=2--------------------------------------------------------------------------
save(lm_stan, file="stanmodels/lm_stan.stanmodel")
load("stanmodels/lm_stan.stanmodel")


## ----lmSampling-----------------------------------------------------------------------------------
Data <- with(l1, list(n = nrow(l1), x = YEAR, y = BLOOM))
lm.fit <- sampling(lm_stan, data = Data)


## -------------------------------------------------------------------------------------------------
lm.fit


## ----lmSummary------------------------------------------------------------------------------------
summary(lm(BLOOM ~ I(YEAR-mean(YEAR)), data = l1))


## ----SD-------------------------------------------------------------------------------------------
summary(lm(BLOOM ~ I(YEAR-mean(YEAR)), data = l1))$sigma


## ----lmChains-------------------------------------------------------------------------------------
require(coda)
lm.chains <- As.mcmc.list(lm.fit, pars =  c("beta0", "beta1", "sigma"))
plot(lm.chains)


## ----lmDensityPlot, message=FALSE-----------------------------------------------------------------
require(lattice)
densityplot(lm.chains)


## data {

##   int<lower=0> n; //

##   vector[n] y; // Y Vector

##   vector[n] x; // X Vector

## }

## 

## parameters {

##   real beta0;  // intercept

##   real beta1;  // slope

##   real<lower=min(x), upper=max(x)> tau;    // changepoint

##   real<lower=0> sigma;   // variance

## }

## 

## model {

##     vector[n] yhat;

## 

##   // Priors

##     sigma ~ inv_gamma(0.001, 0.001);

##     beta0 ~ normal(150,150);

##     beta1 ~ normal(0, 10);

##     tau ~ uniform(min(x), max(x));

##   // Define the mean

##   for(i in 1:n)

##     yhat[i] = beta0 + int_step(x[i]-tau)*beta1*(x[i] - tau);

## 

##   // likelihood / probability model

##     y ~ normal(yhat, sigma);

## }


## ----HockeyStickCompiling, eval=FALSE, echo=1-----------------------------------------------------

eval <- FALSE
if(eval){
    hockeystick_stan <- stan_model(file = "stanmodels/hockeystick.stan")
    save(hockeystick_stan, file="stanmodels/hockeystick.stanmodel")
}

## ----HockeyStickSampling, echo=-1-----------------------------------------------------------------
load("stanmodels/hockeystick.stanmodel")
hockeystick.fit <- sampling(hockeystick_stan, Data)


## ----HockeyStickFit-------------------------------------------------------------------------------
hockeystick.fit


## ----HockeyStickResults---------------------------------------------------------------------------
require(lattice)
hockeystick.mcmc <- As.mcmc.list(hockeystick.fit, c("beta0", "beta1","tau","sigma"))
xyplot(hockeystick.mcmc)
densityplot(hockeystick.mcmc)


## ----VisualizeResults, echo=-1--------------------------------------------------------------------
par(bty="l")
getX.hat <- function(T, beta0, beta1, tau)
  ifelse(T < tau, beta0, beta0 + beta1 * (T-tau))

plot(BLOOM ~ YEAR, pch=19, col=rgb(0,0,0,.5), cex=2, data = l1)
(p.hat <- summary(hockeystick.mcmc)$quantiles[,"50%"])
lines(1950:2000, getX.hat(1950:2000, p.hat['beta0'], p.hat['beta1'], p.hat['tau']), lwd=2, col="darkred")


## ----MonteCarloFits-------------------------------------------------------------------------------
par(bty="l")

hockeystick.chains <- as.array(hockeystick.mcmc)
years <- 1950:2000
hockey.sims <- matrix(0, nrow=length(years), ncol=1e4)
for(i in 1:1e4){
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


## -------------------------------------------------------------------------------------------------
lilacs.nls <- nls( BLOOM ~ getX.hat(YEAR, beta0, beta1, tau), 
                   start=list(beta0=120, beta1=-2,tau=1980), 
                   data = l1)
summary(lilacs.nls)


## -------------------------------------------------------------------------------------------------
hockeystick.ll <- function(par, time, x){
	x.hat <- getX.hat(time, par["beta0"], par["beta1"], par["tau"])
	residual <- x - x.hat
	-sum(dnorm(residual, 0, par["sigma"], log=TRUE))
}

par0 <- c(beta0 = 120, beta1 = -2, tau=1980, sigma = 7)
fit.ll <- optim(par0, hockeystick.ll, time = l1$YEAR, x = l1$BLOOM)
fit.ll$par


## -------------------------------------------------------------------------------------------------
hockeystick.ll <- function(par, time, x){
	x.hat <- getX.hat(time, par["beta0"], par["beta1"], par["tau"])
	residual <- x - x.hat
	-sum(dnorm(residual, 0, exp(par["logsigma"]), log=TRUE))
}

par0 <- c(beta0 = 120, beta1 = -2, tau=1980, logsigma = 1)
fit.ll <- optim(par0, hockeystick.ll, time = l1$YEAR, x = l1$BLOOM)
fit.ll$par


## -------------------------------------------------------------------------------------------------
exp(fit.ll$par['logsigma'])


## data {

##   int<lower=0> n;                     // number of total observations

##   int<lower=1> nsites;                // number of sites

##   int<lower=1,upper=nsites> site[n];  // vector of site IDs

##   real y[n];                        // flowering date

##   real x[n];                        // year

## }

## 

## parameters {

##   real mu_beta0;  // mean intercept

##   real<lower=0> sd_beta0;  // s.d. of intercept

## 

##   real mu_beta1;  // mean slope

##   real<lower=0> sd_beta1;  // s.d. of intercept

## 

##   // overall changepoint and s.d.

##   real<lower = min(x), upper = max(x)> tau;

##   real<lower=0> sigma;

## 

##   vector[nsites] beta0;

##   vector[nsites] beta1;

## }

## 

## model {

##   vector[n] yhat;

## 

##   // Priors

##   sigma ~ inv_gamma(0.001, 0.001);

##   mu_beta0 ~ normal(150,150);

##   sd_beta0 ~ inv_gamma(0.001, 0.001);

##   mu_beta1 ~ normal(0, 10);

##   sd_beta1 ~ inv_gamma(0.001, 0.001);

##   tau ~ uniform(min(x), max(x));

## 

##   for (i in 1:nsites){

##     beta0[i] ~ normal(mu_beta0, sd_beta0);

##     beta1[i] ~ normal(mu_beta1, sd_beta1);

##   }

## 

##  // The model!

##   for (i in 1:n)

##     yhat[i] = beta0[site[i]] + int_step(x[i]-tau) * beta1[site[i]] * (x[i] - tau);

## 

##   // likelihood / probability model

##   y ~ normal(yhat, sigma);

## }


## ----HierarchicalHockeyStickCompiling, eval=FALSE, echo=1-----------------------------------------
eval <- FALSE
if(eval){
    hhs_stan <- stan_model(file = "stanmodels/hhs.stan")
    save(hhs_stan, file="stanmodels/hhs.stanmodel")
}


## ---- echo = FALSE--------------------------------------------------------------------------------
load(file="stanmodels/hhs.stanmodel")


## ---- warning=FALSE, message=FALSE----------------------------------------------------------------
# only lilacs, non NA
lp2 <- subset(lp, !is.na(BLOOM) & SPECIES == 2)
somestations <- names(which(table(lp2$STATION)>20)) %>% sample(30)
lilacs <- subset(lp2, STATION %in% somestations) %>% 
      merge(ll, by.x = "STATION", by.y = "ID")


## -------------------------------------------------------------------------------------------------
Data <- with(lilacs, list(x = YEAR, y = BLOOM, n = length(YEAR), 
              site = STATION %>% factor %>% as.integer, 
						  nsites = length(unique(STATION))))


## ----HHS.fit, cache=TRUE--------------------------------------------------------------------------
load(file="stanmodels/hhs.stanmodel")
hhs.fit <- sampling(hhs_stan, Data, iter = 2000, chains = 2, cores = 2)


## -------------------------------------------------------------------------------------------------
print(hhs.fit, 
      pars = c("mu_beta0", "sd_beta0", "mu_beta1","sd_beta1", "tau", "sigma"))


## ----HHS.iagnositcs-------------------------------------------------------------------------------
hhs.mcmc <- As.mcmc.list(hhs.fit, pars = c("mu_beta0", "sd_beta0", "mu_beta1","sd_beta1", "tau",  "sigma"))
xyplot(hhs.mcmc)
densityplot(hhs.mcmc)


## ---- eval=3, echo=1------------------------------------------------------------------------------
hhswc_stan <- stan_model("stanmodels/hhswithcovar.stan")

save(hhswc_stan, file = "hhswithcovar.stanmodel")
load(file = "stanmodels/hhswithcovar.stanmodel")


## -------------------------------------------------------------------------------------------------
covars <- with(lilacs, 
               model.matrix(BLOOM ~ I(Lat - mean(Lat)) + I((Elev - mean(Elev))/1000)))
head(covars)


## -------------------------------------------------------------------------------------------------
Data <- with(lilacs, 
            list(x = YEAR, y = BLOOM, n = length(YEAR), 
               site = STATION %>% factor %>% as.integer, 
						  nsites = length(unique(STATION)), 
						  covars = covars, ncovars = ncol(covars)))


## ---- echo=FALSE, eval=FALSE, echo = 1------------------------------------------------------------
hhswc.fit <- sampling(hhswc_stan, Data, chains = 3, iter = 1000, cores = 3)
save(hhswc.fit, file = "stanmodels/hhswithcovar.fit")


## ----HHSWC.sampling, cache=TRUE, echo = -1--------------------------------------------------------
load("stanmodels/hhswithcovar.fit")
pars <- names(hhswc.fit)[1:8]
hhswc.mcmc <- As.mcmc.list(hhswc.fit, pars)
summary(hhswc.mcmc)
xyplot(hhswc.mcmc)

## ---- fig.height = 6------------------------------------------------------------------------------
densityplot(hhswc.mcmc)

