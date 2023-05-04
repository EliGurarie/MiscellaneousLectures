setwd("presentation/plots")
require(TuktuTools)

zarrows <- function(Z, ...){
  for(i in 2:length(Z))
    arrows(Re(Z[i-1]), Im(Z[i-1]),Re(Z[i]), Im(Z[i]), ...)
}

max <- 3
Z <- c(0, 1i)

thetas <- circular::rwrappedcauchy(100, 0, .8) %>% as.numeric


thetas <- seq(-pi/4, pi/4, length = 14)
set.seed(1)
steps <- rweibull(14, 2, 1)

z.steps <- complex(mod = steps, arg = Arg(Z[2]-Z[1]) + thetas) %>% c(1i)

xrange = c(-max/2, max/2)
yrange = c(-max/5, max * 1.2)

peak <- c(.8,2.5)
sd.x <- .3
sd.y <- .6

xs <- seq(xrange[1], xrange[2], length = 100)
ys <- seq(yrange[1], yrange[2], length = 100)
cols <- heat.colors(100)

library(zoo)

hab <-outer(xs, ys, function(x,y){
        mvtnorm::dmvnorm(cbind(x,y), mean = peak, sigma = diag(c(sd.x,sd.y)))
      }) + rnorm(length(xs) * length(ys), sd = .2)

hab <- apply(hab, 1, rollmean, k = 5, fill = 0) %>% 
  apply(1, rollmean, k = 5, fill = 0)


barrier <- c(-1.5+.7i, -.75 + 1i, -.25 + 2i, 0.2 + 2.5i, 0.25 + max(ys) * 1i)

{
  par(mar = c(0,0,0,0))
  plot(0,0, xlim = xrange, ylim = yrange, asp = 1)
  image(xs, ys, hab, add = TRUE, col = cols)
  points(Z, pch = 19, type = "o")
  zarrows(Z, lwd = 3)
  Z.new <- Z[2] + z.steps
  a <- sapply(Z.new, 
              function(z) zarrows(c(Z[2], z), 
                                  col = "darkgrey",
                                  length = .1))  
  points(Z.new, pch = 21, bg = "white", cex = 1.2, lwd = 2)
  lines(barrier, col = "darkgreen", lwd = 2, lty = 3)
}



# Plot
png("schematic1.png", width = 400, height = 600, res = 200)
{
  par(mar = c(0,0,0,0))
  plot(0,0, xlim = xrange, ylim = yrange, asp = 1)
  #image(xs, ys, hab, add = TRUE, col = cols)
  zarrows(Z, lwd = 3)
  points(Z, pch = 21, type = "o", cex = 1.5, bg = "lightgreen")
  Z.new <- Z[2] + z.steps
  a <- sapply(Z.new, 
              function(z) zarrows(c(Z[2], z), 
                                  col = "darkgrey",
                                  length = .1))  
  points(Z.new, pch = 21, bg = "white", cex = 1.2, lwd = 2)
}
dev.off()


require(permeability)
source("functions.R")


Z.new <- Z[2] + z.steps


plotWithKappa <- function(kappa){
  findCrossing.Z(z2 = barrier, z1.steps = cbind(rep(Z[2], length(Z.new)), Z.new))
  crossing.df <- findCrossing.Z(z2 = barrier, z1.steps = cbind(rep(Z[2], length(Z.new)), Z.new))
  plot(0,0, xlim = xrange, ylim = yrange, asp = 1, xaxt = "n", yaxt = "n")
  #image(xs, ys, hab, add = TRUE, col = cols)
  zarrows(Z, lwd = 3)
  points(Z, pch = 21, type = "o", cex = 1.5, bg = "lightgreen")
  w = ifelse(crossing.df$crossings == 1, kappa, 1) %>% {./sum(.)}
  
  lines(barrier, col = "darkgreen", lwd = 2, lty = 3)
  a <- sapply(Z.new, 
              function(z) zarrows(c(Z[2], z), 
                                  col = "darkgrey",
                                  length = .1))  
  points(Z.new, pch = 21, bg = "white", cex = sqrt(w * length(w) * 1.2))
}


kappa <- 0.5

png("schematic2.png", width = 1200, height = 500, res = 200)
{
  par(mar = c(0,0,3,0), mfrow = c(1,4), bty = "n")
  plotWithKappa(1); title(expression(kappa == 1))
  plotWithKappa(.5); title(expression(kappa == 0.7))
  plotWithKappa(.2); title(expression(kappa == 0.2))
  plotWithKappa(0); title(expression(kappa == 0))
}
dev.off()



