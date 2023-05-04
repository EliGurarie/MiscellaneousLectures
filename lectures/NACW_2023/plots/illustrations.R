require(permeability)
#setwd("presentation/plots")
require(TuktuTools)

source("functions.R")


max <- 3
Z <- c(0, 1i)

thetas <- circular::rwrappedcauchy(100, 0, .8) %>% as.numeric


thetas <- seq(-pi/4, pi/4, length = 14)
set.seed(1)
steps <- rweibull(14, 2, 1)

z.steps <- complex(mod = steps, arg = Arg(Z[2]-Z[1]) + thetas) %>% c(1i)

xrange = c(-max/2, max/2)
yrange = c(-max/5, max * 1.2)

peak <- c(-.25,2.5)
sd.x <- .8
sd.y <- .5

xs <- seq(xrange[1], xrange[2], length = 100)
ys <- seq(yrange[1], yrange[2], length = 100)
cols <- heat.colors(100)

Z.new <- Z[2] + z.steps
barrier <- c(-1.5+.7i, -.75 + 1i, -.25 + 2i, 0.2 + 2.5i, 0.25 + max(ys) * 1i)




# Plot

png("permeable.png", width = 600, height = 800, res = 200)
  par(mar = c(0,0,0,0), bty = "n")
  plotWithKappa(1)
  mtext(side = 3, line = -1, cex = 1.25, 
        "probabilities all equal", col = "darkgreen", font = 2)
dev.off()

png("semipermeable.png", width = 600, height = 800, res = 200)
par(mar = c(0,0,0,0), bty = "n")
plotWithKappa(.2)
mtext(side = 3, line = -1, cex = 1.25, 
      "P(Z|cross) < P(Z|stay)", col = "darkgreen", font = 2)
dev.off()


png("impermeable.png", width = 600, height = 800, res = 200)
par(mar = c(0,0,0,0), bty = "n")
plotWithKappa(0)
mtext(side = 3, line = -1, cex = 1.25, 
      "Pr(Z|cross) = 0", col = "darkgreen", font = 2)
dev.off()

png("hyperpermeable.png", width = 600, height = 800, res = 200)
par(mar = c(0,0,0,0), bty = "n")
plotWithKappa(3)
mtext(side = 3, line = -1, cex = 1.25, 
      "Pr(Z|cross) > 1", col = "darkgreen", font = 2)
dev.off()


# With habitat --------


library(zoo)

hab <-outer(xs, ys, function(x,y){
  mvtnorm::dmvnorm(cbind(x,y), mean = peak, sigma = diag(c(sd.x,sd.y)))
}) + rnorm(length(xs) * length(ys), sd = .2)

hab <- apply(hab, 1, rollmean, k = 5, fill = 0) %>% 
  apply(1, rollmean, k = 5, fill = 0)


image(hab)

require(raster)
hab.r <- raster(nrows = length(xs), ncols = length(ys),
                xmn = min(xs), xmx = max(xs), 
                ymn = min(ys), ymx = max(ys)) 

rotate <- function(x) t(apply(x, 2, rev))
values(hab.r) <- rotate(rotate(rotate(hab)))
plot(hab.r)




{
  par(mar = c(0,0,0,0))
  plot(0,0, xlim = xrange, ylim = yrange, asp = 1)
  plot(hab.r, add = TRUE)#image(xs, ys, hab, add = TRUE, col = cols)
  points(Z, pch = 19, type = "o")
  zarrows(Z, lwd = 3)
  a <- sapply(Z.new, 
              function(z) zarrows(c(Z[2], z), 
                                  col = "darkgrey",
                                  length = .1))  
  points(Z.new, pch = 21, bg = "white", cex = 1.2, lwd = 2)
  lines(barrier, col = "darkgreen", lwd = 2, lty = 3)
}





require(permeability)
source("functions.R")





Z.new <- Z[2] + z.steps
kappa <- 0.5

png("schematic2.png", width = 1800, height = 675, res = 300)
{
  par(mar = c(0,0,3,0), mfrow = c(1,4), bty = "n")
  plotWithKappa(1); title(expression(kappa == 1))
  plotWithKappa(.5); title(expression(kappa == 0.7))
  plotWithKappa(.2); title(expression(kappa == 0.2))
  plotWithKappa(0); title(expression(kappa == 0))
}
dev.off()



png("schematic3.png", width = 1800, height = 675, res = 300)
{
  par(mar = c(0,0,3,0), mfrow = c(1,4), bty = "n")
  plotWithKappa(1, r = hab.r, beta = 2); title(expression(kappa == 1))
  plotWithKappa(.5, r = hab.r, beta = 2); title(expression(kappa == 0.7))
  plotWithKappa(.2, r = hab.r, beta = 2); title(expression(kappa == 0.2))
  plotWithKappa(0, r = hab.r, beta = 2); title(expression(kappa == 0))
}
dev.off()
