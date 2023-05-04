zarrows <- function(Z, ...){
  for(i in 2:length(Z))
    arrows(Re(Z[i-1]), Im(Z[i-1]),Re(Z[i]), Im(Z[i]), ...)
}

plotWithKappa <- function(kappa, r = NULL, beta = 1, 
                          z = Z, b = barrier, z.new = Z.new){
  
  findCrossing.Z(z2 = barrier, 
                 z1.steps = cbind(rep(z[2], length(z.new)), z.new))
  crossing.df <- findCrossing.Z(z2 = barrier, 
                                z1.steps = cbind(rep(z[2], length(z.new)), z.new))
  
  plot(0,0, xlim = xrange, ylim = yrange, asp = 1, xaxt = "n", yaxt = "n")
  if(!is.null(r)){
    image(r, add = TRUE, col = terrain.colors(100))
    habitat <- raster::extract(r, cbind(Re(z.new), Im(z.new)))
  } else habitat <- 1
  
  crossing = ifelse(crossing.df$crossings == 1, kappa, 1) %>% {./sum(.)}
  w = (ifelse(crossing.df$crossings == 1, kappa, 1) * beta * habitat) %>% {./sum(.)}
  
  zarrows(c(1.5 - 3i, z), lwd = 3, length  = 0.25)
  lines(barrier, col = "darkgreen", lwd = 2, lty = 3)
  
  a <- sapply(Z.new, function(myz) 
    zarrows(c(z[2], myz), col = "darkgrey", length = .1))  
  
  points(z.new, pch = 21, bg = "white", cex = sqrt(w * length(w) * 1.5))
  points(z, pch = 21, type = "o", cex = 1.5, bg = "lightgreen")
}


findCrossing.Z <- function(z1, z2, z1.steps = NULL){
  
  if(is.null(z1.steps)){
    n1 <- length(z1)
    start1 <- z1[-n1]
    end1 <- z1[-1]
    z1.steps <- cbind(start1, end1)
  } else {
    n1 <- nrow(z1.steps)+1
    start1 <- z1.steps[,1]
    end1 <- z1.steps[,2]
  }
  
  n2 <- length(z2)
  start2 <- z2[-n2]
  end2 <- z2[-1]
  z2.steps <- cbind(start2, end2)
  
  crossings.Matrix <- matrix(NA, n1 - 1, n2 - 1)
  for(i in 1:(n1 -1)) for(j in 1:(n2 - 1))
    crossings.Matrix[i,j] <- isIntersect(z1.steps[i,], z2.steps[j,])
  
  which.cross <- which(crossings.Matrix, arr.ind = TRUE)
  
  crossing.df <- data.frame(start = start1, end = end1, 
             crossings = rowSums(crossings.Matrix),
             barrier.index = NA) 
  crossing.df$barrier.index[which.cross[,1]] <- which.cross[,2]
  crossing.df$side 
  crossing.df
}

sideSwitch <- function(Z1, Z2, b){
  score1 <- (Re(Z1) - Re(b[1])) * (Im(b[2]) - Im(b[1])) - 
    (Im(Z1) - Im(b[1])) * (Re(b[2]) - Re(b[1])) 
  score2 <- (Re(Z2) - Re(b[1])) * (Im(b[2]) - Im(b[1])) - 
    (Im(Z2) - Im(b[1])) * (Re(b[2]) - Re(b[1])) 
  return(score1*score2 < 0)
}

inside <- function(Z.new, x.max, y.max){
  Re(Z.new) < x.max & Re(Z.new) > -x.max & Im(Z.new) < y.max & Im(Z.new) > -y.max
}

simWalk <- function(N, Z0, angles, steps, kappa, barrier, 
                    xmax = x.max, ymax = y.max, plotme = TRUE){
  Z1 <- Z0 + complex(mod = sample(steps, 1), arg = runif(1,-pi,pi))
  Z <- c(Z0, Z1)
  Phi <- Arg(Z1 - Z0)
  for(i in 1:N){
    tryagain <- TRUE
    while(tryagain){
      Z.old <- Z[length(Z)]
      samplestep <- complex(arg = Phi+sample(angles, 1), mod = sample(steps)[1])
      Z.new <- Z[length(Z)] + samplestep
      if(inside(Z.new, xmax, ymax) & 
         ( !sideSwitch(Z.old, Z.new, barrier) |
           (sideSwitch(Z.old, Z.new, barrier) & runif(1) < kappa))) {
        Phi <- Arg(Z.new - Z.old)
        Z <- c(Z, Z.new)
        tryagain <- FALSE
      }
    }
  }
  if(plotme){
    plot(barrier, type = "o",
         xlim = c(-1,1) * xmax, ylim = c(-1,1) * ymax, 
         asp = 1, lwd = 4, col = "darkgrey")
    lines(Z, type = "o")  
  }
  return(Z)
}
