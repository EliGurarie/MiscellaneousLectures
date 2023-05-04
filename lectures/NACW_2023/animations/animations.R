# animations ------


source("./plots/functions.R")
require(circular)
Z0 <- -20
y.max <- x.max <- 10
kappa <- 0.5

N <- 1e3

n <- 1e3
rho = 0.8

angles <- rwrappedcauchy(n, rho = .8)
steps <- rweibull(n, 2,4)/5


animateZ <- function(Z.sim, xmax = x.max, ymax = y.max, 
                     b = barrier, title = ""){
  par(mar = c(0,0,3,0), bty = "n")
  for(i in 1:length(Z.sim)){
    plot(b, type = "l", xlim = c(-1,1)*xmax, ylim = c(-1,1)*ymax, 
         asp  = 1, lwd = 4, col = "darkgrey", main = title)
    lines(Z.sim[1:i], col = "darkgreen")  
    points(Z.sim[i], pch = 21, bg = "green", col = "darkgreen")
  }
}

barrier <- c(-y.max, y.max)*1i
Z0 <- -2.5 + 0i


eval <- FALSE
if(eval){
Z.impermeable <- simWalk(400, Z0 = Z0, angles, steps, kappa = 0, 
                     barrier = barrier, xmax = 10, ymax = 10, plotme = TRUE)
Z.semipermeable <- simWalk(400, Z0 = Z0, angles, steps, kappa = .2, 
                     barrier = barrier, xmax = 10, ymax = 10, plotme = TRUE)
Z.permeable <- simWalk(400, Z0 = Z0, angles, steps, kappa = 1, 
                     barrier = barrier, xmax = 10, ymax = 10, plotme = TRUE)

save(Z.impermeable, Z.semipermeable, Z.permeable, file = "animations/three_animations.rda")
} else load("animations/three_animations.rda")

require(animation)
ani.options(interval = 0.02, loop = FALSE, ani.width = 600, ani.height = 600, ani.res = 200)
saveVideo(
  animateZ(Z.impermeable, title = expression(kappa == 0)),
  video.name = "impermeable.mp4",
  img.name = "impermeable",
  ffmpeg = ani.options("ffmpeg")
)
saveVideo(
  animateZ(Z.semipermeable, title = expression(kappa == 0.2)),
  video.name = "semipermeable.mp4",
  img.name = "semipermeable",
  ffmpeg = ani.options("ffmpeg")
)

saveVideo(
  animateZ(Z.permeable, title = expression(kappa == 1)),
  video.name = "permeable.mp4",
  img.name = "permeable",
  ffmpeg = ani.options("ffmpeg")
)
