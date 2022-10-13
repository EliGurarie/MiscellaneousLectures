data(single)

plot(aurelia ~ Day, data = single, col = 1, type = "o")
lines(caudatum ~ Day, data = single, col = 2, type = "o")
legend("bottomright", legend = c("aurelia", "caudatum"), col = 1:2, pch = 1, lty = 1)

fit1 <- fitLogistic(single, y = "aurelia", time = "Day", 1, 200, .75)
fit2 <- fitLogistic(single, y = "caudatum", time = "Day", 1, 200, .75)

linesLogistic(fit1, lwd = 3)
linesLogistic(fit2, col = 2, lwd = 3)
