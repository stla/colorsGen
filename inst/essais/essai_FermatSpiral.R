library(colorsGen)

n <- 400
theta <- seq(0, n/3, length.out = n)
x <- sqrt(theta) * cos(theta)
y <- sqrt(theta) * sin(theta)
pts <- cbind(x, y)
clrs <- randomColor(n, hue = "random", luminosity = "bright")
opar <- par(mar = c(0, 0, 0, 0), bg = "black")
# Here is a Fermat spiral:
plot(pts, asp = 1, xlab = NA, ylab = NA, axes = FALSE, pch = 19, col = clrs)
par(opar)
