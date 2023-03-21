## written by K. Garner and chatGPT, 2023
## create a plot that plots a straight line

# create sample data and define variables
x <- seq(-2, 3, .1)
y_int <- 1
beta <- 0
y <- y_int + beta*x

png(filename=sprintf("prediction/int%d.png", y_int), width = 600, height = 600)

plot(x, y, xlab = "X", ylab = "Y", type = "l",
     lwd=2, col = "purple", pch=19, cex.axis = 2, cex.lab=2, xlim = c(-1, 3))
abline(h = seq(min(y), max(y), by = 1), v = seq(min(x), max(x), by = 1), col = "gray")
abline(v=0, col="black", lwd = 2)
abline(h=0, col="black", lwd = 2)
dev.off()
# save this plot
#dev.off()
