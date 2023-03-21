## written by K. Garner and chatGPT, 2023
## create a plot that shows estimation
## then a gif to demonstrate the point of prediction
## the point is that we don't want to just measure what's happening now,
## we want to predict what's going to happen

# create sample data and define variables
set.seed(42) # meaning of life
xmin <- 260
xmax <- 400
ymin <- 56
ymax <- 62
beta <- .009
safe <- 57.5
y_int <- 55

x <- runif(100, xmin, xmax) # CO2 concentration (ppm)
y <- y_int + beta*(x) + rnorm(100, 0, 0.5)
y_color <- ifelse(y < safe, "blue", "red")

# create animation frames
n <- 50 # number of frames
frames <- list()
# ugly loop but pushed for time
count <- 0
for (i in seq(0, 1, length.out = n)) {
  count <- count + 1
  # calculate line of best fit
  fit <- lm(y ~ x, weights = i*ifelse(y < safe, 1, 0) + (1-i)*ifelse(y >= safe, 1, 0))
  
  # create plot
  plot(x, y, col = y_color, xlab = "CO2 concentration (ppm)", ylab = "Global temperature", 
            xlim = c(260, 400), ylim = c(ymin, ymax), pch=19, cex.axis = 1.5, cex.lab=1.5)
  abline(fit, col = "black")
  
  # add plot to frames
  filename <- paste0("prediction/lobf_frame_", count, ".png")
  dev.copy(png, filename, width = 600, height = 400)
  dev.off()
}

# create animated gif from saved frames
library(magick)
frames <- list.files(path= "prediction/", pattern = "lobf_frame_.*\\.png", full.names=TRUE)
frames_list <- lapply(frames, image_read)
frames_joined <- image_join(frames_list)
frames_animate <- image_animate(frames_joined, fps = 2)
frames_animate

image_write(frames_animate, "prediction/fit_lobf.gif")

### now compute the details required for the prediction model
mean(y)
sd(y)
cor.test(x,y)
r = cor.test(x,y)
mean(x)
sd(x)
# z-score 600 (x of interest)
Zx = (450 - mean(x))/sd(x)
# get the prediction (Zy = * Zx)
r$estimate*Zx

## now plot the z-transformed x and y, and fit the correlation line
nu_x <- scale(x)
nu_y <- scale(y)
nu_safe <- (safe - mean(y))/sd(y)
nuy_color <- ifelse(nu_y < nu_safe, "blue", "red")
nu_xlim <- c(-3, 3)
nu_ylim <- c(-3, 3)
plot(nu_x, nu_y, col = nuy_color, xlab = "CO2 concentration (ppm) Z", ylab = "Global temperature Z", 
     xlim = nu_xlim, ylim = nu_ylim, pch=19, cex.axis = 1.5, cex.lab=1.5)
abline(0, .69, col = "black", lwd = 2)
abline(h = 0, v = 0, col="grey", lwd = 1)
text(x=2, y=2.5, labels="r=.69", cex = 1.5)
dev.copy(png, "prediction/globalTmp_Z.png", width = 600, height = 400)
dev.off()

## now plot the prediction for 450
nu_x <- scale(x)
nu_y <- scale(y)
nu_safe <- (safe - mean(y))/sd(y)
nuy_color <- ifelse(nu_y < nu_safe, "blue", "red")
nu_xlim <- c(-3, 3)
nu_ylim <- c(-3, 3)
plot(nu_x, nu_y, col = nuy_color, xlab = "CO2 concentration (ppm) Z", ylab = "Global temperature Z", 
     xlim = nu_xlim, ylim = nu_ylim, pch=19, cex.axis = 1.5, cex.lab=1.5)
abline(0, .69, col = "black", lwd = 2)
abline(h = 0, v = 0, col="grey", lwd = 1)
text(x=2, y=2.5, labels="r=.69", cex = 1.5)
segments(x0=c(Zx, Zx), y0=c(-3.5, -3.5), 
         x1=c(Zx, Zx), y1=c(Zx*r$estimate, Zx*r$estimate), col="purple", lwd=2)
segments(x0=-3.5, y0=Zx*r$estimate, 
         x1=Zx, y1=Zx*r$estimate, col="purple", lwd=2)
text(x=1, y=-2.5, labels="Zx = 2.76", cex = 1.5)
text(x=-2, y=2.2, labels="Zy = 1.9", cex = 1.5)
dev.copy(png, "prediction/globalTmp_Z_wXandY.png", width = 600, height = 400)
dev.off()

