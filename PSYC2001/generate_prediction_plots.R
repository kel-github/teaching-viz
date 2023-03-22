## written by K. Garner and chatGPT, 2023
## create a plot that shows estimation
## then a gif to demonstrate the point of prediction
## the point is that we don't want to just measure what's happening now,
## we want to predict what's going to happen

# create sample data and define variables
set.seed(42) # measning of life
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

png(filename="prediction/estimation.png", width = 600, height = 400)
plot(x, y, col = y_color, xlab = "CO2 concentration (ppm)", ylab = "Global temperature", 
     xlim = c(260, 400), ylim = c(ymin, ymax), pch=19, cex.axis = 1.5, cex.lab=1.5)

# add line of best fit
abline(lm(y ~ x), col = "black")
dev.off()
# save this plot
#dev.off()

png(filename="prediction/estimation_wextendaxis.png", width = 600, height = 400)
plot(x, y, col = y_color, xlab = "CO2 concentration (ppm)", ylab = "Global temperature", 
     xlim = c(260, 475), ylim = c(ymin, ymax), pch=19, cex.axis = 1.5, cex.lab=1.5)

# add line of best fit
abline(lm(y ~ x), col = "black")
dev.off()


# create function to update plot and save each frame for gif
create_frame <- function(x_max){
  # calculate new y values based on regression equation
  new_y <- y_int + beta*(xmin:x_max)
  # plot new data and line of best fit
  plot(x, y, col = y_color, xlab = "CO2 concentration (ppm)", ylab = "Global temperature", 
       xlim = c(xmin, x_max), ylim = c(ymin, ymax), pch=19, cex.axis = 1.5, cex.lab=1.5)
  abline(y_int, beta, col = "black")
  #lines(xmax:x_max, new_y, col = "purple")
  # save plot as png file
  filename <- paste0("prediction/frame_", x_max, ".png")
  dev.copy(png, filename, width = 600, height = 400)
  dev.off()
}

# create frames for gif
for (x_max in c(450, 500, 600, 650, 700, 750)){
  create_frame(x_max)
}

# create animated gif from saved frames
library(magick)
frames <- list.files(path= "prediction/", pattern = "frame_.*\\.png", full.names=TRUE)
frames_list <- lapply(frames, image_read)
frames_joined <- image_join(frames_list)
frames_animate <- image_animate(frames_joined, fps = 2)
frames_animate

image_write(frames_animate, "prediction/prediction_forecast.gif")

