library(ggplot2)
library(magick)
library(gridExtra)

# Set the seed for reproducibility
set.seed(123)

# Create the data
x <- 1:12
y <- 2.7 + 0.56 * x + rnorm(12, 0, 1)

# Calculate the line of best fit
model <- lm(y ~ x)
predicted <- predict(model, data.frame(x = x))
# Create a data frame for the plot
df <- data.frame(x, y, predicted)
# Calculate the mean of y
y_mean <- mean(y)


# Create a function to generate each frame of the animation
generate_frame <- function(i) {
  # Highlight the current point
  df$highlight <- ifelse(df$x == i, "highlight", "regular")

  # Create a base plot with the points and line of best fit
  base <- ggplot(df, aes(x, y)) +
    geom_point(aes(color = highlight), size = 5) +
    geom_smooth(aes(y = predicted), method = "lm", color = "#FFC300", size = 1.2) +
    theme_bw() +
    theme(
      text = element_text(size = 16),
      plot.background = element_rect(fill = "#0D1F2D")
    ) +
    geom_segment(aes(x=0, y=mean(y), xend=12, yend=mean(y)), colour="darkgrey") +
    scale_color_manual(values = c("regular" = "#8F9779", "highlight" = "#FF5733")) +
    theme(
      legend.position = "none"
    )
  
  
  # Create a new plot with the highlighted point and segment

  disp <- base + geom_segment(aes(x = x[i]-.1, y = y[i], 
                   xend = x[i]-.1, yend = y_mean),
               color = "#C70039", size = 1, 
               arrow = arrow(length = unit(0.25, "cm"))) 
    
    pred <- base + geom_segment(aes(x = x[i]+.1, y =  predicted[i], 
                     xend = x[i]+.1, yend = y_mean),
                     color = "#FFC300", size = 1, 
                     arrow = arrow(length = unit(0.25, "cm"))) 
    
    res <- base + geom_segment(aes(x = x[i]+.15, y = y[i], 
                     xend = x[i]+.15, yend = predicted[i]),
                 color = "darkblue", size = 1, 
                 arrow = arrow(length = unit(0.25, "cm")))
    
  
  # Generate the plot as a magick image
  all <- grid.arrange(disp, pred, res, ncol = 3)
  ggsave(filename=paste("prediction/y_partition_", i, ".png", sep=""), plot=all, width=30, height=10, units="cm")
}

# Generate a list of frames for the animation
frames <- lapply(1:12, generate_frame)

# Combine the frames into an animated gif
frames <- list.files(path= "prediction", pattern = "y_partition_.*\\.png", full.names=TRUE)
frames_list <- lapply(frames, image_read)
frames_joined <- image_join(frames_list)
frames_animate <- image_animate(frames_joined, fps = 2)
frames_animate

magick::image_write(frames_animate, "prediction/partition_y_animate.gif")
