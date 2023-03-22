# written by K. Garner and ChatGPT3, 2023

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

# Calculate the mean of y
y_mean <- mean(y)

# Choose the point to highlight
highlight_point <- 3

# Create a data frame for the plot
df <- data.frame(x, y, predicted)

# Create a column to indicate which point to highlight
df$highlight <- ifelse(df$x == highlight_point, "highlight", "regular")

# Create a plot with the points and line of best fit
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

# Add the point and initial segment to the plot
# this shows Y-MY
var <- base +
  geom_point(aes(x = x[highlight_point], y = y[highlight_point]), 
             color = "#FF5733", size = 5) +
  geom_segment(aes(x = x[highlight_point]-.1, y = y[highlight_point], 
                   xend = x[highlight_point]-.1, yend = mean(y)),
               color = "#C70039", size = 1, 
               arrow = arrow(length = unit(0.25, "cm"))) +
  geom_text(aes(x = x[highlight_point]-.2, y = mean(y)+.25, label = "Y - MY"), hjust = 1.2, color = "#C70039") 

pe <- base +
  geom_segment(aes(x = x[highlight_point]+.1, y =  predicted[highlight_point], 
                   xend = x[highlight_point]+.1, yend = mean(y)),
               color = "#FFC300", size = 1, 
               arrow = arrow(length = unit(0.25, "cm"))) +
  geom_text(aes(x = x[highlight_point]+2, y = predicted[highlight_point]-.6, label = "Y' - MY"), 
            hjust = 1.2, color = "#FFC300") 

resid <- base +
  geom_segment(aes(x = x[highlight_point]+.15, y = y[highlight_point]-.15, 
                   xend = x[highlight_point]+.15, yend = predicted[highlight_point]),
               color = "darkblue", size = 1, 
               arrow = arrow(length = unit(0.25, "cm"))) +
  geom_text(aes(x = x[highlight_point]-.7, y = predicted[highlight_point]+.25, label = "Y - Y'"), 
            hjust = 1.2, color = "darkblue") 

all <- grid.arrange(var, pe, resid, ncol = 3)
ggsave(filename="prediction/y_partition.png", plot=all, width=30, height=10, units="cm")
#ggsave(filename="prediction/y_base.png", plot=base, width=10, height=10, units="cm")
