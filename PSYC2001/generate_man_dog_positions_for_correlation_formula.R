## written by K. Garner 2023
## this code creates plots that aim to visualise why the z-score correlation
## formula works (r_xy = sum(z_x*z_y)/n)

ylims = c(-1,3)

## Plot 1 - man's position on his own
# Define the vector of values
man_values <- c(1, 2, 3, 4, 3, 2, 1, 1) # man

# Convert the values to z-scores
man_z_scores <- scale(values)

# Create a vector of x-axis labels
x_labels <- paste0("Z_", 1:length(z_scores))

# Plot the z-scores as upward lines
plot(man_z_scores, type = "n", xaxt = "n", ylab = "z",
     xlab = "position", bty = "n", ylim=ylims)
for(i in 1:length(z_scores)){
  lines(c(i,i), c(0,z_scores[i]), col = "blue")
}

# Add x-axis labels and legend
axis(1, at = 1:length(man_z_scores), labels = x_labels)
legend("topright", legend = "man", col = "blue", lty = 1, bty = "n")

## Plot 2 - man with dog's position added
# Define the vectors of values
# Define the vectors of values
values1 <- c(1, 2, 3, 4, 3, 2, 1, 1)
values2 <- c(0.8, 2.2, 2.7, 4.1, 2.8, 0.7, 1.1, 0.9)

# Convert the values to z-scores
z_scores1 <- scale(values1)
z_scores2 <- scale(values2)

# Create a vector of x-axis labels
x_labels <- paste0("Z_", 1:length(z_scores1))

# Set up plot area
plot(NA, xlim = c(0.5, length(z_scores1) + 0.5), ylim = ylims, 
     xlab = "", ylab = "z", bty="n", xaxt="n")

# Plot the z-scores for 'man' as upward lines
for (i in 1:length(z_scores1)) {
  lines(c(i - 0.2, i - 0.2), c(0, z_scores1[i]), col = "blue", xaxt="n", main="")
}

# Plot the z-scores for 'dog' as upward lines
for (i in 1:length(z_scores2)) {
  lines(c(i + 0.2, i + 0.2), c(0, z_scores2[i]), col = "orange")
}

# Add x-axis labels and legend
axis(1, at = 1:length(z_scores1), labels = x_labels)
legend("topright", legend = c("Man", "Dog"), col = c("blue", "orange"), lty = 1, bty="n")

###### Plot 3 
# Define the vectors of values
# Define the vectors of values
values1 <- c(1, 2, 3, 4, 3, 2, 1, 1)
values2 <- c(0.8, 2.2, 2.7, 4.1, 2.8, 0.7, 1.1, 0.9)

# Convert the values to z-scores
z_scores1 <- scale(values1)
z_scores2 <- scale(values2)
z_scores3 <- z_scores1 * z_scores2  # Calculate 'man x dog'

# Create a vector of x-axis labels
x_labels <- c(paste0("Z_", 1:length(z_scores1)), "Man x Dog")

# Set up plot area
plot(NA, xlim = c(0.5, length(z_scores1) + 1.5), ylim = ylims, 
     xlab = "", ylab = "z", bty = "n", xaxt = "n")

# Plot the z-scores for 'man' as upward lines
for (i in 1:length(z_scores1)) {
  lines(c(i - 0.2, i - 0.2), c(0, z_scores1[i]), col = "blue", xaxt = "n", main = "")
}

# Plot the z-scores for 'dog' as upward lines
for (i in 1:length(z_scores2)) {
  lines(c(i + 0.2, i + 0.2), c(0, z_scores2[i]), col = "orange")
}

# Plot the z-scores for 'man x dog' as upward lines
for (i in 1:length(z_scores3)) {
  lines(c(i + 0.5, i + 0.5), c(0, z_scores3[i]), col = "purple", lwd=3)
}

# Add x-axis labels and legend
axis(1, at = 1:length(x_labels), labels = x_labels)
legend("topright", legend = c("Man", "Dog", "Man x Dog"), col = c("blue", "orange", "purple"), lty = 1, bty = "n")

###### Plot 4
###### man and dog not related
# Define the vectors of values
# Define the vectors of values
values1 <- c(1, 2, 3, 4, 3, 2, 1, 1)
values2 <- c(1, 2, 1, 2, 1, 2, 1, 2)

# Convert the values to z-scores
z_scores1 <- scale(values1)
z_scores2 <- scale(values2)
z_scores3 <- z_scores1 * z_scores2  # Calculate 'man x dog'

# Create a vector of x-axis labels
x_labels <- c(paste0("Z_", 1:length(z_scores1)), "Man x Dog")

# Set up plot area
plot(NA, xlim = c(0.5, length(z_scores1) + 1.5), ylim = ylims, 
     xlab = "", ylab = "z", bty = "n", xaxt = "n")

# Plot the z-scores for 'man' as upward lines
for (i in 1:length(z_scores1)) {
  lines(c(i - 0.2, i - 0.2), c(0, z_scores1[i]), col = "blue", xaxt = "n", main = "")
}

# Plot the z-scores for 'dog' as upward lines
for (i in 1:length(z_scores2)) {
  lines(c(i + 0.2, i + 0.2), c(0, z_scores2[i]), col = "orange")
}

# Plot the z-scores for 'man x dog' as upward lines
for (i in 1:length(z_scores3)) {
  lines(c(i + 0.5, i + 0.5), c(0, z_scores3[i]), col = "purple", lwd=3)
}

# Add x-axis labels and legend
axis(1, at = 1:length(x_labels), labels = x_labels)
legend("topright", legend = c("Man", "Dog", "Man x Dog"), col = c("blue", "orange", "purple"), lty = 1, bty = "n")

## Plot 2 - man and dog not related
# Define the vectors of values
# Define the vectors of values
values1 <- c(1, 2, 3, 4, 3, 2, 1, 1)
values2 <- c(1, 2, 1, 2, 1, 2, 1, 2)

# Convert the values to z-scores
z_scores1 <- scale(values1)
z_scores2 <- scale(values2)

# Create a vector of x-axis labels
x_labels <- paste0("Z_", 1:length(z_scores1))

# Set up plot area
plot(NA, xlim = c(0.5, length(z_scores1) + 0.5), ylim = ylims, 
     xlab = "", ylab = "z", bty="n", xaxt="n")

# Plot the z-scores for 'man' as upward lines
for (i in 1:length(z_scores1)) {
  lines(c(i - 0.2, i - 0.2), c(0, z_scores1[i]), col = "blue", xaxt="n", main="")
}

# Plot the z-scores for 'dog' as upward lines
for (i in 1:length(z_scores2)) {
  lines(c(i + 0.2, i + 0.2), c(0, z_scores2[i]), col = "orange")
}

# Add x-axis labels and legend
axis(1, at = 1:length(z_scores1), labels = x_labels)
legend("topright", legend = c("Man", "Dog"), col = c("blue", "orange"), lty = 1, bty="n")


#### now dividing by N principle
