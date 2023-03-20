## written by K. Garner 2023
## produce bivariate correlation plots with a 
# given N and correlation
# assumes a mean of 0 for each variable

# Set the seed for reproducibility
set.seed(123)

###### Run the first section for linear relationships
#########################################################
# settings for plot
n <- 1000 # n data points
pearsons_r <- 0.08 # correlation between two values
yl = c(-3, 3)
xl = yl

# Generate two variables with 25 datapoints each
x <- rnorm(n)
y <- rnorm(n)

# Calculate the covariance matrix
cov_mat <- matrix(c(1, pearsons_r, pearsons_r, 1), nrow = 2, ncol = 2)

# Generate the correlated data using the Cholesky decomposition method
library(mvtnorm)
data_cor <- rmvnorm(n = n, mean = c(0, 0), sigma = cov_mat)

# Extract the two variables from the correlated data
x_cor <- data_cor[,1]
y_cor <- data_cor[,2]

# plot
plot(x_cor, y_cor, xlab = "x", ylab="y", cex = 1.5, pch=19, col="darkred",
     bty="n", cex.lab=1.5, cex.axis = 1.5, ylim=yl, xlim=xl)


###### Run the second section for non-linear relationships
#########################################################
x <- rnorm(n)
y <- -x^2 + rnorm(n, 0, 0.5)
plot(x, y, xlab = "x", ylab="y", cex = 1.5, pch=19, col="darkred", bty="n", cex.lab=1.5, cex.axis = 1.5)
