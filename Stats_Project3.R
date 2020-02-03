n_vals = c(1,2,3,5,10,30)
theta_vals = c(1,5,50,100)

mse <- function(n, theta) {
  estimates <- function(n, theta) {
    # random draws
    x <- runif(n, min = 0, max = theta)
    # mle = max of sample
    mle <- max(x)
    # mome = twice of mean of sample
    mome <- 2 * mean(x)
    return (c(mle = mle, mome = mome))
  }
  # replicate 1000 times
  estimatesAfterReplications <- replicate(1000, estimates(n, theta))
  # return mse values for mle, mome respectively
  return (rowMeans((estimatesAfterReplications - theta) ^ 2))
}

# create MSE matrix for MLE
mle_matrix = matrix(nrow = length(n_vals), ncol = length(theta_vals))
rownames(mle_matrix) <- n_vals
colnames(mle_matrix) <- theta_vals

# create MSE matrix for MOME
mome_matrix = matrix(nrow = length(n_vals), ncol = length(theta_vals))
rownames(mome_matrix) <- n_vals
colnames(mome_matrix) <- theta_vals

# fill values in the matrices
for (i in 1:length(n_vals)) {
  for (j in 1:length(theta_vals)) {
    value <- mse(n_vals[i], theta_vals[j])
    mle_matrix[i, j] <- value["mle"]
    mome_matrix[i, j] <- value["mome"]
  }
}

# print matrices
mle_matrix
mome_matrix

# dividing plots into 2X2 array of subplots
par(mfrow = c(2, 2))
# subplots for each value of theta
for (i in 1:length(theta_vals)) {
  plot(n_vals, mle_matrix[,i],
  # plotting lines and points both
  type = "b",
  col = "red",
  ylab = "MSE",
  xlab = "n",
  main = paste("Theta: ", theta_vals[i]),
  )
  lines(n_vals, mome_matrix[,i],
  col = "blue",
  type = "b"
  )
  legend("topright", c("MLE", "MOME"),
  fill = c("red", "blue"))
}



