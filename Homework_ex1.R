# Define initial data
data1 <- matrix(c(
  0.098, 0.154, 0.559, 0.809,
  0.006, 0.220, -0.822, 0.526,
  0.825, -0.553, -0.096, 0.072,
  0.557, 0.789, 0.053, -0.254
), nrow=4)

# Calculate covariance matrix
S = cov(data1)

# Calculate correlation matrix from covariance matrix
R = cov2cor(S)

# Perform PCA
result <- prcomp(data1, center=TRUE, scale=TRUE)
print(summary(result))

# Make screeplot
screeplot(result, main="PCA", type="lines", ylim=c(0,2))