# Library for Mahalanobis distance function
require(biotools)

# Define initial data
data2 <- matrix(c(
  34, 93, 98, 75,
  39, 89, 98, 67,
  20, 40, 76, 32,
  50, 51, 88, 32
), nrow=4, byrow=TRUE)
rownames(data2) <- c("EST","DEU","POL","CYP")

S <- matrix(c(
  381.34, 330.23, 310.63, 413.61,
  330.23, 570.46, 448.30, 588.36,
  310.63, 448.30, 458.85, 502.37,
  413.61, 588.36, 502.37, 804.91
), nrow=4, byrow=TRUE)

# Euclidean distance matrix
Eucl_dist <- as.matrix(dist(data2, method="euclidean"))

# Mahalanobis distance matrix
Mah_dist <- as.matrix(D2.dist(data2, S))

# Perform PCA
result <- prcomp(data2, center=TRUE, scale=TRUE)
print(summary(result))

# Make scatterplot
plot(result$x[,1],result$x[,2], xlab="PC1 74%", ylab = "PC2 25%", main = "PCA")
