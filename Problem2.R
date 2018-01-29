# Install and Load Libraries
library(dplyr)
library(lattice)
library(tourr)
library(fitdistrplus)

# Load Dataset
data_dir <- "C:/Users/Chenguang/Workspace/Challenges/data_dist/data_set.txt"
data_set <- read.table(data_dir, quote="\"", comment.char="")

# Pair-wise Plotting
plot(data_set)
# 3D Visualization
animate(data_set, fps=15)
## There Seems 2 Clusters

# Either (V2 V3) or (V1 V2 V3) as Variables
cluster_model_23 <- kmeans(data_set[c("V2", "V3")], 2)
cluster_model_123 <- kmeans(data_set[c("V1", "V2", "V3")], 2)
data_set["cluster_23"] <- cluster_model_23$cluster
data_set["cluster_123"] <- cluster_model_123$cluster
table(data_set$cluster_23, data_set$cluster_123)
## 2 candidate clusters Similar

# Compare Correlations Within Cluster
cor(dplyr::select(filter(data_set, cluster_23 == 1), V1:V3))
cor(dplyr::select(filter(data_set, cluster_23 == 2), V1:V3))
cor(dplyr::select(filter(data_set, cluster_123 == 1), V1:V3))
cor(dplyr::select(filter(data_set, cluster_123 == 2), V1:V3))
## Choose cluster_23, More Independent V1, V2, V3
data_set$cluster <- data_set$cluster_23
data_set$cluster_23 <- NULL
data_set$cluster_123 <- NULL

# Each Cluster
histogram(~ V1 + V2 + V3 | cluster, data = data_set, nint = 50)
plot(dplyr::select(filter(data_set, cluster == 1), V1:V3))
plot(dplyr::select(filter(data_set, cluster == 2), V1:V3))

# Estimate Distribution for Cluster 1 and 2, Variable V1, V2 and V3
table(data_set$cluster)
for (c in c(1, 2)) {
  for (v in c("V1", "V2", "V3")) {
    cv <- filter(data_set, cluster == c)[[v]]
    offset <- floor(min(cv))
    # Consider Weibull or Normal distribution
    dist_weibull <- fitdist(cv - offset, "weibull")
    dist_normal <- fitdist(cv, "norm")
    cat(sprintf("Cluster%d %s ~ ", c, v))
    # Compare Relative Likelihood
    if (dist_weibull$aic < dist_normal$aic) {
      cat(sprintf("Weibull: shape=%.1f, scale=%.1f, offset=%d \n", 
                  dist_weibull$estimate["shape"], dist_weibull$estimate["scale"], offset))
    }
    else {
      cat(sprintf("Normal: mean=%.1f, sd=%.1f \n", 
                  dist_normal$estimate["mean"], dist_normal$estimate["sd"]))
    }
  }
}

# Reference
# https://stats.stackexchange.com/questions/132652/how-to-determine-which-distribution-fits-my-data-best
# http://www.science.smith.edu/~amcnamara/BigDataDay/