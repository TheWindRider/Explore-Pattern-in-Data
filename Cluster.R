library(dplyr)
library(lattice)

cluster_analysis <- function(data_set, observeVar, clusterVar, k) {
  # Apply K-Means Clustering
  cluster_model <- kmeans(data_set[clusterVar], k)
  data_set["cluster_label"] <- cluster_model$cluster
  
  # Examine Each Cluster
  for (label in unique(cluster_model$cluster)) {
    print(paste("Cluster", label, sep = ':'))
    print(cor(select(filter(data_set, cluster_label == label), observeVar)))
    plot(select(filter(data_set, cluster_label == label), observeVar))
  }
  
  # Display Overall Distribution
  formula <- paste("~", paste(observeVar, collapse = " + "), "| cluster_label")
  do.call("histogram", list(as.formula(formula), data=as.name("data_set"), nint=50))
}