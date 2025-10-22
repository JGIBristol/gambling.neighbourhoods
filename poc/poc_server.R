#   This is a Proof of Concept (PoC) script for clustering gambling premises data.
#   It reads in gambling premises data and geographical data, cleans it, and performs HDBSCAN clustering.
# Load necessary libraries

library(dplyr)
# Load geospatial libraries
library(sp)
library(geosphere)
# Load libraries for clustering
library(dbscan)

# Load ggplot2 for plotting
library(ggplot2)


# HDBSCAN clustering
# Using Sys.time to measure execution time
# trying with minPts = 10
mdist <- readRDS("data/mdist.rds")

# # Perform HDBSCAN clustering for different MinPts values and save results
# for (MinPts in c(2:5,10)) {
#   start_time <- Sys.time()
#   print(paste("MinPts =", MinPts))
#   res <- dbscan::hdbscan(mdist, minPts = MinPts, verbose = TRUE)
#   saveRDS(c(res), file = paste0("results/hdbscan_results_", MinPts, ".rds"))
#   print(paste0("HDBSCAN results saved to results/hdbscan_results_", MinPts, ".rds"))
#   end_time <- Sys.time()
#   print(paste("Execution time:", end_time - start_time))
# }

# Perform hybrid HDBSCAN clustering with eps=500m for different MinPts values and save results

# eps=500
# for (MinPts in c(4:5, 10, 15, 20)) {
#   start_time <- Sys.time()
#   print(paste("MinPts =", MinPts))
#   res <- dbscan::hdbscan(mdist, minPts = MinPts, cluster_selection_epsilon = eps, verbose = TRUE)
#   saveRDS(c(res), file = paste0("results/hdbscan_results_eps", eps, "_MinPts", MinPts, ".rds"))
#   print(paste0("HDBSCAN results saved to results/hdbscan_results_eps_", eps, "_MinPts_", MinPts, ".rds"))
#   end_time <- Sys.time()
#   exec_time <- end_time - start_time
#   print(paste("Execution time:", exec_time))
#   # save execution time into an unique file and append on a matrix
#   write.table(data.frame(MinPts = MinPts, ExecutionTime = exec_time, EPS = eps), file = "results/hdbscan_execution_times.csv", append = TRUE, sep = ",", col.names = !file.exists("results/hdbscan_execution_times.csv"), row.names = FALSE)

# }


# DBSCAN clustering
# Using Sys.time to measure execution time
eps=500
for (MinPts in c(4:5, 10, 15, 20)) {
  start_time <- Sys.time()
  print(paste("MinPts =", MinPts))
  res <- dbscan::dbscan(mdist, minPts = MinPts, eps = eps)
  saveRDS(c(res), file = paste0("results/dbscan_results_eps", eps, "_MinPts", MinPts, ".rds"))
  print(paste0("DBSCAN results saved to results/dbscan_results_eps", eps, "_MinPts", MinPts, ".rds"))
  end_time <- Sys.time()
  exec_time <- end_time - start_time
  print(paste("Execution time:", exec_time))
  # save execution time into an unique file and append on a matrix
  write.table(data.frame(MinPts = MinPts, ExecutionTime = exec_time, EPS = eps), file = "results/dbscan_execution_times.csv", append = TRUE, sep = ",", col.names = !file.exists("results/dbscan_execution_times.csv"), row.names = FALSE)

}
