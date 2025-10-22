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

# setwd("/home/ar17162/Documents/gambling_neighbourhoods")
# read geo data
gc <- read.csv("data/premises-licence-register_06082025.csv", header = TRUE)
geo <- read.csv("data/ONSPD_Online_Latest_Centroids_1060347089026503474.csv", header = TRUE)

# #' Function to clean data by merging gc_data with geo_data
# #' @param gc_data Data frame containing gambling premises data
# #' @param geo_data Data frame containing geographical data
# #' @return Merged data frame with cleaned and relevant columns
# #' @examples
# #' clean_data(gc_data, geo_data)
# clean_data <- function(gc_data, geo_data) {
#   # Assuming gc and geo are data frames with appropriate columns
#   #  rename columns for clarity 
#   geo_data["Postcode"] <- geo_data$PCD
#   # select relevant columns
#   geo_data <- geo_data[, c("Postcode", "LAT", "LONG", "IMD", "LSOA21")]
#   #  remove spaces between postcode elements
#   geo_data$Postcode <- gsub(" ", "", geo_data$Postcode)
#   gc_data$Postcode <- gsub(" ", "", gc_data$Postcode)
#   # add ID column to gc_data
#   gc_data$ID <- seq_len(nrow(gc_data))
#   #  subset geo data when Postcode matches
#   geo_data <- geo_data[geo_data$Postcode %in% gc_data$Postcode, ]
#   # merge gc_data with geo_data on Postcode
#   merged_data <- merge(gc_data, geo_data, by = "Postcode", all.x = TRUE)
#   # some postcodes were wrongly formatted. Around 17 rows
#   # Some more cleaning is needed (TODO)
#   #  But for now I will remove the rows with NA in Lat and Long
#   merged_data <- merged_data[which(merged_data$LAT!="NA"),]
#   # return the cleaned and merged data
#   return(merged_data)
# }

data <- clean_data(gc,geo)
saveRDS(data,file = "data/premises_cleaned.rds")

# 1. Make spatialpointsdataframe
xy <- sp::SpatialPointsDataFrame(
  matrix(c(data$LONG, data$LAT),ncol=2),
  data.frame(ID=data$ID),
  proj4string=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

saveRDS(xy,file = "data/xy.rds")
print("SpatialPointsDataFrame saved to data/xy.rds")


# 2. Generate a distance matrix. (distance in meters)
coord <- as.data.frame(sp::coordinates(xy))
mdist <- geosphere::distm(coord)

saveRDS(mdist,file = "data/mdist.rds")
print("Distance matrix saved to data/mdist.rds")
# 3. Run HDBSCAN clustering

# coord <- as.data.frame(sp::coordinates(xy))
# wss <- (nrow(coord)-1)*sum(apply(coord,2,var))

# K=50
# for (i in 2:K) wss[i] <- sum(kmeans(coord,centers = i)$withinss)

# pdf("figures/elbow_plot.pdf")
# plot(1:K, wss, type="b", xlab="K",
#      ylab="Within groups sum of squares")
# dev.off()

# HDBSCAN clustering
# Using Sys.time to measure execution time
# trying with minPts = 10


for (MinPts in c(2:5,10)) {
  Sys.time()
  print(paste("MinPts =", MinPts))
  res <- dbscan::hdbscan(mdist, minPts = MinPts, verbose = TRUE)
  saveRDS(c(res), file = paste0("results/hdbscan_results_", MinPts, ".rds"))
  print(paste0("HDBSCAN results saved to results/hdbscan_results_", MinPts, ".rds"))
  Sys.time()
}
n_points <- 8761
 
#
# Testing optimal minPts for HDBSCAN on a distance matrix with 8761 points
library(ggplot2)
library(gridExtra)

# Define range of minPts to test
minPts_range <- 4:15
results <- data.frame(
  minPts = integer(),
  n_clusters = integer(),
  n_noise = integer(),
  noise_percent = numeric(),
  largest_cluster = integer(),
  runtime_mins = numeric()
)

# Loop through different minPts values
for (minPts in minPts_range) {
  print(paste("Testing minPts =", minPts))
  
  # Run HDBSCAN with timing
  start_time <- Sys.time()
  res <- hdbscan(mdist, minPts = minPts)
  end_time <- Sys.time()
  runtime <- as.numeric(difftime(end_time, start_time, units = "mins"))
  
  # Calculate metrics
  n_clusters <- max(res$cluster)
  n_noise <- sum(res$cluster == 0)
  noise_percent <- n_noise / length(res$cluster) * 100
  
  # Calculate size of largest cluster
  if (n_clusters > 0) {
    cluster_sizes <- table(res$cluster[res$cluster > 0])
    largest_cluster <- max(cluster_sizes)
  } else {
    largest_cluster <- 0
  }
  
  # Store results
  results <- rbind(results, data.frame(
    minPts = minPts,
    n_clusters = n_clusters,
    n_noise = n_noise,
    noise_percent = noise_percent,
    largest_cluster = largest_cluster,
    runtime_mins = runtime
  ))
  
  # Save HDBSCAN result
  saveRDS(res, file = paste0("results/hdbscan_results_", minPts, ".rds"))
}

# Create evaluation plots
p1 <- ggplot(results, aes(x = minPts, y = n_clusters)) +
  geom_line() + geom_point() + theme_minimal() +
  labs(title = "Number of clusters vs minPts")

p2 <- ggplot(results, aes(x = minPts, y = noise_percent)) +
  geom_line() + geom_point() + theme_minimal() +
  labs(title = "Percentage of noise points vs minPts")

p3 <- ggplot(results, aes(x = minPts, y = largest_cluster)) +
  geom_line() + geom_point() + theme_minimal() +
  labs(title = "Size of largest cluster vs minPts")

# Combine and save plots
combined_plot <- grid.arrange(p1, p2, p3, ncol = 2)
ggsave("figures/hdbscan_parameter_tuning.pdf", combined_plot, width = 10, height = 8)

# Save evaluation results
write.csv(results, "results/minpts_evaluation.csv", row.names = FALSE)
print(results)