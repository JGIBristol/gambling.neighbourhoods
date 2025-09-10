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


# read geo data
gc <- read.csv("data/premises-licence-register_06082025.csv", header = TRUE)

geo <- read.csv("data/ONSPD_Online_Latest_Centroids_1060347089026503474.csv", header = TRUE)



#' Function to clean data by merging gc_data with geo_data
#' @param gc_data Data frame containing gambling premises data
#' @param geo_data Data frame containing geographical data
#' @return Merged data frame with cleaned and relevant columns
#' @examples
#' clean_data(gc_data, geo_data)
clean_data <- function(gc_data, geo_data) {
  # Assuming gc and geo are data frames with appropriate columns
  #  rename columns for clarity 
  geo_data["Postcode"] <- geo_data$PCD
  # select relevant columns
  geo_data <- geo_data[, c("Postcode", "LAT", "LONG", "IMD", "LSOA21")]
  #  remove spaces between postcode elements
  geo_data$Postcode <- gsub(" ", "", geo_data$Postcode)
  gc_data$Postcode <- gsub(" ", "", gc_data$Postcode)
  # add ID column to gc_data
  gc_data$ID <- seq_len(nrow(gc_data))
  #  subset geo data when Postcode matches
  geo_data <- geo_data[geo_data$Postcode %in% gc_data$Postcode, ]
  # merge gc_data with geo_data on Postcode
  merged_data <- merge(gc_data, geo_data, by = "Postcode", all.x = TRUE)
  # some postcodes were wrongly formatted. Around 17 rows
  # Some more cleaning is needed (TODO)
  #  But for now I will remove the rows with NA in Lat and Long
  merged_data <- merged_data[which(merged_data$LAT!="NA"),]
  # return the cleaned and merged data
  return(merged_data)
}

data <- clean_data(gc,geo)


# 1. Make spatialpointsdataframe
xy <- sp::SpatialPointsDataFrame(
  matrix(c(data$LAT, data$LONG),ncol=2),
  data.frame(ID=data$ID),
  proj4string=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))


# 2. Generate a distance matrix.
mdist <- geosphere::distm(xy)


# 3. Run HDBSCAN clustering

# elbow test for optimal number of clusters
coord <- as.data.frame(sp::coordinates(xy))
wss <- (nrow(coord)-1)*sum(apply(coord,2,var))

K=50
for (i in 2:K) wss[i] <- sum(kmeans(coord,centers = i)$withinss)

pdf("figures/elbow_plot.pdf")
plot(1:K, wss, type="b", xlab="K",
     ylab="Within groups sum of squares")
dev.off()

# HDBSCAN clustering
# Using Sys.time to measure execution time
# trying with minPts = 10
Sys.time()
res <- dbscan::hdbscan(mdist, minPts = 10, verbose = TRUE)
Sys.time()

saveRDS(c(res,mdist), file = "data/hdbscan_results.rds")



pdf("figures/hdbscan_hullplot.pdf")
dbscan::hullplot(mdist, res, main = "HDBSCAN Clustering of Gambling Premises (minPts = 10)")
dev.off()

pdf("figures/hdbscan_clplot.pdf")
dbscan::clplot(mdist, res, main = "HDBSCAN Clustering of Gambling Premises (minPts = 10)")

dev.off()
