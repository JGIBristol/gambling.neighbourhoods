gc <- read.csv("data/premises-licence-register_06082025.csv", header = TRUE)
geo <- read.csv("data/ONSPD_Online_Latest_Centroids_1060347089026503474.csv", header = TRUE)


#  bristol Local Authority code: E06000023
data <- clean_data(gc,geo)
# 
bristol_data <- subset_by_la_code(data, la_code = "E06000023")
# write.csv(bristol_data, file="data/bristol_premises_cleaned.csv", row.names = FALSE)


# 1. Make spatialpointsdataframe
xy_bristol <- sp::SpatialPointsDataFrame(
  matrix(c(bristol_data$LONG, bristol_data$LAT),ncol=2),
  data.frame(ID=bristol_data$ID),
  proj4string=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

saveRDS(xy_bristol,file = "data/xy_bristolLA.rds")
print("SpatialPointsDataFrame saved to data/xy_bristolLA.rds")


# 2. Generate a distance matrix. (distance in meters)
coord <- as.data.frame(sp::coordinates(xy_bristol))
mdist <- geosphere::distm(coord)

saveRDS(mdist,file = "data/mdist_bristolLA.rds")
print("Distance matrix saved to data/mdist_bristolLA.rds")

# 3. Run HDBSCAN with different parameters and save results to new folder
# Create results folder if it doesn't exist

if (!dir.exists("testing/results")) {
  dir.create("testing/results")
}  
folder <- "testing/"
clust_file<- paste0(folder,"results/bristol_hdbscan_results_eps", eps, "_MinPts", MinPts, "_bristolLA.rds")
eps=750
for (MinPts in c(2:5)) {
  start_time <- Sys.time()
  print(paste("MinPts =", MinPts))
  res <- dbscan::hdbscan(mdist, minPts = MinPts, cluster_selection_epsilon = eps, verbose = TRUE)
  saveRDS(c(res), file = clust_file)
  print(paste0("HDBSCAN results saved to results/hdbscan_results_eps_", eps, "_MinPts_", MinPts, "_bristolLA.rds"))
  end_time <- Sys.time()
  exec_time <- end_time - start_time
  print(paste("Execution time:", exec_time))
  # save execution time into an unique file and append on a matrix
  write.table(data.frame(MinPts = MinPts, ExecutionTime = exec_time, EPS = eps), file = paste0(folder, "results/hdbscan_execution_times_bristolLA.csv"), append = TRUE, sep = ",", col.names = !file.exists(paste0(folder, "results/hdbscan_execution_times_bristolLA.csv")), row.names = FALSE)

}



# 4. Create leaflet maps for each result
for (MinPts in c(2:5)) {
    print(paste("Generating map for MinPts =", MinPts))
    res <- readRDS(clust_file)
    df <- create_df(res,xy_bristol)
    print(head(df))
    print(dim(df))

    k <- length(unique(df$cluster)) - 1
    print(k)
    df_null <- create_df_null(df)
    n_noise <- nrow(df_null)
    print(n_noise)
    # Get the convex hulls for each cluster
    cluster_hulls <- get_cluster_hulls(df)
    # Create the leaflet map
    map <-leaflet_map(cluster_hulls, df, df_null)
    saveWidget(map, file=paste0(folder, "results/map_hdbscan_eps",eps, "_MinPts", MinPts, "_bristolLA.html"))
    map
}


# 4. use dbscan instead of hdbscan for comparison
eps=1000
clust_file<- paste0(folder,"results/bristol_dbscan_results_eps", eps, "_MinPts", MinPts, "_bristolLA.rds")

for (MinPts in c(2:5)) {
  print(paste("DBSCAN Generating map for MinPts =", MinPts))
res_db <- dbscan::dbscan(mdist, eps = eps, minPts = MinPts)
saveRDS(c(res_db), file =clust_file)
df_db <- create_df(res_db,xy_bristol)
print(head(df_db))
print(dim(df_db))
k_db <- length(unique(df_db$cluster)) - 1
print(k_db)
df_null_db <- create_df_null(df_db)
n_noise_db <- nrow(df_null_db)
print(n_noise_db)
# Get the convex hulls for each cluster
cluster_hulls_db <- get_cluster_hulls(df_db)
# Create the leaflet map
map_db <-leaflet_map(cluster_hulls_db, df_db, df_null_db)
saveWidget(map_db, file=paste0(folder, "results/map_dbscan_eps",eps, "_MinPts", MinPts, "_bristolLA.html"))
map_db
}
