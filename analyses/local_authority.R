library(leaflet)
library(htmltools)
library(sp)
library(dplyr)
library(htmlwidgets)

if (!dir.exists("local_authority/")) {
  dir.create("local_authority/")
} 

data_all <- read.csv("data/cleaned_premises_licence_data.csv", header = TRUE) 
la_codes <-unique(data_all$OSLAUA)



  
eps_values=c(1000, 1250,1500,1750,2000)
minpt_values=2

for (la_code in la_codes) {
  print(paste0("Processing Local Authority code: ", la_code))
  data <- subset_by_la_code(data_all, la_code = la_code)
  folder_la <- paste0("local_authority/", la_code, "/")
  if (!dir.exists(folder_la)) {
    dir.create(folder_la)
  }


  # 1. Make spatialpointsdataframe
  xy <- sp::SpatialPointsDataFrame(
    matrix(c(data$LONG, data$LAT),ncol=2),
    data.frame(ID=data$ID),
    proj4string=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

  saveRDS(xy,file = paste0(folder_la, "xy_",la_code, ".rds"))
  print(paste0("SpatialPointsDataFrame saved to ", folder_la, "xy_",la_code, ".rds"))


  # 2. Generate a distance matrix. (distance in meters)
  coord <- as.data.frame(sp::coordinates(xy))
  mdist <- geosphere::distm(coord)

  saveRDS(mdist,file = paste0(folder_la, "mdist_",la_code, ".rds"))
  print(paste0("Distance matrix saved to ", folder_la, "mdist_",la_code, ".rds"))

  folder_maps <- paste0("local_authority/", la_code, "/maps/")
  if (!dir.exists(folder_maps)) {
    dir.create(folder_maps)
  }  


  # 3. use dbscan and generate maps
  summary_table <- data.frame()

  for (eps in eps_values) {
    for (MinPts in minpt_values) {
      clust_file<- paste0(folder_la, "dbscan_results_eps", eps, "_MinPts", MinPts, "_", la_code, ".rds")
      print(paste("DBSCAN Generating map for eps =", eps, "and MinPts =", MinPts))
      res_db <- dbscan::dbscan(mdist, eps = eps, minPts = MinPts)
      saveRDS(c(res_db), file =clust_file)
      df_db <- create_df(res_db,xy)
      print(head(df_db))
      print(dim(df_db))
      k_db <- length(unique(df_db$cluster)) - 1
      print(k_db)
      df_null_db <- create_df_null(df_db)
      n_noise_db <- nrow(df_null_db)
      print(n_noise_db)
      # Get the convex hulls for each cluster
      cluster_hulls_db <- get_cluster_hulls(df_db)

      # summary table
      summary_table <- rbind(summary_table, data.frame(
        EPS = eps,
        MinPts = MinPts,
        Num_Clusters = k_db,
        Num_Noise_Points = n_noise_db
      ))  
      # Create the leaflet map
      map_db <-leaflet_map(cluster_hulls_db, df_db, df_null_db)
      saveWidget(map_db, file=paste0(folder_maps, "map_dbscan_eps",eps, "_MinPts", MinPts, "_", la_code, ".html"))  
    }
    
  }
  colnames(summary_table) <- c("EPS", "MinPts", "Num_Clusters", "Num_Noise_Points")
  write.csv(summary_table, file = paste0(folder_la, "dbscan_summary_", la_code, ".csv"), row.names = FALSE)
}

# remove the folders inside folder_la/maps/ that end _files

for (la_code in la_codes) {
  for(eps in eps_values) {
    maps_folder <- paste0("local_authority/", la_code, "/maps/map_dbscan_eps",eps,"_MinPts2_", la_code, "_files")

    subfolders <- list.dirs(maps_folder, recursive = TRUE, full.names = TRUE)
    unlink(maps_folder, recursive = TRUE)
    print(paste("Removed folder:", maps_folder))
  }
}


##################################################

# create matrix with stats for each LA , each eps, say cluster for each eps and minPts
df_la_clusters <- data.frame()
  
eps_values=c(1000, 1250,1500,1750,2000)
minpt_values=2
for (la_code in la_codes) {
  folder_la <- paste0("local_authority/", la_code, "/")
  xy <- readRDS(file = paste0(folder_la, "xy_",la_code, ".rds"))
  coord <- as.data.frame(sp::coordinates(xy))
  data <- subset_by_la_code(data_all, la_code = la_code)
  colnames(coord) <- c("LONG", "LAT")
  for (eps in eps_values) {
    

  clust_file<- paste0(folder_la, "dbscan_results_eps", eps, "_MinPts", minpt_values, "_", la_code, ".rds")
  df_clust <- readRDS(clust_file)
  clusters <- df_clust$cluster

  data <- cbind(data, clusters)
  data[[paste0("eps_", eps, "_clusters")]] <- clusters
  }


}