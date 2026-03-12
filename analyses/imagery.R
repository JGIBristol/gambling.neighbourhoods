



# download and save google strret view figures for each cluster centroid (non-noise points only)
folder_images <- paste0("local_authority/", la_code[i], "/images/")
if (!dir.exists(folder_images)) {
  dir.create(folder_images)
}

# Bristol
eps=1000
MinPts=2
la_code="E06000023"

data <- subset_by_la_code(data_all, la_code = la_code)
folder_la <- paste0("local_authority/", la_code, "/")
clust_file<- paste0(folder_la, "dbscan_results_eps", eps, "_MinPts", MinPts, "_", la_code, ".rds")

df_db <- readRDS(clust_file)

download_gsv_images(df_db, data,folder_images, api_key ="AIzaSyD0XmXjdmHUdn0DG2QHKwHsGTpTh0GMrL4", destfolder = folder_images)

download_gsv_images <- function(df, data, folder, api_key, heading=90, size="640x640", destfolder=NULL) {
  for (i in 1:length(df$cluster)) {
    if (df$cluster[i] != 0) {  # Skip noise points
      lat <- data$LAT[i]
      lon <- data$LONG[i]
      url <- paste0("https://maps.googleapis.com/maps/api/streetview?location=", 
                    lat, ",", lon, 
                    "&heading=", heading, "&size=", size, "&key=", api_key)
      destfile <- paste0(destfolder, "cluster_", df$cluster[i], "_ID_", data$ID[i], ".jpg")
      download.file(url, destfile, mode = "wb")
      print(paste("Downloaded image for cluster", df$cluster[i], "ID", data$ID[i]))
    }
  }
}

# Example URL:


# https://maps.googleapis.com/maps/api/streetview?location=51.456271,-2.606096&heading=0&size=640x640&key= AIzaSyD0XmXjdmHUdn0DG2QHKwHsGTpTh0GMrL4




#########################
library(SegmentR)
library(imager)
library(RColorBrewer)
SegmentR::setup_conda_environment()


la_code="E06000023"
folder_la <- paste0("local_authority/", la_code, "/")
file_name <- paste0(folder_la, "images/cluster_6_ID_483.jpg")
img <- load.image(file_name)
plot(img)
SegmentR::run_grounded_segmentation(file_name, labels = NULL,  output_plot = paste0(folder_la, "images/segments_cluster_6_ID_483/"),output_json = paste0(folder_la, "images/segments_cluster_6_ID_483/"))







run_grounded_segmentation <- function(image, labels, n_segments, compactness, color_space, save_segments = FALSE, output_folder = NULL) {
  segments <- superpixels(image, method = "slic", n_segments = n_segments, compactness = compactness, color_space = color_space)
  
  if (save_segments && !is.null(output_folder)) {
    dir.create(output_folder, showWarnings = FALSE)
    for (i in 1:n_segments) {
      segment_mask <- segments == i
      segment_image <- image * as.cimg(segment_mask)
      save.image(segment_image, file.path(output_folder, paste0("segment_", i, ".png")))
    }
  }
  
  return(segments)
}



https://api.geoapify.com/v1/geocode/search?text=11%20Rue%20Grenette%2C%2069002%20Lyon%2C%20France&apiKey=YOUR_API_KEY



-2.5916689991523167


51.457397068644184


https://maps.googleapis.com/maps/api/streetview?location=51.457397068644184,-2.5916689991523167&heading=0&size=640x640&key= AIzaSyD0XmXjdmHUdn0DG2QHKwHsGTpTh0GMrL4