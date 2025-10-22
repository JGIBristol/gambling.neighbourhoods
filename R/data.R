#' Clean and merge gambling premises data with geographical data
#' @param gc_data Data frame containing gambling premises data
#' @param geo_data Data frame containing geographical data
#' @return A merged data frame with cleaned and relevant columns)
#' @import dplyr
#' @export
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

#' Create a Leaflet map with cluster hulls and points
#' @param cluster_hulls A list of data frames, each containing the convex hull for a cluster
#' @param df Data frame containing clustered points with columns LONG, LAT, cluster
#' @param df_null Data frame containing unclustered points with columns LONG, LAT, ID
#' @return A leaflet map object
#' @import leaflet
#' @export
leaflet_map <- function(cluster_hulls, df, df_null) {
  m <-leaflet() %>% addTiles() 
    # Add polygons for each cluster hull
  for (c in names(cluster_hulls)) {
    hull <- cluster_hulls[[c]]
    # Count the number of points in this cluster
    cluster_count <- sum(df$cluster == c)
    m <- m %>% 
      addPolygons(
        lng = hull$LONG, 
        lat = hull$LAT, 
        color = "blue",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.2,
        popup = paste("Cluster ID:", c, "<br>", "Number of premises:", cluster_count)
      )
  }
  m <- m %>%
    # Add circle markers for individual points
    addCircleMarkers(
      data = df,
      lng = ~LONG,
      lat = ~LAT,
      color = "blue",
      popup = ~paste("Cluster ID:", cluster,"<br>", "GP ID:", ID),
      radius = 3,
      stroke = FALSE,
      fillOpacity = 0.8
    ) %>%
    addCircleMarkers(
      data = df_null,
      lng = ~LONG,
      lat = ~LAT,
      color = "black",
      popup = ~paste("Unclustered: GP ID", ID),
      radius = 3,
      stroke = FALSE,
      fillOpacity = 1.0
    )
  return(m)
}

#' Create a data frame from clustering results and spatial data
#' @param res Clustering results
#' @param xy Spatial data (points)
#' @return A data frame with coordinates and cluster information
#' @import sp
#' @import dplyr
#' @export
create_df <- function(res, xy) {
  df <- as.data.frame(sp::coordinates(xy))
  df$cluster <- as.factor(res$cluster)
  df$ID <-  xy@data$ID
  df <- dplyr::rename(df, LONG = coords.x1, LAT = coords.x2) %>%
    group_by(cluster) %>%
    mutate(Pop = ifelse(cluster == 0, 0, n())) %>%
    ungroup()
  return(df)
}

#' create a data frame containing the unnclustered points
#' @param df Data frame containing clustered points with columns LONG, LAT, cluster, ID
#' @return A data frame with unclustered points (cluster == 0)
#' @import dplyr
create_df_null <- function(df) {
  df_null <- df %>%
    filter(cluster == 0)
  return(df_null)
}

#' Get convex hulls for each cluster
#' @param df Data frame containing clustered points with columns LONG, LAT, cluster
#' @return A list of data frames, each containing the convex hull for a cluster
#' @import dplyr
#' @import sp
get_cluster_hulls <- function(df) {
  clusters <- unique(df$cluster[df$cluster != 0])
  hulls <- list()
  for (c in clusters) {
    cluster_points <- df[df$cluster == c, c("LONG", "LAT")]
    if (nrow(cluster_points) >= 3) {
      hull_indices <- chull(cluster_points)
      hull_indices <- c(hull_indices, hull_indices[1])
      hulls[[as.character(c)]] <- cluster_points[hull_indices, ]
    }
  }
  return(hulls)
}