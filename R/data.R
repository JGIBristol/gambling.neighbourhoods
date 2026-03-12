#' Clean and merge gambling premises data with geographical data
#' @param gc_data Data frame containing gambling premises data
#' @param geo_data Data frame containing geographical data
#' @return A merged data frame with cleaned and relevant columns
#' @import dplyr
#' @export
clean_data <- function(gc_data, geo_data) {
  # Assuming gc and geo are data frames with appropriate columns
  #  rename columns for clarity 
  geo_data["Postcode"] <- geo_data$PCD
  # select relevant columns
  geo_data <- geo_data[, c("Postcode", "LAT", "LONG", "IMD", "LSOA21", "MSOA21", "OSLAUA")]
  #  remove spaces between postcode elements
  geo_data$Postcode <- gsub(" ", "", geo_data$Postcode)
  gc_data$Postcode <- gsub(" ", "", gc_data$Postcode)
  #  all gc values in capital letters
  gc_data$Postcode <- toupper(gc_data$Postcode)
  # add ID column to gc_data
  gc_data$ID <- seq_len(nrow(gc_data))
  #  subset geo data when Postcode matches
  geo_data <- geo_data[geo_data$Postcode %in% gc_data$Postcode, ]
  # merge gc_data with geo_data on Postcode
  merged_data <- merge(gc_data, geo_data, by = "Postcode", all.x = TRUE)
  # some postcodes were wrongly formatted. Around 17 rows
  #  Remove the rows with NA in Lat and Long
  merged_data <- merged_data[which(merged_data$LAT!="NA"),]
  # return the cleaned and merged data
  return(merged_data)
}


#' Subset data frame by postcode area
#' @param data Data frame containing gambling premises data with a Postcode column
#' @param postcode_area String to filter postcodes by area (e.g., "BS" for Bristol)
#' @return A subset of the data frame with postcodes starting with the specified area
#' @import dplyr
#' @export
subset_by_postcode <- function(data, postcode_area) { 
  if (length(postcode_area)>1) {
    data_sub <- data[0, ]
    for (area in postcode_area) {
      data_sub <- rbind(data_sub, data[grep(paste0("^", area, "[0-9]"),data$Postcode), ])
    } 
  } else {
    data_sub <- data[grep(paste0("^", postcode_area, "[0-9]"), data$Postcode), ]
    }
  return(data_sub)
}

#' Subset by Local Authority code
#' @param data Data frame containing gambling premises data with an OSLAUA column
#' @param la_code String representing the Local Authority code to filter by (e.g., "E06000023" for Bristol)
#' @return A subset of the data frame with rows matching the specified Local Authority code
#' @import dplyr
#' @export
subset_by_la_code <- function(data, la_code) { 
  data_sub <- data[data$OSLAUA == la_code, ]
  return(data_sub)
}


