# Proof of Concept (PoC)
The `poc.R` file is a PoC script that reads in gambling premises data and geographical data, cleans it, and performs HDBSCAN clustering.

Some of the key steps include:
1. Loading necessary libraries.
2. Reading in gambling premises and geographical data.
3. Cleaning the data by merging the two datasets. I created the function `clean_data` that merges the gambling premises data with the geographical data based on postcode.
4. Creating a spatial points data frame using the `sp::SpatialPointsDataFrame` function. I am using the WGS84 projection.
5. Generating a distance matrix using the `geosphere::distm` function.
6. Running HDBSCAN clustering on the distance matrix using the `dbscan::hdbscan` function for different values of MinPts (2, 3, 4, 5, 10).
7. Saving the clustered data as .rds files for each MinPts value.