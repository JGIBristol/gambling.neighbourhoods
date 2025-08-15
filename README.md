# gambling-neighbourhoods

This repository contains code and data (just gambling data for now, as geographic is too large for this repo) for analyzing gambling premises in neighbourhoods.

## Proof of Concept (PoC)
The `poc.R` file is a PoC script that reads in gambling premises data and geographical data, cleans it, and performs HDBSCAN clustering.

Some of the key steps include:
1. Loading necessary libraries.
2. Reading in gambling premises and geographical data.
3. Cleaning the data by merging the two datasets. I created the function `clean_data` that merges the gambling premises data with the geographical data based on postcode.
4. Creating a spatial points data frame using the `sp::SpatialPointsDataFrame` function. I am using the WGS84 projection.
5. Generating a distance matrix using the `geosphere::distm` function.
6. Running HDBSCAN clustering on the distance matrix.


## Future Work
The idea is to create a R package that can be used in this project. The package will start by including functions for cleaning the data, generating distance matrices, and performing clustering, and adding more functionality as information is included.