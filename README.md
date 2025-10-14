<!-- badges: start -->
  [![R-CMD-check](https://github.com/JGIBristol/gambling.neighbourhoods/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JGIBristol/gambling.neighbourhoods/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->
  
# gambling-neighbourhoods
The *Gambling neighbourhoods project* aims to  produce a typology of gambling neighbourhoods across the UK.  It does  aims to identify clusters of gambling premises in urban environments, using clustering algorithms such as DBSCAN, and visualize the results using interactive maps and ultimately create a Shiny dashboard to plot these typologies.

Previous research has indicated the pervasiveness of gambling outlets as they became a staple of UK high streets. They can be found in almost any type of high street: from high streets in expensive, affluent areas in close proximity to luxurious retail shops, to high streets in deprived town centres with hardly any retail opportunities. By creating such a typology of gambling neighbourhoods this project will expose the location strategies of the gambling industry and the functions these concentrations of gambling shops perform. The outputs of this project will also support Local Authorities, which need to develop policy frameworks for managing gambling shop licence applications as well as mitigation strategies against harmful gambling behaviours.


## R package (gambling.neighbourhoods) 
The R package `gambling.neighbourhoods` is in the early stages of development to provide functions for analyzing and visualizing gambling premises in urban environments.

The package will start by including functions for cleaning the data, generating distance matrices, and performing clustering, visualization, and adding more functionality as information is included.

You can install the development version of the package from GitHub with:

```R
if (!requireNamespace("devtools", quietly = TRUE)){
  install.packages("devtools")
}
devtools::install_github("JGIBristol/gambling.neighbourhoods")
```
More information about the package can be found in the [gambling.neighbourhoods documentation](https://JGIBristol.github.io/gambling.neighbourhoods/).

## Proof of Concept (PoC)
The proof  of concept is available in the [poc](poc) directory, demonstrating the use of clustering algorithms to identify clusters of gambling premises.
