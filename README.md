# evolSOM

## About 

evolSOM cluster genes and/or phenotypic traits according to expression patterns.
Map genes and/or phenotypic traits in different species to evaluate conservation
along evolution. 
The general steps are as follow: 
1- Import data and scaling 
2- Build a reference species-specific SOM 
3- Map features in reference SOM 
4- Characterize displacement types 
5- Create network dataframe to count and graph displacements 
6- Plot the displacements

# Installation

```{r setup}
#devtools::install_github("sanprochetto/evolSOM")
library(evolSOM)
```

## Vignette

See: vignettes/my-vignette.Rmd
This vignette details some of the most important functions of the 
evolSOM package and the overall process to analyse features displacement.
