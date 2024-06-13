# evolSOM

## About 

evolSOM can cluster genes and/or phenotypic traits according to expression patterns.
Map genes and/or phenotypic traits in different species to evaluate conservation. 
The general steps for using evolSOM are as follows:

1.  Import data and perform scaling.

2.  Construct a reference SOM (Self-Organizing Map).

3.  Map features within the reference SOM.

4.  Characterize displacement types.

5.  Create a network dataframe to quantify and visualize displacements.

6.  Generate displacement plots to gain insights into the data.


# Installation

```{r setup}
#devtools::install_github("sanprochetto/evolSOM")
library(evolSOM)
```

## Vignette

See: vignettes/my-vignette.Rmd
This vignette details some of the most important functions of the 
evolSOM package and the overall process to analyse features displacement.
