[![Build Status](https://travis-ci.org/riatelab/MTA.svg?branch=master)](https://travis-ci.org/riatelab/MTA)

# Multiscalar Territorial Analysis

*Multiscalar territorial analysis based on various contexts.*   


## Main functions :
- `gdev()`: general deviation between regional ratios and a ratio of reference.  
- `tdev()`: territorial deviation between regional ratios and ratios of an aggregated level.  
- `sdev()`: spatial deviation between regional ratios and ratios of neighborhing regions. 
- `bidev()`: multiscalar typology based on 2 deviations.  
- `mst()`: multiscalar typology based on the 3 deviations.  
- `mas()`: multiscalar absolute synthesis, total amount of redistributions based on the three deviations.  
- `map_bidev()`: creating bidev and parameters for producing a map based on it.
- `map_mst()`: creating mst and parameters for producing a map based on it.
- `plot_bidev()`: creating a plot for visualizing bidev results.
- `plot_mst()`: creating a plot for visualizing mst results.


## Installation

* Stable version
```{r}
install.packages("MTA")
```

* Development version on GitHub
```{r}
require(remotes)
install_github("riatelab/MTA")
```

## Community Guidelines

One can contribute to the package through [pull requests](https://github.com/riatelab/MTA/pulls) and report issues or ask questions [here](https://github.com/riatelab/MTA/issues).
