
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NYgrid

<!-- badges: start -->
<!-- badges: end -->

🚧 WIP 🚧

The NYgrid R package provides access to a set of high-frequency (hourly)
time series dataset, describing the demand generation of electricity by
the New York Independent System Operator.

Data source: [EIA API](https://www.eia.gov/opendata/)

The package goals:

-   Supporting material for the Applied Time Series Analysis and
    Forecasting book.
-   Example for setting live data automation with R, Github Actions, and
    Docker

## Installation

You can install the development version of NYgrid from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("RamiKrispin/NYgrid")
```

## Data

The electricity demand generation dataset includes eleven time series,
each representing a New York Independent System Operator region.

``` r
library(NYgrid)
```

TODO

-   Add data dictionary
-   Simple example
-   Data architecture

## Data architecture

<img src="man/figures/data architecture.png" width="100%" />
