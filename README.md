
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mvlazy <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: AGPL
v3](https://img.shields.io/badge/License-AGPL%20v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)
<!-- badges: end -->

**mvlazy** provides quick, pre-configured visualizations for common
macroeconomic analysis workflows within the
[macroverse](https://github.com/macroverse-r/macroverse) ecosystem. It
offers ready-to-use templates for balance of payments, real effective
exchange rates, credit analysis, trade patterns, and other key economic
indicators.

## Installation

You can install the development version of mvlazy from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("macroverse-r/mvlazy")
```

## Overview

The mvlazy package streamlines the creation of complex economic
visualizations through a single interface. Instead of manually combining
multiple data series and configuring plots, you can generate
publication-ready figures with minimal code.

### Main Function

The package provides one main function:

- `ml_quick()`: Generate pre-configured economic visualizations

### Available Graph Types

mvlazy supports the following analysis types:

- **bop**: Balance of Payments decomposition
- **reer**: Real Effective Exchange Rate analysis
- **credit**: Credit and lending indicators
- **trade**: Trade balance and patterns
- **fiscal**: Government finance indicators
- **investment**: Investment flows and stocks
- **integration**: Economic integration measures
- **labour**: Labor market indicators
- **bubble**: Asset price and credit bubbles
- **impact**: Economic impact assessments
- **production**: Production and output measures
- **monetary**: Monetary policy indicators

## Examples

### Balance of Payments Analysis

``` r
library(mvlazy)

# Simple balance of payments visualization for the United States
ml_quick("USA", c(2010, 2023), graph = "bop")

# Compare multiple countries with GDP normalization
ml_quick(c("USA", "CHN", "DEU"), c(2010, 2023), 
         graph = "bop",
         theme = list(normalization = "GDP"))
```

### Real Effective Exchange Rate

``` r
# REER analysis with key event markers
key_dates <- rbind(
  c("GFC", "2008-09-15"),
  c("COVID", "2020-03-01")
)

ml_quick("JPN", c(2000, 2023), 
         graph = "reer",
         theme = list(key_dates = key_dates))
```

### Trade Analysis

``` r
# Trade balance decomposition
ml_quick("KOR", c(2015, 2023), 
         graph = "trade",
         option = 2,  # Detailed breakdown
         theme = list(sea_adj = TRUE))  # Seasonal adjustment
```

### Custom Themes

The `theme` parameter allows extensive customization:

``` r
ml_quick("MEX", c(2010, 2023), 
         graph = "fiscal",
         theme = list(
           normalization = "GDP",      # Normalize to GDP
           sea_adj = TRUE,            # Apply seasonal adjustment
           with_title = TRUE,         # Include descriptive title
           dim = c(12, 8),           # Figure dimensions
           base_size = 14,           # Font size
           explanation = TRUE        # Include explanatory notes
         ))
```

## Options

Most graph types support multiple visualization options through the
`option` parameter:

- `option = 1`: Standard view with main components
- `option = 2`: Detailed breakdown with subcomponents
- `option = 3+`: Graph-specific alternative views

## Integration with macroverse

mvlazy builds on other macroverse packages:

- Uses **macrodata** for data access
- Leverages **pplot** for visualization engine
- Applies **isomapper** for country code handling
- Utilizes **mvcommon** for shared utilities

## Workflow Example

A typical workflow might combine multiple analyses:

``` r
library(mvlazy)

# Define countries and period
countries <- c("USA", "EUR", "CHN", "JPN")
period <- c(2015, 2023)

# Generate multiple analyses
ml_quick(countries, period, graph = "bop", filename = "bop_analysis")
ml_quick(countries, period, graph = "trade", filename = "trade_flows")
ml_quick(countries, period, graph = "credit", filename = "credit_conditions")

# Custom bubble analysis for emerging markets
ml_quick(c("BRA", "IND", "IDN", "MEX"), period, 
         graph = "bubble",
         theme = list(normalization = "GDP", dim = c(16, 10)))
```

## Development Status

mvlazy is under active development. Current priorities include:

- Expanding visualization templates
- Adding more customization options
- Improving documentation and examples
- Performance optimization for large datasets

## License

This package is part of the macroverse ecosystem and is available under
dual licensing:

- **Open Source**: AGPL-3.0 for academic and non-commercial use
- **Commercial**: Alternative licensing available for commercial
  applications

For commercial licensing inquiries, please contact the package
maintainer.

## See Also

Other macroverse packages:

- [macroverse](https://github.com/macroverse-r/macroverse): Meta-package
  for the ecosystem
- [macrodata](https://github.com/macroverse-r/macrodata): Core data
  access functionality
- [pplot](https://github.com/macroverse-r/pplot): Plotting engine
- [isomapper](https://github.com/macroverse-r/isomapper): ISO code
  mapping utilities
- [mvcommon](https://github.com/macroverse-r/mvcommon): Shared utilities
