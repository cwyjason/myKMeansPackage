# myKMeansPackage

[![R-CMD-check](https://github.com/cwyjason/myKMeansPackage/workflows/R-CMD-check/badge.svg)](https://github.com/cwyjason/myKMeansPackage/actions)

## Overview

`myKMeansPackage` is an R package for performing K-means clustering with interactive visualization. It provides functions for clustering data and visualizing the results, along with a Shiny app for interactive exploration.

## Installation

```r
# Install devtools if not installed
install.packages("devtools")

# Install the package from GitHub
devtools::install_github("YOUR_USERNAME/myKMeansPackage")
```

## Usage

### Basic Clustering

```r
library(myKMeansPackage)

# Generate random data
set.seed(123)
data <- matrix(rnorm(200), ncol = 2)

# Perform clustering
result <- kmeans_cluster(data, centers = 3)

# Plot the results
kmeans_plot(data, result)
```

### Using the Shiny App

```r
# Run the interactive Shiny app
shiny::runApp(system.file("shiny_app", package = "myKMeansPackage"))
```

## Features

- `kmeans_cluster()`: Performs K-means clustering on numeric data
- `kmeans_plot()`: Visualizes clustering results with customizable options
- Interactive Shiny app with multiple data generation options and visualization controls

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

