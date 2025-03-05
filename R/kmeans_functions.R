#' Perform K-means clustering
#'
#' This function performs K-means clustering on a numeric dataset.
#'
#' @param data A numeric matrix or data frame containing the data to cluster.
#' @param centers The number of clusters to create.
#' @param ... Additional parameters passed to the stats::kmeans function.
#'
#' @return A list with the following components:
#' \itemize{
#'   \item cluster: A vector of integers indicating the cluster assignment for each observation.
#'   \item centers: A matrix of cluster centers.
#'   \item totalss: The total sum of squares.
#'   \item withinss: Vector of within-cluster sum of squares, one component per cluster.
#'   \item tot.withinss: Total within-cluster sum of squares.
#'   \item betweenss: The between-cluster sum of squares.
#'   \item size: The number of observations in each cluster.
#'   \item iter: The number of iterations performed.
#' }
#'
#' @examples
#' # Generate sample data
#' set.seed(123)
#' data <- matrix(rnorm(100), ncol = 2)
#'
#' # Perform clustering
#' result <- kmeans_cluster(data, centers = 3)
#'
#' # View cluster assignments
#' table(result$cluster)
#'
#' @export
kmeans_cluster <- function(data, centers, ...) {
  # Input validation
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Data must be a matrix or data frame")
  }

  if (!all(apply(data, 2, is.numeric))) {
    stop("All columns in data must be numeric")
  }

  if (!is.numeric(centers) || centers <= 0 || centers != round(centers)) {
    stop("Centers must be a positive integer")
  }

  if (centers > nrow(data)) {
    stop("Number of clusters cannot be greater than the number of data points")
  }

  # Perform K-means clustering
  result <- stats::kmeans(data, centers = centers, ...)

  return(result)
}

#' Plot K-means clustering results
#'
#' This function creates a scatter plot of the clustered data and highlights the cluster centers.
#'
#' @param data A numeric matrix or data frame with 2 columns for plotting.
#' @param kmeans_result The result from kmeans_cluster function.
#' @param x_col Index or name of the column to use for x-axis. Defaults to 1.
#' @param y_col Index or name of the column to use for y-axis. Defaults to 2.
#' @param main Title for the plot. Defaults to "K-means Clustering Result".
#' @param point_size Size for data points. Defaults to 1.5.
#' @param center_size Size for cluster centers. Defaults to 3.
#' @param color_palette A vector of colors for the clusters. If NULL, a default palette is used.
#' @param ... Additional parameters passed to the plot function.
#'
#' @return A plot of the clustered data (invisibly returns NULL).
#'
#' @examples
#' # Generate sample data
#' set.seed(123)
#' data <- matrix(rnorm(100), ncol = 2)
#'
#' # Perform clustering
#' result <- kmeans_cluster(data, centers = 3)
#'
#' # Plot the results
#' kmeans_plot(data, result)
#'
#' # Customize the plot
#' kmeans_plot(data, result, main = "My Custom Clustering Plot",
#'             point_size = 2, center_size = 4)
#'
#' @export
kmeans_plot <- function(data, kmeans_result, x_col = 1, y_col = 2,
                        main = "K-means Clustering Result",
                        point_size = 1.5, center_size = 3,
                        color_palette = NULL, ...) {
  # Input validation
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Data must be a matrix or data frame")
  }

  if (ncol(data) < 2) {
    stop("Data must have at least 2 columns for plotting")
  }

  if (!inherits(kmeans_result, "kmeans")) {
    stop("kmeans_result must be an object of class kmeans")
  }

  # Extract plot data
  if (is.character(x_col) && is.character(y_col)) {
    x <- data[, x_col]
    y <- data[, y_col]
    x_name <- x_col
    y_name <- y_col
  } else {
    x <- data[, x_col]
    y <- data[, y_col]
    x_name <- colnames(data)[x_col]
    y_name <- colnames(data)[y_col]
  }

  # Set up color palette
  k <- length(unique(kmeans_result$cluster))
  if (is.null(color_palette)) {
    # Use a predefined color palette
    default_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                        "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")
    color_palette <- default_colors[1:min(k, length(default_colors))]
  }

  # Create the plot
  plot(x, y, col = color_palette[kmeans_result$cluster],
       pch = 20, cex = point_size,
       main = main,
       xlab = x_name,
       ylab = y_name,
       ...)

  # Add cluster centers
  if (is.character(x_col) && is.character(y_col)) {
    centers_x <- kmeans_result$centers[, x_col]
    centers_y <- kmeans_result$centers[, y_col]
  } else {
    centers_x <- kmeans_result$centers[, x_col]
    centers_y <- kmeans_result$centers[, y_col]
  }

  graphics::points(centers_x, centers_y,
                   pch = 8, cex = center_size, lwd = 2, col = "black")

  # Add legend
  legend_text <- paste("Cluster", 1:k)
  graphics::legend("topright", legend = legend_text,
                   col = color_palette[1:k], pch = 20, cex = 0.8)

  # Return invisibly
  invisible(NULL)
}
