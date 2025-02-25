# Test file for kmeans_functions

test_that("kmeans_cluster handles basic functionality correctly", {
  set.seed(123)  # For reproducibility

  # Create simple test data with clear clusters
  group1 <- matrix(rnorm(50, mean = 0, sd = 0.5), ncol = 2)
  group2 <- matrix(rnorm(50, mean = 5, sd = 0.5), ncol = 2)
  group3 <- matrix(rnorm(50, mean = -5, sd = 0.5), ncol = 2)
  test_data <- rbind(group1, group2, group3)

  # Test with k=3
  result <- kmeans_cluster(test_data, centers = 3, nstart = 25)

  # Tests for return value structure
  expect_s3_class(result, "kmeans")
  expect_named(result, c("cluster", "centers", "totss", "withinss", "tot.withinss", "betweenss", "size", "iter", "ifault"))
  expect_length(result$cluster, nrow(test_data))
  expect_equal(nrow(result$centers), 3)
  expect_equal(ncol(result$centers), ncol(test_data))

  # Test that we have 3 clusters
  expect_length(unique(result$cluster), 3)

  # Test that betweenss + tot.withinss = totss
  expect_equal(result$betweenss + result$tot.withinss, result$totss,
               tolerance = 1e-10)
})

test_that("kmeans_cluster validates inputs correctly", {
  # Test with invalid data types
  expect_error(kmeans_cluster("not a matrix", 3), "must be a matrix or data frame")
  expect_error(kmeans_cluster(matrix(c("a", "b", "c", "d"), ncol = 2), 2),
               "must be numeric")

  # Test with invalid centers
  test_data <- matrix(rnorm(100), ncol = 2)
  expect_error(kmeans_cluster(test_data, -1), "must be a positive integer")
  expect_error(kmeans_cluster(test_data, 0), "must be a positive integer")
  expect_error(kmeans_cluster(test_data, 2.5), "must be a positive integer")

  # Test with too many centers
  expect_error(kmeans_cluster(test_data, nrow(test_data) + 1),
               "Number of clusters cannot be greater than the number of data points"
)
})

test_that("kmeans_plot validates inputs correctly", {
  set.seed(123)
  test_data <- matrix(rnorm(100), ncol = 2)
  result <- kmeans_cluster(test_data, centers = 3)

  # Test with invalid data
  expect_error(kmeans_plot("not a matrix", result),
               "must be a matrix or data frame")

  # Test with insufficient columns
  expect_error(kmeans_plot(matrix(1:5, ncol = 1), result),
               "at least 2 columns")

  # Test with invalid kmeans_result
  expect_error(kmeans_plot(test_data, "not a kmeans result"),
               "must be an object of class kmeans")
})

test_that("kmeans_plot handles column selection correctly", {
  set.seed(123)

  # Create test data frame with named columns
  test_df <- data.frame(
    feature1 = rnorm(50),
    feature2 = rnorm(50),
    feature3 = rnorm(50)
  )

  result <- kmeans_cluster(test_df, centers = 2)

  # Test with numeric indices
  expect_silent(kmeans_plot(test_df, result, x_col = 1, y_col = 2))
  expect_silent(kmeans_plot(test_df, result, x_col = 2, y_col = 3))

  # Test with column names (if using a data frame)
  if (is.data.frame(test_df)) {
    expect_silent(kmeans_plot(test_df, result,
                              x_col = "feature1", y_col = "feature2"))
    expect_silent(kmeans_plot(test_df, result,
                              x_col = "feature2", y_col = "feature3"))
  }
})

test_that("kmeans_plot returns invisibly", {
  set.seed(123)
  test_data <- matrix(rnorm(100), ncol = 2)
  result <- kmeans_cluster(test_data, centers = 3)

  # Check that the function returns invisibly
  expect_invisible(kmeans_plot(test_data, result))
})
