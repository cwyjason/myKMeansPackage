#' Launch K-means Clustering Shiny App
#'
#' This function launches the interactive Shiny application for K-means clustering.
#'
#' @return A Shiny app object
#' @export
#'
#' @examples
#' \dontrun{
#' run_kmeans_app()
#' }
run_kmeans_app <- function() {
  app_dir <- system.file("shiny_app", package = "myKMeansPackage")
  if (app_dir == "") {
    stop("Could not find Shiny app directory. Try reinstalling the package.")
  }
  shiny::runApp(app_dir)
}
