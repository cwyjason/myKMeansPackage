library(shiny)
library(myKMeansPackage)

# Define UI
ui <- fluidPage(
  # Application title
  titlePanel("Interactive K-means Clustering"),

  # Sidebar with input controls
  sidebarLayout(
    sidebarPanel(
      # Data generation options
      h3("Data Options"),
      sliderInput("n_points", "Number of points:",
                  min = 50, max = 500, value = 200, step = 50),

      selectInput("distribution", "Data distribution:",
                  choices = c("Normal clusters" = "normal",
                              "Uniform random" = "uniform",
                              "Circular pattern" = "circle",
                              "Iris dataset" = "iris"),
                  selected = "normal"),

      # K-means options
      h3("Clustering Options"),
      sliderInput("k_clusters", "Number of clusters (k):",
                  min = 1, max = 9, value = 3),

      selectInput("distance", "Distance method:",
                  choices = c("Euclidean" = "euclidean",
                              "Manhattan" = "manhattan"),
                  selected = "euclidean"),

      numericInput("nstarts", "Number of random starts:",
                   value = 10, min = 1, max = 50),

      # If using iris dataset
      conditionalPanel(
        condition = "input.distribution == 'iris'",
        selectInput("x_var", "X variable:",
                    choices = c("Sepal.Length", "Sepal.Width",
                                "Petal.Length", "Petal.Width"),
                    selected = "Sepal.Length"),
        selectInput("y_var", "Y variable:",
                    choices = c("Sepal.Length", "Sepal.Width",
                                "Petal.Length", "Petal.Width"),
                    selected = "Sepal.Width")
      ),

      # Controls
      actionButton("regenerate", "Regenerate Data",
                   class = "btn-primary"),

      # Additional options
      checkboxInput("show_centers", "Show cluster centers", value = TRUE),
      checkboxInput("show_legend", "Show legend", value = TRUE)
    ),

    # Main panel with outputs
    mainPanel(
      # Tabs for different views
      tabsetPanel(type = "tabs",
                  tabPanel("Cluster Plot",
                           plotOutput("cluster_plot", height = "500px"),
                           h4("Clustering Statistics"),
                           verbatimTextOutput("cluster_stats")),
                  tabPanel("Help",
                           h3("How to use this app"),
                           p("This app demonstrates K-means clustering using the myKMeansPackage."),
                           p("1. Select the data distribution and number of points to generate."),
                           p("2. Choose the number of clusters (k) and other clustering parameters."),
                           p("3. Click 'Regenerate Data' to create a new dataset."),
                           p("4. View the clustering results in the plot."),
                           p("5. Examine the clustering statistics below the plot."),
                           h4("About K-means Clustering"),
                           p("K-means is an unsupervised machine learning algorithm that groups data points into k clusters based on similarity."),
                           p("The algorithm works by:"),
                           tags$ol(
                             tags$li("Randomly initializing k cluster centers"),
                             tags$li("Assigning each data point to the nearest cluster center"),
                             tags$li("Updating the cluster centers to the mean of the assigned points"),
                             tags$li("Repeating steps 2-3 until convergence")
                           ))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive expression to generate the dataset
  dataset <- reactive({
    # Depend on the button to regenerate
    input$regenerate

    # Set seed for reproducibility within a session
    isolate({
      set.seed(as.numeric(Sys.time()))
    })

    # Generate dataset based on selected distribution
    if (input$distribution == "iris") {
      # Return the iris dataset
      return(iris[, 1:4])
    }

    n <- input$n_points

    if (input$distribution == "normal") {
      # Create 3 normal clusters
      group1 <- matrix(rnorm(n/3*2, mean = 0, sd = 0.5), ncol = 2)
      group2 <- matrix(rnorm(n/3*2, mean = 5, sd = 0.8), ncol = 2)
      group3 <- matrix(rnorm(n/3*2, mean = -5, sd = 0.6), ncol = 2)
      data <- rbind(group1, group2, group3)[1:n,]

    } else if (input$distribution == "uniform") {
      # Create uniform random data
      data <- matrix(runif(n*2, -10, 10), ncol = 2)

    } else if (input$distribution == "circle") {
      # Create circular pattern
      theta <- runif(n, 0, 2*pi)
      r <- runif(n, 3, 8)  # Ring with inner radius 3, outer radius 8
      data <- matrix(c(r*cos(theta), r*sin(theta)), ncol = 2)
    }

    # Convert to data.frame for consistency
    data <- as.data.frame(data)
    colnames(data) <- c("X", "Y")
    return(data)
  })

  # Get the data for plotting (iris or generated)
  plot_data <- reactive({
    data <- dataset()

    if (input$distribution == "iris") {
      # For iris, select the two variables chosen by the user
      return(data[, c(input$x_var, input$y_var)])
    } else {
      # For generated data, use X and Y
      return(data)
    }
  })

  # Perform K-means clustering
  clusters <- reactive({
    # Get the data for clustering
    data <- plot_data()

    # Apply kmeans_cluster function from our package
    result <- kmeans_cluster(
      data,
      centers = input$k_clusters,
      nstart = input$nstarts
    )

    return(result)
  })

  # Render the cluster plot
  output$cluster_plot <- renderPlot({
    data <- plot_data()
    k_result <- clusters()

    # Get axis labels
    if (input$distribution == "iris") {
      x_col <- input$x_var
      y_col <- input$y_var
    } else {
      x_col <- 1
      y_col <- 2
    }

    # Use kmeans_plot function from our package
    kmeans_plot(
      data = data,
      kmeans_result = k_result,
      x_col = x_col,
      y_col = y_col,
      main = paste0("K-means Clustering (k=", input$k_clusters, ")"),
      point_size = 1.8,
      center_size = 4
    )
  })

  # Display clustering statistics
  output$cluster_stats <- renderPrint({
    k_result <- clusters()

    # Calculate percentage of variance explained
    variance_explained <- k_result$betweenss / k_result$totss * 100

    # Print clustering statistics
    cat("Number of clusters:", length(k_result$size), "\n")
    cat("Cluster sizes:", k_result$size, "\n\n")
    cat("Sum of squares:\n")
    cat("  Total SS (total variance):", round(k_result$totss, 2), "\n")
    cat("  Between SS (explained variance):", round(k_result$betweenss, 2), "\n")
    cat("  Within SS (unexplained variance):", round(k_result$tot.withinss, 2), "\n")
    cat("  Percentage of variance explained:", round(variance_explained, 2), "%\n\n")
    cat("Number of iterations:", k_result$iter, "\n")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
