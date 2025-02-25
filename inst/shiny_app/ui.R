library(shiny)

fluidPage(
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
