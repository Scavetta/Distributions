dashboardPage(
  dashboardHeader(title = "Distributions"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Binomial", tabName = "binom"),
      menuItem("Normal", tabName = "norm"),
      menuItem("t", tabName = "t_dist"),
      br(),
      menuItem("Central Limit Theorem", tabName = "CLT")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("binom",
        fluidRow(
          box(
            width = 4, status = "info", solidHeader = TRUE,
            title = "Input",
            sliderInput("ptile","Number of trials (n):",
                        min = 1, max = 30, value = 2, step= 1),
            sliderInput("prob","Probability (p) of success:",
                        min = 0, max = 1, value = 0.5, step= 0.1)
            ),
          # box(
          #   width = 8, status = "info", solidHeader = TRUE,
          #   title = "Distribution",
          #   plotOutput(outputId = "dist_plot1")
          # )
          box(
            width = 8, status = "info", solidHeader = TRUE,
            title = "Distribution",
            ggvisOutput("totalProbs")
          )
        )
      ), # End Binom
      tabItem("norm",
              fluidRow(
                box(
                  width = 4, status = "info", solidHeader = TRUE,
                  title = "Input",
                  h4("Population Mean"),
                  sliderInput("mu", HTML("&mu;:"), 
                              min = -4, max = 4, value = 0, step= 0.5),
                  br(),
                  
                  # set sigma:
                  h4("Population Variance"),
                  sliderInput("sigma", HTML("&sigma;<sup>2</sup>:"), 
                              min = 0, max = 4, value = 1, step= 0.5),
                  br(),
                  
                  # p("The standard normal distribution describes the region that of our data covering a certain percent of the sample."),
                  
                  # percentile centered around mean:
                  h4("Central Region"),
                  #     sliderInput("cen_ptile","Highlight percentile around mean:",
                  #                 min = 65, max = 99.9, value = 65, step= 0.1)
                  selectInput("cen_ptile", "Percentile around mean:", 
                              choices = c("0", "65", "95", "97.5", "99", "99.99") )
                  
                ),
                box(
                  width = 8, status = "info", solidHeader = TRUE,
                  title = "Distribution",
                  plotOutput(outputId = "dist_plot2"),
                  verbatimTextOutput('summary')
                  )
                )
              ), # End Norm
      tabItem("t_dist",
              fluidRow(
                box(
                  width = 4, status = "info", solidHeader = TRUE,
                  title = "Input",
                  sliderInput("t_samsize", h4("Sample Size"), 
                              min = 3, max = 60, value = 10, step= 1),
                  br(),
                  h4("Central Region"),
                  selectInput("t_cen_ptile", "Percentile around mean:", 
                              choices = c("0", "65", "95", "97.5", "99", "99.99") )
                  ),
                box(
                  width = 8, status = "info", solidHeader = TRUE,
                  title = "Distribution",
                  plotOutput(outputId = "t_dist"),
                  verbatimTextOutput('t_dist_summary')
                )
              )
      ), # End t_dist
      tabItem("CLT",
              fluidRow(
                box(
                  width = 8, status = "info", solidHeader = TRUE,
                  title = "Population Distribution",
                  plotOutput("PoplnPlot", height="300px")
                ),
                box(
                  width = 4, status = "info", solidHeader = TRUE,
                  title = "Adjust Population",
                  radioButtons("dist", "Parent distribution:", list(
                    "Uniform" = "runif",
                    "Log-normal" = "rlnorm",
                    "Normal" = "rnorm",
                    "Exponential" = "rexp",
                    "Irregular" = "sample")),
                  sliderInput("k", "Sample size:", 
                              value = 5,
                              min = 2, 
                              max = 100)
                  )
                
              ),
              fluidRow(
                box(
                  width = 8, status = "info", solidHeader = TRUE,
                  title = "Sample Distribution",
                  plotOutput("plot_samples", height="300px")
                ),
                box(
                  width = 4, status = "info", solidHeader = TRUE,
                  title = "Adjust sample plot",
                  checkboxInput("setparent", "Zoom into data range?", TRUE),
                  selectInput("showdensity", label = "Plot type",
                               choices = list("Density" = 1,
                                              "Histogram" = 2), 
                               selected = 1),
                  uiOutput("prodshownorm"),
                  uiOutput("prodbinselect")
                )
              )
      ) # End CLT
    )
  )
)

