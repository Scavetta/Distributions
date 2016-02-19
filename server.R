function(input,output) {
  
  df <- reactive({
    
    level <-  input$ptile
    x <- 0:level
    val <- dbinom(x, size = level, prob = input$prob)
    df <- data.frame(variable = seq(0, level, 1), values = val)
    
    return(df)
    
  })
  
  output$dist_plot1 <- renderPlot({
    
    level <-  input$ptile
    
    df.raw <- df()
    df <- as.data.frame(df.raw)
    names(df) <- c("variable", "values")
    
    q <- ggplot(df, aes(x = factor(variable), y = values)) +
      geom_bar(stat = "identity", fill = "#7489C0") +
      scale_y_continuous(minor_breaks = seq(0, 0.5, 0.025)) +
      labs(x = paste("The number of successes in",
                     level,
                     "trials, given a probability of success of", input$prob, sep = " "),
           y = "p(x)") + 
      theme(panel.background = element_blank(),
            axis.text.x = element_text(colour = "black", angle=0),
            axis.text.y = element_text(colour = "black"),
            axis.ticks.x = element_line(colour = "black"),
            panel.grid.minor = element_line(colour = "grey80"),
            panel.grid.major = element_line(colour = "grey80"))
    
    print(q)
  }) # end of render plot "dist_plot"

  
  # Make the labels for area
  mysessions <- function(x) {
    
    df <- df()
    df$variable <- factor(df$variable)
    
    
    if(is.null(x)) return(NULL)
    # compute the probability for the variable that is hovered over
    total_prob <- df[df$variable == x$variable, "values"]
    # format the value with prettyNum:
    paste0("Point probability:", "&nbsp;",
           prettyNum(total_prob, scientific=F)
    )
  }
  
  myvis <- reactive({
    
    df <- df()
    df$variable <- factor(df$variable)
    level <-  input$ptile
    outvis <-
      df %>%
      ggvis(~variable, ~values) %>%
      #     layer_bars(width = 0.8, fill = "#7489C0") %>%
      layer_bars(width = 0.8, fill = "#7489C0", key := ~variable) %>%
      add_axis("x", title = paste("The number of successes in",
                                  level,
                                  "trials, given a probability of success of", input$prob, sep = " ")) %>%
      add_axis("y", title = "p(x)") %>%
      add_tooltip(mysessions ,"click") %>%
      hide_legend("fill")
    return(outvis)
  })
  
  myvis %>% bind_shiny("totalProbs")
  
  ######################################
  ######################################
  #### Normal ##########################
  
  output$dist_plot2 <- renderPlot({
    mu <- input$mu # Get user inputed mu
    sigma <- sqrt(input$sigma) # Get user inputed sigma
    xRange <- 6 # Set the upper limit of the x-axis
    
    x <- seq(mu-(xRange*2), mu+(xRange*2), length=5000) # Define an x series based on inputed mu and xRange
    hx <- dnorm(x, mean = mu, sd = sigma) # Define the density of the normal curve at each x position
    hstd <- dnorm(x) # Define the density of the standard normal curve at each x position
    
    test.df <- data.frame(value = x, dist = hx, std = hstd)
    
    
    
    # Base plot:
    q <- ggplot(test.df, aes ( x= value, y = dist)) +
      geom_line() +
      geom_line(aes ( x= value, y = std), col = "grey80") +
      
      # Set x and y scale:
      scale_x_continuous("x", breaks = seq(-xRange, xRange, 1), labels = seq(-xRange, xRange, 1), expand = c(0,0)) +
      scale_y_continuous("pdf(x)", expand = c(0,0)) +
      
      coord_cartesian(xlim = c(-xRange, xRange), ylim = c(0, 0.8)) +
      
      # Add a line for the mean:
      geom_vline(xintercept = mu, colour = "#D67A64") +
      
      
      # Add a line for the percent being shown  
      #  geom_vline(xintercept = qnorm(ptile), colour = "red") +
      
      # Tabel the colour scales:  
      #  scale_colour_discrete("Distribution",labels = c("Normal", "t"))+
      
      
      
      # Colour percentile from the left:  
    #   geom_ribbon(data=subset(test.df, value < qnorm(ptile) ),
    #               aes(x=value,ymax=dist),ymin=0,fill="blue", alpha=0.95) +
    
    # Theme:
    theme(panel.background = element_blank(),
          axis.text.x = element_text(colour = "black", angle=0),
          axis.text.y = element_text(colour = "black"),
          axis.ticks.x = element_line(colour = "black"),
          panel.grid = element_blank()
    )
    
    if(input$cen_ptile == 0) {    
      print(q)
    } else {
      
      cen_ptile <- 0.5 + (as.numeric(input$cen_ptile)/100)/2 # Get the user inputed region of interest.
      cut_off <- qnorm(cen_ptile, mean = mu, sd = sigma)
      
      print(q +   
              # Colour percentile centered on mu:
              geom_ribbon(data=subset(test.df, value < cut_off & value > mu-(cut_off-mu) ),
                          aes(x=value,ymax=dist),ymin=0,fill="#7489C0", alpha=0.95) 
            #         +
            #           geom_vline(xintercept = cut_off, colour = "#005AA4")
      )
    }
    
    
    
  }) # end of render plot "dist_plot"
  
  
  
  output$summary <- renderText({
    
    mu <- input$mu # Get user inputed mu
    sigma <- input$sigma # Get user inputed sigma
    cen_ptile <- as.numeric(input$cen_ptile) # Get the user inputed region of interest.
    
    cen_ptile_cut <- 0.5 + (as.numeric(input$cen_ptile)/100)/2 # Get the user inputed region of interest.
    cut_off <- qnorm(cen_ptile_cut, mean = mu, sd = sqrt(sigma))
    
    paste('A normal distribution with a mean of ',
          mu,
          ' and a variance of ',
          sigma
          , 
          if (cen_ptile == 0) '.' else paste(', ', cen_ptile, '% of the sample is between ',
                                             round(mu-(cut_off-mu),2),
                                             ' and ',
                                             round(cut_off,2),
                                             "."
                                             , sep = "") # close cutoff
          , sep = "") # close main paste
  }) # End of summary

  
  ######################################
  ######################################
  #### t ###############################
  
  output$t_dist <- renderPlot({
    
    xRange <- 4 # Set the upper limit of the x-axis
    df <- input$t_samsize - 1
    x <- seq(-1*(xRange*2), (xRange*2), length=5000) # Define an x series based on inputed mu and xRange
    hstd <- dnorm(x) # Define the density of the standard normal curve at each x position
    tdist <- dt(x, df = df) # Define the density of the standard normal curve at each x position
    
    #    test.df <- data.frame(value = x, std = hstd, tdist = tdist)
    #    test.m <- melt(test.df)
    
    test.df <- data.frame(x, std = hstd, tdist = tdist)
    #    library(reshape2)
    test.m <- melt(test.df, id.vars = c("x"))
    #   names(test.m)
    
    # Base plot:
    q <-  ggplot(test.m, aes ( x = x, y = value, col = variable)) +
      geom_line() +
      #      geom_line(aes ( x= value, y = std), col = "black") +
      
      # Set x and y scale:
      scale_x_continuous("x", breaks = seq(-xRange, xRange, 1), labels = seq(-xRange, xRange, 1), expand = c(0,0)) +
      scale_y_continuous("pdf(x)", expand = c(0,0)) +  
      coord_cartesian(xlim = c(-xRange, xRange), ylim = c(0, 0.5)) +
      
      # Set colour scale:
      scale_color_manual("Distribution", labels = c("Normal", "t"), values = c("grey80","black")) +
      
      # Add a line for the mean:
      geom_vline(xintercept = 0, colour = "#D67A64") +
      
      # Theme:
      
      theme(panel.background = element_blank(),
            axis.text.x = element_text(colour = "black", angle=0),
            axis.text.y = element_text(colour = "black"),
            axis.ticks.x = element_line(colour = "black"),
            legend.position = c(0.9, 0.9),
            legend.background = element_blank(),
            legend.key = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
      )        
    
    
    if(input$t_cen_ptile == 0) {    
      print(q)
    } else {
      
      
      cen_ptile <- 0.5 + (as.numeric(input$t_cen_ptile)/100)/2 # Get the user inputed region of interest.
      cut_off <- qt(cen_ptile, df = df)
      
      print(  q + geom_ribbon(data=subset(test.m, variable == "tdist" & x < cut_off & x > -1*(cut_off) ),
                              aes(x=x,ymax=value),ymin=0,fill="#7489C0", colour = NA, alpha=0.95)
      )
    }
    
    
    
    
  }) # end of render plot "dist_plot"
  
  
  
  output$t_dist_summary <- renderText({
    
    cen_ptile <- as.numeric(input$t_cen_ptile) # Get the user inputed region of interest.
    cen_ptile_cut <- 0.5 + (as.numeric(input$t_cen_ptile)/100)/2 # Get the user inputed region of interest.
    df <- input$t_samsize - 1
    
    cut_off <- qt(cen_ptile_cut, df = df)
    
    paste('A t distribution with ',
          df,
          ' degrees of freedom ',
          if (cen_ptile == 0) '.' else paste(', ', cen_ptile, '% of the sample is between ',
                                             round(-1*(cut_off),2),
                                             ' and ',
                                             round(cut_off,2),
                                             "."
                                             , sep = "") # close cutoff
          , sep = "") # close main paste
  }) # End of summary
  
  ##########################
  ##########################
  ##### CLT ################
  
  # Make a list called data, where the type of distribution (fun) and 
  # associated values (vals) are stored  
  data <- reactive({  
    
    if ( input$dist == "sample") {
      
      #      vals <-  do.call(dist, list(x = 1:50, size = 100, replace = TRUE))
      set.seed(68686)
      vals <-  c(sample(1:50, 5000, TRUE), rnorm(5000, 38, 6), rexp(1000), runif(5000, 4, 30))
      
    } else {
      vals <-  do.call(input$dist, list(n=20000))
      
    }      
    return (list(fun=input$dist, vals=vals))
  })
  
  #############################################################
  output$PoplnPlot <- renderPlot({
    # Same a human readable distribution name as a string
    distname <- switch(input$dist,
                       sample = "Irregular distribution",
                       rexp = "Exponential distribution",
                       rnorm = "Normal distribution",
                       rlnorm = "Log-normal distribution",
                       runif = "Uniform distribution")
    
    # Save the input variables as new variables
    nparent <- 20000
    #     n <- input$n # Size of sample.
    n <- 30 # Size of sample.
    k <- input$k # number of reiterations.
    
    # pdist: The generated data set 
    pdist <- data()$vals
    
    # x: A list with multiple random samples from the parent data set:
    x <- replicate(k, do.call(sample, list(x = pdist, size = n, replace = TRUE)))
    
    # ndist: A vector containing the means of all the k samples of size n from the original population.  
    ndist <- rowMeans(x) 
    
    # Make irregular dist:
    set.seed(68686)
    r_irreg <- c(sample(1:50, 5000, TRUE), rnorm(5000, 38, 4), exp(x = 5000), runif(5000, 5, 25))
    
    # expect: Set expected mean for each distribution
    expect <- switch(input$dist,
                     irreg = c(mean(r_irreg), sd(r_irreg)),
                     rexp = c(1^-1, 1^-2),
                     rnorm = c(0, 1),
                     rlnorm = c(exp(0+(1/2)*1^2), exp(0 + 1^2)*(exp(1^2)-1)),
                     runif = c(0.5, (1/12)*1))
    
    # obs: A data frame containing the observed means and variance of the pdist
    obs <- data.frame(
      pdist=c(mean(pdist),var(pdist)), # Mean and variance of the parent distribution
      ndist=c(mean(ndist), var(ndist)) # Mean and variance of means of the k samples
    ) 
    
    # Plot 1: The original distribution.
    df.raw <- data()$vals
    df.raw <- as.data.frame(df.raw)
    #print(names(df.raw))
    names(df.raw)[1] <-  c("value")
    
    q <- ggplot(df.raw, aes ( x = value, )) +
      #  geom_histogram(aes(y = ..density..), col = "black", fill = "#F5B319") +
      geom_histogram(fill = "#F5B319") +
      scale_y_continuous(name = "Absolute frequency", expand = c(0,0)) +
      theme(panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.ticks.x = element_line(colour = "black"),
            axis.ticks.y = element_line(colour = "black"),
            axis.line = element_line(colour = "black"),
            #        axis.line.y = element_line(colour = "blue", size = 2),
            axis.text.x = element_text(colour = "black"),
            axis.text.y = element_text(colour = "black")
      )
    
    
    if (input$dist == "rnorm") {
      print(q + scale_x_continuous("Value in Population", expand = c(0,0), limits = c(-4,4)))  
    } else if (input$dist == "rexp") {
      print(q + scale_x_continuous("Value in Population", expand = c(0,0), limits = c(0,10)))    
    } else if (input$dist == "rlnorm") {
      print(q + scale_x_continuous("Value in Population", expand = c(0,0), limits = c(0,20)))    
    } else if (input$dist == "runif") {
      print(q + scale_x_continuous("Value in Population", expand = c(0,0), limits = c(0,1)))    
    } else {  
      print(q + scale_x_continuous("Value in Population", expand = c(0,0)))
    }      
    
    
  }) # end renderPlot for PoplnPlot
  #############################################################
  
  
  
  ### Make UI elements:
  output$prodshownorm <- renderUI({
    if (is.null(input$showdensity)) {
      return(NULL) 
    } else if (input$showdensity == 1) {
      checkboxInput("shownorm", "Show normal distributuion?", FALSE)
    } else if (input$showdensity == 2) {
      checkboxInput("setwidth", "Adjust Binwidth? (not for irregular dist.)", FALSE)
    }  
  })
  
  output$prodbinselect <- renderUI({
    # req(input$showdensity)
    # shiny::validate(need(input$showdensity, message=FALSE))
    if (is.null(input$setwidth)) {
      return(NULL) 
    } else if (input$setwidth) {
    sliderInput("bin", "Bin width:", 
              value = 0.13,
              min = 0.025, 
              max = 0.25)
    }
  })
  
  #############################################################
  output$plot_samples <- renderPlot({
    if (is.null(input$setwidth)) {
      return(NULL) 
    }

    # New
    n <- input$k # Size of sample.
    k <- 30 # number of reiterations - fixed
    
    # pdist: The generated data set 
    pdist <- data()$vals
    
    # x: A list with multiple random samples from the parent data set:
    x <- replicate(k, do.call(sample, list(x = pdist, size = n, replace = TRUE)))
    
    # ndist: A vector containing the means of all the k samples of size n from the original population.  
    ndist <- colMeans(x) 
    
    df.raw <- as.data.frame(ndist)
    names(df.raw)[1] <-  c("value")
    
    # if (is.null(df.raw)) {
    #   return(NULL) 
    # }
    # 
    p <- ggplot(df.raw, aes (x = value))
    
    if (input$setwidth) {
      bin_norm <- input$bin
      bin_exp <- input$bin
      bin_lnorm <- input$bin
      bin_unif <- input$bin
    } else {
      bin_norm <- 8/60
      bin_exp <- 10/60
      bin_lnorm <- 20/120
      bin_unif <- 1/40
    }
    # 
    # 
    # 
    # # Use parental x-scale?
    if (!input$setparent) {
      if (input$dist == "rnorm") {
        p <- p +
          scale_x_continuous("Sample mean", expand = c(0,0), limits = c(-4,4))
      } else if (input$dist == "rexp") {
        p <- p +
          scale_x_continuous("Sample mean", expand = c(0,0), limits = c(0,10))
      } else if (input$dist == "rlnorm") {
        p <- p +
          scale_x_continuous("Sample mean", expand = c(0,0), limits = c(0,20))
      } else if (input$dist == "runif") {
        p <- p +
          scale_x_continuous("Sample mean", expand = c(0,0), limits = c(0,1))
      } else {
        p <- p +
          scale_x_continuous("Sample mean", expand = c(0,0), limits = c(0,60))
      }
    }

      
    # Density/histogram radiobutton:        
    if(input$showdensity == 1) { # choose density plot
    ################# good
      p <- p +
        geom_density() +
        scale_y_continuous(name = "Density", expand = c(0,0))

      if(input$shownorm) { # Display norm on density
        p <- p +
          stat_function(fun = dnorm,
                    colour = "red",
                    args = list(mean = mean(df.raw$value), sd = sd(df.raw$value)))
      }
      ################# good
      
    #   
    } else if (input$showdensity == 2) { # Show histogram
      p <- p +
      scale_y_continuous(name = "Absolute frequency", expand = c(0,0))

        if (input$dist == "rnorm") {
          p <- p + geom_histogram(fill = "#C42126", binwidth = bin_norm)
        } else if (input$dist == "rexp") {
          p <- p + geom_histogram(fill = "#C42126", binwidth = bin_exp)
        } else if (input$dist == "rlnorm") {
          p <- p + geom_histogram(fill = "#C42126", binwidth = bin_lnorm)
        } else if (input$dist == "runif") {
          p <- p + geom_histogram(fill = "#C42126", binwidth = bin_unif)
        } else {
          p <- p + geom_histogram(fill = "#C42126")
        }

    }
    
        
    
    p +
      theme(panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.ticks.x = element_line(colour = "black"),
            axis.ticks.y = element_line(colour = "black"),
            axis.line = element_line(colour = "black"),
            #        axis.line.y = element_line(colour = "blue", size = 2),
            axis.text.x = element_text(colour = "black"),
            axis.text.y = element_text(colour = "black")
      )
    
  })
  # end renderPlot for plot_samples
  #############################################################

} # End server