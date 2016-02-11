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
  
  

} # End server