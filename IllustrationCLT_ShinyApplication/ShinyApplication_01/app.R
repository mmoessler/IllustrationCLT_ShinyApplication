
library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(
      
      # tags$hr(),
      # tags$h3("Change the inputs"),
      tags$div(HTML("<span style='margin-top: 25pt; font-size: 18pt'>Change Inputs</span>")),

      tags$hr(),
      
      # Input 1: Sample size ----
      sliderInput(inputId = "N",
                  # label = withMathJax(
                  #   'Sample Size \\(N\\)'
                  # ),
                  label = "Sample Size",
                  min = as.numeric(1),
                  max = as.numeric(100),
                  value = as.numeric(11),
                  step = 5),
      
      # Input 2: Probability ----
      sliderInput(inputId = "p",
                  # label = withMathJax(
                  #   'Probability \\(p\\)'
                  # ),
                  label = "Probability of Success",
                  min = as.numeric(0),
                  max = as.numeric(1),
                  value = as.numeric(0.5),
                  step = 0.05)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      fluidRow(
        
        # Output 1: non standardized ----
        column(12,
               # HTML("<hr>"),
               h3("Law of Large Number (LLN): Consitency"),
               h4("Sampling Distribution of Sample Average"),
               h5("Bernoulli Random Variable"),
               plotOutput("Plot01", height = 350)),

        # Output 2: standardized ----
        column(12,
               HTML("<hr>"),
               h3("Central Limit Theorem (CLT): Asymptotic Normality"),
               h4("Sampling Distribution of Standardized Sample Average"),
               h5("Bernoulli Random Variable"),
               plotOutput("Plot02", height = 350)),
        
        # # Output 3: For bin width (1) ----
        # column(12,
        #        HTML("<hr>"),
        #        h3("Some Additional Illustration"),
        #        h4("Ordered Values for Standardized Sample Average"),
        #        h5("Bernoulli Random Variable"),
        #        plotOutput("Plot03", height = 350)),
        # 
        # # Output 3: For bin width (2) ----
        # column(12,
        #        HTML("<hr>"),
        #        h3("Some Additional Illustration"),
        #        h4("Choosen width and number of bins"),
        #        h5("Bernoulli Random Variable"),
        #        tableOutput("Table01"))
        
      )
    )
  )
)

server <- function(input, output) {

  Sim <- reactive({
    
    # function for simulation results
    Y_bar_sim_fun <- function(RR,NN,p){
      set.seed(12345)
      # theoretical moments
      mu <- p      # mean
      s2 <- p*(1-p) # variance
      # initialize vectors for simulation results
      Y.bar <- numeric(RR)
      Y.bar.z <- numeric(RR)
      for (ii in 1:RR) {
        Y.sim <- rbinom(n=NN, size=1, prob=p)
        Y.bar[ii] <- mean(Y.sim)
        Y.bar.z[ii] <- (mean(Y.sim)-mu)/sqrt(s2/NN)
        # see, S&W, 2020, p.89
      }
      return(list(Y.bar=Y.bar, Y.bar.z=Y.bar.z))
    }
    
    N <- input$N
    p <- input$p
    mu  <- p
    s2 <- p*(1-p)
    
    Y.bar.sim.n <- Y_bar_sim_fun(RR = 10000, NN = N, p = p)
    Y.bar.sim.n
    
  })
  
  output$Plot01 <- renderPlot({
    
    SimRes <- Sim()
    
    N <- input$N
    p <- input$p
    mu  <- p
    s2 <- p*(1-p)
    
    # Y.bar.sim.n <- Y_bar_sim_fun(RR = 10000, NN = N, p = p)
    Y.bar.sim.n <- Sim()
    
    brk.int <- 1/N 
    
    hist(x=Y.bar.sim.n$Y.bar, breaks=seq(0,1,brk.int), freq=FALSE,
         xlim=c(0,1),
         # main=paste("n=",N),
         main="",
         xlab="", 
         ylab="Absolute Frequency")
    abline(v = mu, lty = 2, col = "red", lwd = 2)
    
  })
  
  output$Plot02 <- renderPlot({
    
    N <- input$N
    p <- input$p
    mu <- p
    s2 <- p*(1-p)
    
    # Y.bar.sim.n <- Y_bar_sim_fun(RR = 10000, NN = N, p = p)
    Y.bar.sim.n <- Sim()
    
    jj <- unique(Y.bar.sim.n$Y.bar.z)
    # jj
    
    kk <- order(jj)
    # kk
    
    brk.int <- diff(jj[kk])[2]
    # brk.int
    
    if (N == 1) {
      
      # see: https://statisticsglobe.com/plot-only-text-in-r
      plot(x = 0:1, # Create empty plot
           y = 0:1,
           ann = F,
           bty = "n",
           type = "n",
           xaxt = "n",
           yaxt = "n")
      text(x = 0.5, # Add text to empty plot
           y = 0.5,
           # "This is my first line of text!\nAnother line of text.\n(Created by Base R)", 
           "Choose a sample size greater than one!", 
           cex = 2)
      
    } else {
      
      # histogram for Y.bar.z
      hist(x=Y.bar.sim.n$Y.bar.z, breaks=c(rev(seq(0,-10,-brk.int)),seq(brk.int,10,brk.int)), freq=FALSE,
           xlim=c(-3,3), ylim=c(0,0.6),
           # main=paste("n=",N),
           main="",
           xlab="", 
           ylab="Relative Frequency")
      curve(dnorm(x, mean = 0, sd = 1), -3, 3,
            xlim = c(-3,3), 
            ylim=c(0,0.6),
            lty = 2,
            lwd = 2, 
            xlab = "", 
            ylab = "",
            add = TRUE,
            col = "red")
      legend("topleft",
             legend = "Standard Normal PDF",
             lty = 2,
             lwd = 1,
             col = "red",
             inset = 0.05)
      
    }
    
  })
  
  
  # Some interesting plot
  output$Plot03 <- renderPlot({
    
    N <- input$N
    p <- input$p
    mu <- p
    s2 <- p*(1-p)
    
    # Y.bar.sim.n <- Y_bar_sim_fun(RR = 10000, NN = N, p = p)
    Y.bar.sim.n <- Sim()
    
    # # standardized
    # ii <- order(Y.bar.sim.n$Y.bar.z)
    # plot(seq(1,length(ii)), Y.bar.sim.n$Y.bar.z[ii],
    #      xlab = "",
    #      ylab = "")
    # non-standardized
    ii <- order(Y.bar.sim.n$Y.bar)
    plot(seq(1,length(ii)), Y.bar.sim.n$Y.bar[ii],
         xlab = "",
         ylab = "",
         ylim = c(0,1))
    
    
  })
  
  
  output$Table01 <- renderTable({
    
    N <- input$N
    p <- input$p
    mu <- p
    s2 <- p*(1-p)
    
    # Y.bar.sim.n <- Y_bar_sim_fun(RR = 10000, NN = N, p = p)
    Y.bar.sim.n <- Sim()
    
    jj <- unique(Y.bar.sim.n$Y.bar.z)
    # jj
    
    kk <- order(jj)
    # kk
    
    brk.int <- diff(jj[kk])[2]
    
    result <- cbind(brk.int, length(jj))
    colnames(result) <- c("width of bins", "number of bins")

    result
    
  })
  
}

shinyApp(ui = ui, server = server)
