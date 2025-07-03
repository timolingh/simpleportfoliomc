library(data.table)
library(ggplot2)
library(shiny)
library(DT)
library(scales)

## Simulation functions
normSim <- function(mu, s, nyear, nsim, wd=0) {
  
  ## The size of the matrix
  N = nyear * nsim
  
  ## Generate returns
  r = rnorm(n=N, mean=mu, sd=s)
  
  ## organize as a nyear x nsim matrix
  R <- matrix((1 + r - wd), nrow=nyear)
  
  ## The cumulative returns over the specified horizon
  #returns <- apply(R, 2, prod)
  
}

# lnormSim <- function(mu, s, nyear, nsim) {
#   
#   ## The size of the matrix
#   N = nyear * nsim
#   
#   ## Generate returns
#   r = rlnorm(n=N, meanlog=mu, sdlog=s)
#   
#   ## organize as a nyear x nsim matrix
#   R <- matrix(log(r), nrow=nyear)
#   
#   ## The cumulative returns over the specified horizon
#   returns <- apply(R, 2, sum)
#   
#   ## The distribution above is log(returns)
#   returns <- exp(returns)
#   
# }


## UI
ui <- fluidPage(
  titlePanel("Simple portfolio simulation"),
  sidebarLayout(
    sidebarPanel(
      p("Model inputs"),
      checkboxInput("useSeed", "Use seed", value = TRUE),
      numericInput("seed_value", "Seed value:", 8888),
      numericInput("initial_investment", "Initial investment:", 100000),
      numericInput("nsim", "Number of simulations to run:", 5000),
      numericInput("nyear", "Investment horizon in years:", 20),
      numericInput("mupct", "Expected return (%):", 5),
      numericInput("sdpct", "Portfolio std. dev. (%):", 20),
      numericInput("wd", "Withdrawal percent (%):", 0),
      br(),
      actionButton("goButton", "Submit"),
      p("Click button to start simulation")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary",
                 h4("Description"),
                 p("This program performs a Monte Carlo simulation of growth in
                   a hypothetical portfolio over a specified time horizon.
                   The user sets capital market assumptions and simulation parameters 
                   on the left and the program outputs probable (20th, 50th, 80th percentile) 
                   ending portfolio values."),
                 tags$hr(),
                 h4("Simulation results - value of investment at the end of horizon"),
                 p("Based on the portfolio capital market assumptions multiple simulations are
                   run.  The ending portfolio values for the 20th, 50th, and 80th percentile of all
                   the simulation results are shown below."),
                 tableOutput("sim_result"),
                 
                 tags$hr(),
                 h4("Distribution of outcomes"),
                 p("Distribution of all possible ending portfolio values. The dashed lines indicate
                   the 20th, 50th, and 80th percentiles"),
                 plotOutput("simulation_outcomes"),
                 
                 tags$hr(),
                 h4("Simulation traces - growth of investment over time"),
                 p("The portfolio value at the end of each year.  Every simulation is plotted.
                   The dashed line represents the initial investment"),
                 plotOutput("plot_sims")
        ),
        
        tabPanel("Yearly Accumulation",
                 h4("Yearly accumulation"),
                 p("Ending balance for each year. Given for 20th, 50th, 80th percentile."),
                 tableOutput("sim_table")
        )
      )
    )
  )
)


server <- function(input, output) {
  
  # resM <- reactive({
  #   set.seed(8888)
  #   res_matrix <- normSim(input$mupct / 100, input$sdpct / 100, input$nyear, input$nsim)
  #   
  # })
  
  seed_value <- eventReactive(input$goButton, {
    input$seed_value
  })
  
  init_investment <- eventReactive(input$goButton, {
    input$initial_investment
  })
  
  nyear <- eventReactive(input$goButton, {
    input$nyear
  })
  
  nsim <- eventReactive(input$goButton, {
    input$nsim
  })
  
  resM <- eventReactive(input$goButton, {
    if (input$useSeed) {
      set.seed(seed_value())
    } 
    
    ## Return a nyear x nsim matrix
    res_matrix <- normSim(input$mupct / 100., input$sdpct / 100., input$nyear, input$nsim, input$wd / 100.)
    
  })
  
  res <- reactive({
    res_vector <- apply(resM(), 2, prod) * init_investment()
  })
  
  sim_table <- reactive({
    ## Cumulative return over time
    M <- resM()
    cumM <- apply(M, 2, cumprod) * init_investment()
    dt <- data.table(cumM)
    
  })
  
  output$sim_result <- renderTable({
    q <- c(0.2, 0.5, 0.8)
    vals <- quantile(res(), q)
    quantile_dt <- data.table(Quantile = q, Value = dollar(round(vals)))
  })
  
  
  output$sim_table <- renderTable({
    dt <- sim_table()
    nyear <- dim(dt)[1]
    Mquant <- apply(dt, 1, quantile, probs=c(0.2, 0.5, 0.8))
    dt <- data.table(
      t(Mquant)
    )
    
    dt <- cbind(1:nyear, dt)
    setnames(dt, "V1", "Year")
    
    # Format all numeric columns except Year
    dt[, (2:ncol(dt)) := lapply(.SD, function(x) dollar(round(x))), .SDcols = 2:ncol(dt)]
    
    return(dt)
  })
  
  output$plot_sims <- renderPlot({
    dt <- sim_table()
    dt[, yr := 1:nyear()]
    dt_long <- melt(dt, id.vars = "yr", variable.name = "sim_id", value.name = "cum_return")
    dt_long[, portfolio_value := cum_return]
    pl <- ggplot(dt_long) + geom_line(aes(as.factor(yr), portfolio_value, group=sim_id, color=sim_id)) +
      geom_hline(yintercept=init_investment(), linetype="dashed") +
      xlab("Year") + ylab("Portfolio value") + scale_y_continuous(label=dollar_format()) +
      guides(color=F)
    pl
    
  })
  
  output$simulation_outcomes <- renderPlot({
    sim_index <- 1:nsim()
    ending_value <- res()
    dt <- data.table(sim_index, ending_value)
    
    ending_value <- dt[, ending_value]
    q <- c(0.2, 0.5, 0.8)
    vals <- quantile(ending_value, q)
    
    pl <- ggplot(dt) + stat_density(aes(x=ending_value), color="blue", geom="line") +
      geom_vline(xintercept=vals[1], linetype="dashed") + 
      geom_vline(xintercept=vals[2], linetype="dashed") +
      geom_vline(xintercept=vals[3], linetype="dashed") +
      xlab("Ending value") + ylab("Density") + scale_x_continuous(label=dollar_format()) +
      scale_y_continuous(labels = NULL, breaks = NULL)
    pl
    
  })
  
  
}

shinyApp(ui, server)


# # Return a vector of cum returns over the number of simulations
# normResult <- normSim(mu, s, nyear, nsim)
# logNormResult <- lnormSim(mu, s, nyear, nsim)
# 
# dt <- data.table(normRet = normResult, logNormRet = logNormResult)
# 
# # print(quantile(normResult, 0.2))
# # print(quantile(logNormResult, 0.2))
# 
# pl <- ggplot(dt) + geom_density(aes(x=normResult), color="blue") + 
#   geom_density(aes(x=logNormResult), color="red") 
# 
# print(pl)
# 
# niter <- 1000:5000
# conv <- sapply(niter, function(x) quantile(normSim(mu, s, nyear, x), 0.5))
# plot(niter, conv)
