library(shiny)
defaultW <- getOption("warn") 
options(warn = -1) 
library(grf)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(partykit)
library(patchwork)
library(causalTree)
library(gganimate)
options(warn = defaultW)
library(shinythemes)
library(shinyWidgets)
set.seed(420)


server <- function(input, output) {
  list_of_tau <- list("0 + 1*(x>-0.5*pi)",
                      "0 + exp(x)",
                      "0.3*x")
  list_of_e <- list("pnorm(sin(x))", #
                    "0.5", # 50:50
                    "ifelse(x < -1, 0.1,ifelse(x >= -1 & x < 1, 0.5,ifelse(x >= 1 & x < pi, 0.9, 0)))" # stepfunction
  )
  
  n = 200 # n variieren 200/2000
  p = 2
  
  
  # Draw sample
  error <- rnorm(n,0,1/10)
  #x = matrix(runif(n*p,-pi,pi),ncol=p)
  x = read.csv("data/x.csv") %>% select(-X) %>% as.matrix()
  m0 = function(x){sin(x)}
  
  w_all = read.csv("data/w.csv") %>% select(-X) %>% as.matrix()
  
  
  
  ## define all data within reactive function as they vary in tau for residual plot
  sliderValues <- reactive({
    
    # pick chosen e from list
    e = function(x){eval(parse(text = list_of_e[[as.numeric(input$e)]]))} 
    
    
    # pick chosen DGP from list
    tau <- function(x){eval(parse(text = list_of_tau[[as.numeric(input$dgp)]]))}
    
    # compute output under treatment
    m1 = function(x){m0(x) + tau(x)}
    
    w <- w_all[,as.numeric(input$dgp)] # change for different e
    #set.seed(42);w = rbinom(n,1,e(x)) # treatment status
    y = m0(x[,1]) + w*tau(x[,1]) + error # outcome
    
    
    
    if (as.numeric(input$e) == 1){
      cf <- readRDS("data/cf_sinus.RData")
      cf <- cf[[as.numeric(input$dgp)]]
    }else if(as.numeric(input$e) == 2){
      cf <- readRDS("data/cf_balanced.RData")
      cf <- cf[[as.numeric(input$dgp)]]
    }else if(as.numeric(input$e) == 3){
      cf <- readRDS("data/cf_step.RData")
      cf <- cf[[as.numeric(input$dgp)]]
    }
    
    # Get residuals
    res_y = as.matrix(y - cf$Y.hat)
    res_w = as.matrix(w - cf$W.hat)
    # Set parameters
    grid <- input$grid
    
    gridx = as.matrix(cbind(grid,matrix(0,1,p-1)))
    grid_hat = predict(cf, newdata = gridx)$predictions
    
    
    alpha = get_forest_weights(cf, newdata = gridx)[1,]
    rorr = lm(res_y ~ res_w, weights = alpha)
    
    # residual on residual regression
    rorr = lm(res_y ~ res_w,weights = alpha)
    slope <- rorr$coefficients[2]
    
    # data
    data_rorr <- as.data.frame(data.frame(res_w,res_y,alpha=alpha,x=x[,1],                    
                                          constant=rep(rorr$coefficients[1],n),
                                          slope=rep(rorr$coefficients[2],n),
                                          grid = rep(grid,n),
                                          grid_hat = rep(grid_hat,n))
    )
    return(data_rorr)
    
  })
  
  # Define reactive part for CATE plot
  sliderGrid <- reactive({
    
    # pick chosen e from list
    e = function(x){eval(parse(text = list_of_e[[as.numeric(input$e)]]))} 
    
    # pick chosen DGP from list
    tau <- function(x){eval(parse(text = list_of_tau[[as.numeric(input$dgp)]]))}
    m1 = function(x){m0(x) + tau(x)}
    
    #set.seed(42);w = rbinom(n,1,e(x)) # treatment status
    w <- w_all[,as.numeric(input$dgp)] 
    y = m0(x[,1]) + w*tau(x[,1]) + error # outcome
    #cf = causal_forest(x, y, w,tune.parameters = "all", seed=42)
    
    if (as.numeric(input$e) == 1){
      cf <- readRDS("data/cf_sinus.RData")
      cf <- cf[[as.numeric(input$dgp)]]
    }else if(as.numeric(input$e) == 2){
      cf <- readRDS("data/cf_balanced.RData")
      cf <- cf[[as.numeric(input$dgp)]]
    }else if(as.numeric(input$e) == 3){
      cf <- readRDS("data/cf_step.RData")
      cf <- cf[[as.numeric(input$dgp)]]
    }
    
    
    grid_seq <- seq(-3,3,0.2) # sequence of X
    grid <- input$grid # chosen tau
    tau_y <- tau(grid_seq)
    gridx = as.matrix(cbind(grid_seq,matrix(0,length(grid_seq),p-1)))
    grid_hat = predict(cf, newdata = gridx)$predictions
    
    data_grid <- as.data.frame(data.frame(grid_seq,
                                          grid = rep(grid,length(grid_seq)),
                                          grid_hat = grid_hat,
                                          tau_y)
    )
  })
  
  
  
  output$CatePlot <- renderPlot({
    ggplot(data = sliderGrid()) + 
      geom_line(aes(x = grid_seq, y = grid_hat), color="#d02404") + 
      geom_line(aes(x = grid_seq, y = tau_y), color="black") + 
      geom_point(aes(x=grid[1],y=`grid_hat`[grid_seq==grid]),size=4,color="#d02404",shape=4)+
      xlim(-3,3) +
      theme_bw() +
      ylab(expression(tau)) +
      xlab(expression(x)) +
      labs(size = expression(alpha)) +
      theme(plot.background = element_rect(fill = "#FAF9F6", colour = "#FAF9F6"),
            panel.background = element_rect(fill = "#FAF9F6", colour = "#FAF9F6"))
    
    
  }, height = 400, width = 700)
  
  
  output$scatterPlot <- renderPlot({
    ggplot(aes(res_w, res_y, colour = x), data=sliderValues()) +
      geom_point(aes(size = alpha), alpha = 0.5, show.legend = TRUE) +
      scale_colour_gradient2(low = "#4e9b46", mid = "#fbda14", high = "#d02404", name = expression(X[1])) + 
      geom_abline(mapping=aes(slope=slope, intercept=constant)) +
      theme_bw() +
      ylab(expression(res[y])) +
      xlab(expression(res[w])) +
      labs(size = expression(alpha)) +
      theme(plot.background = element_rect(fill = "#FAF9F6", colour = "#FAF9F6"),
            panel.background = element_rect(fill = "#FAF9F6", colour = "#FAF9F6"))
    
  }, height = 400, width = 800)
  
  
  
  output$about <- renderText({
  })
  
  output$explanation1 <- renderText({
    "Idea: Modify supervised ML methods to target causal effect estimation"
  })
  
  output$explanation2 <- renderText({
    "How: Causal Forests estimate CATEs as a localized/individualized residual-on-residual regression with X-specific weights alpha(X)"
  })
  
  output$explanationformula <- renderText({
    "𝜏^(cf)(x) = argmin_𝜏 {Σ(i=1)^N α_i(x) [(Y - mhat(X_i))- 𝜏(x)(W_i-ê(X_i))]^2}"
  })
  
  output$explanation3 <- renderText({
    "In the first step, the nuisance parameters mhat(X) and ê(X) are estimated.
  After that, a Causal Forest is built using both nuisance parameters and weights α(X) to predict tauhat."
  })

  output$explanation4 <- renderText({
    "Splitting Criterion: General Forests split along the influence function of the partially linear estimator:"
  })
  
  
  output$explanation6 <- renderText({
    "This pseudo-outcome is used to place regression splits. In this way it eventually results in the weights for the residual-on-residual regression."
  })
  
  output$explanation7 <- renderText({
    "Weights:
    For the evolution of the weights consider this illustration of Athey, Tibshirani & Wagner (2019):"
  })

  output$explanation8 <- renderText({
    "Causal Inference:
    Careful when it comes to Causal Inference! 
    The CATEs estimated by Causal Forests are asymptotically normal. 
    Athey et al. (2019) propose an inference procedure that works for low dimensional Xs. 
    Even in this case the asymptotic properties require a lot of observations. 
    Causal interpretations for high-dimensional Xs are not in line with Athey et al. (2019)."
  })
  
  output$explanation9 <- renderText({
    "If you want to dive further into causal forests we recommend looking at the following additional sources:"
  })
  
  output$explanation10 <- renderText({
    "grf package documentation: https://grf-labs.github.io/grf/index.html"
  })  
    
  output$explanation11 <- renderText({
    "ML-based Causal Inference tutorial: https://bookdown.org/stanfordgsbsilab/ml-ci-tutorial/"
  })
  
  output$genset <- renderImage({
    list(src = "www/generalsetup.jpg",
         width = 700,
         height = 350)
  }, deleteFile = F)

  output$image2 <- renderImage({
    list(src = "www/function.jpg",
         width = "40%",
         height = "auto")
  }, deleteFile = F)
  
  output$split <- renderImage({
    list(src = "www/splitting_crit.jpg",
         width = "40%",
         height = "auto")
  }, deleteFile = F)
  
 
  output$image1 <- renderImage({
    list(src = "www/image.jpg",
         width = 400,
         height = 400)
  }, deleteFile = F)
  
  output$image3 <- renderImage({
    list(src = "www/meme.jpg",
         width = "70%",
         height = 550,
         align = "center")
  }, deleteFile = F)
  
}


