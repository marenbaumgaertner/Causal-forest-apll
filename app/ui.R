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

set.seed(42)


ui <- fluidPage(theme = shinytheme("simplex"),
                navbarPage( # Create different panel with tabsetPanel(
                  "Causal Forest Fun",
                  
                  # First Panel
                  tabPanel("Visualisation",
                           h3("Welcome to  our Shiny App!"),
                           h5("For the general General Setup see the Theoretical Background"),
########################--PUT YOUR EXPLANATION IN HERE--########################
################################################################################
                           br(),
                           br(), 
                           sidebarPanel(
                             "This page allows you to engage with causal forests.",
                             br(),
                             br(),
                             selectInput("e", withMathJax(helpText("Choose a treatment propensity")), 
                                         choices = c("e(x) = pnorm(sin(x))" = 1, 
                                                     "e(x) = 0.5" = 2, 
                                                     "e(x) = step" = 3), multiple = FALSE),
                             br(),
                             selectInput("dgp", withMathJax(helpText("Choose a data generating process")), 
                                         choices = c("𝜏(x) = 1[x > -0.5π]" = 1, 
                                                     "𝜏(x) = exp(x)" = 2, 
                                                     "𝜏(x) = 0.3x" = 3), multiple = FALSE),
                             br(),
                             sliderInput(
                               "grid",
                               withMathJax(helpText("Choose a value for x:")),
                               min = -3,
                               max = 3,
                               value = -2,
                               step = 0.5,
                               width = "90%"
                             ),
                             submitButton(" Apply changes!", icon = icon("tree")),
                             textOutput("result")
                           ),
                           mainPanel(
                             h5("CATE function"),
########################--PUT YOUR EXPLANATION IN HERE--########################
                             
                             p("This Graph plots the true CATE-Function 𝜏 (black) as well as the estimated 𝜏_hats (red) for different levels of X1. With increased sample size n, the tau_hats are more close to the true 𝜏s"),
                             plotOutput("CatePlot"),
                             h5("Residual regression"),
########################--PUT YOUR EXPLANATION IN HERE--########################
                             p("This graph plots the residuals of W against the residuals of Y, showing how accurate the Causal forest predicts treatment W and outcome Y. Additionally, the size of the residuals is proportional to the weight they receive and the color indicates lower to higher values of X1.
Move the slider bar to have a look at how the weights change for different values of X1."),
                             plotOutput("scatterPlot")
                           ),
                           cellWidths = c("40%", "60%")
                  ),
                  tabPanel("Theoretical Background", h3("Here you find some theoretical background on causal forests"),
                           br(),
                           br(),
                           sidebarLayout(
                             sidebarPanel(h4("Causal Forest Theory:"),
                                          imageOutput("genset"),
                                          textOutput("explanation1"),
                                          br(),
                                          textOutput("explanation2"),
                                          #textOutput("explanationformula"),
                                          imageOutput("image2"),
                                          textOutput("explanation3"),
                                          br(),
                                          br(),
                                          textOutput("explanation4"),
                                          br(),
                                          textOutput("explanation5"),
                                          br(),
                                          textOutput("explanation6"),
                                          br(),
                                          br(),
                                          textOutput("explanation7"),
                                          br(),
                                          imageOutput("image1"),
                                          br(),
                                          textOutput("explanation8"),
                                          br(),
                                          textOutput("explanation9"),
                                          br(),
                                          textOutput("explanation10"),
                                          textOutput("explanation11"),
                                          br(),
########################--PUT YOUR EXPLANATION IN HERE--########################
                                          h4("DGP 1"), 
                                          p("Here, we can see at x = -pi/2 an upwards jump of the true CATE from 0 to 1.
                                            Very close to x = -pi/2, the estimates differ a lot to the true CATEs. 
                                            If we move further away from x = -pi/2,
                                            the estimated CATE gets closer to the true CATE.
                                            
                                            In the case of a balanced treatment share, for small values, x < -1.5,
                                            the predicted CATEs are close to zero.
                                            Hence, the RORR presents a flat regression-line. For values larger than -1.5,
                                            the predicted CATEs are close to one and the regression line gets steeper. 
                                            
                                            When we want to predict the treatment effect of a small x value i.e. of -3,
                                            these residuals receive a higher weight (larger circle),
                                            where the corresponding observations end up more often in the same leaf (=green points). 
                                            At the y-axis (res_y) the green residuals are centered around zero,
                                            because the outcome model fits well the observations corresonding y-values
                                            under a small treatment effect (less variance of y-values). 
                                            In contrast, when we aim to predict the treatment effect of a high x value i.e. of 3,
                                            these residuals in line with higher x values (orange/red points) are weighted more heavily.
                                           
                                            In the cases of non-constant treatment propensities, the res_W's are more dispersed. 
                                            "),
########################--PUT YOUR EXPLANATION IN HERE--########################
                                          h4("DGP 2"), 
                                          p("The CATE is close to zero for individuals with a low value in x 
                                            and increases exponentially when increasing the value of x, which leads to an
                                            extreme difference between treated and untreated.
                                            
                                            At negative x-values (green points), low values and high values 
                                            of res_W co-occur with low values of res_y, since the treatment effect is 
                                            close to 0 in this area and thus incorrect assigning has no big influence 
                                            (treatment and non-treatment don't make a difference concerning the outcome 
                                            at these points). As the x-values start to increase (fading from yellow 
                                            to red points), low values of res_W co-occur with large negative values of 
                                            res_y and high values of res_W co-occur with high values of res_y. This is 
                                            because mistakes in the prediction of the treatment lead to big mistakes in 
                                            the estimation of the outcome, since the difference between treated and 
                                            untreated gets quite substantial when x gets bigger than 0. 
                                            
                                            At low values of x, where the slopes of the true IATE function is small, 
                                            units with low values of x receive most weight. We see that when the the 
                                            slope of the true IATE function increases, units with higher values of x 
                                            receive more weight."),
########################--PUT YOUR EXPLANATION IN HERE--########################
                                          h4("DGP 3"),
                                          p("Explanation DGP 3"),
                                          p("This DGP is defined by having a negative treatment effect for 
                                          units with a value of x below 0, meaning the treated have a 
                                          negative outcome in this area. An ambivalent treatment could also 
                                          me imaginable, hence, it is considered here.
                                          We can somewhat see that for estimatig the treatment effect for 
                                          a low value of x, predominantly observations with a low value of 
                                          x are given a relatively bigger  weight. On the contrary, for 
                                          the estimation of the treatment effect for a high x,
                                          observations with a similar x-value get a bigger weight.
                                          
                                          This is expected as, when estimating the treatment effect
                                          for a new observation, causal forests give higher weights
                                          to observations with a similar value for the important variable
                                          (in this case x).
                                          
                                          We can see that the observations with a low value for
                                          x are are mainly located on the left side of the
                                          graph for the residual-on-residual-regression. This
                                          occurs since the treatment probability in this are of
                                          the x-scale is very low (in the case of the step-function
                                          as treatment-probability). On the other hand, observations
                                          with a high value of x are predominantly located on the
                                          right side of the graph (on the side for the treated 
                                          observations), as the treatment-probability is very high
                                          in that are of the x-scale.
                                          
                                          We can also see that for observations that settled further
                                          away from x = 0, the observations stray further away from
                                          0 residual for y, as missclassified observations will display
                                          more variance in y."),
                                          width = "60%"),
                             mainPanel()
                           )
                           
                  ),
                  tabPanel("Fun",
                           setBackgroundImage(
                             src = "https://images-wixmp-ed30a86b8c4ca887773594c2.wixmp.com/f/df03765c-4963-440f-b65c-9d892bd45ba3/dej1kki-36b93e62-9880-4753-a9be-87e585cf380a.png/v1/fill/w_1600,h_900,q_80,strp/blinding_light_by_natozuski_dej1kki-fullview.jpg?token=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1cm46YXBwOjdlMGQxODg5ODIyNjQzNzNhNWYwZDQxNWVhMGQyNmUwIiwiaXNzIjoidXJuOmFwcDo3ZTBkMTg4OTgyMjY0MzczYTVmMGQ0MTVlYTBkMjZlMCIsIm9iaiI6W1t7ImhlaWdodCI6Ijw9OTAwIiwicGF0aCI6IlwvZlwvZGYwMzc2NWMtNDk2My00NDBmLWI2NWMtOWQ4OTJiZDQ1YmEzXC9kZWoxa2tpLTM2YjkzZTYyLTk4ODAtNDc1My1hOWJlLTg3ZTU4NWNmMzgwYS5wbmciLCJ3aWR0aCI6Ijw9MTYwMCJ9XV0sImF1ZCI6WyJ1cm46c2VydmljZTppbWFnZS5vcGVyYXRpb25zIl19.KEKdAkMR4R097tQtgeuBF_DG-mdgj4BLzbLScCBteJY"
                             #"https://thumbs.dreamstime.com/z/creative-shiny-pink-color-background-flat-lay-abstract-decorations-soft-focus-163557328.jpg", shinydashboard = FALSE
                           ),
                           imageOutput("image3")),

                  tabPanel("So Shiny Its Blinding",
                           setBackgroundImage(
                             src = "https://images-wixmp-ed30a86b8c4ca887773594c2.wixmp.com/f/df03765c-4963-440f-b65c-9d892bd45ba3/dej1kki-36b93e62-9880-4753-a9be-87e585cf380a.png/v1/fill/w_1600,h_900,q_80,strp/blinding_light_by_natozuski_dej1kki-fullview.jpg?token=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1cm46YXBwOjdlMGQxODg5ODIyNjQzNzNhNWYwZDQxNWVhMGQyNmUwIiwiaXNzIjoidXJuOmFwcDo3ZTBkMTg4OTgyMjY0MzczYTVmMGQ0MTVlYTBkMjZlMCIsIm9iaiI6W1t7ImhlaWdodCI6Ijw9OTAwIiwicGF0aCI6IlwvZlwvZGYwMzc2NWMtNDk2My00NDBmLWI2NWMtOWQ4OTJiZDQ1YmEzXC9kZWoxa2tpLTM2YjkzZTYyLTk4ODAtNDc1My1hOWJlLTg3ZTU4NWNmMzgwYS5wbmciLCJ3aWR0aCI6Ijw9MTYwMCJ9XV0sImF1ZCI6WyJ1cm46c2VydmljZTppbWFnZS5vcGVyYXRpb25zIl19.KEKdAkMR4R097tQtgeuBF_DG-mdgj4BLzbLScCBteJY"
                             #"https://thumbs.dreamstime.com/z/creative-shiny-pink-color-background-flat-lay-abstract-decorations-soft-focus-163557328.jpg", shinydashboard = FALSE
                           )),
                                    # Last Panel
                  tabPanel("About", # panel name
                           br(),
                           br(), # space
                           sidebarPanel(
                             p("This app has been created as a group project by Maren Baumgärtner, Sophia Herrmann, Kevin Kopp, Alexandros Parginos Dös and Stella Rotter for the course \" DS407 Causal Machine Learing\"."
                               ),
                             br(),
                             p("In case you have any further questions,
feel free to contact us via"), em("maren.baumgaertner@student.uni-tuebingen.de"), width = "60%"
                             
                             
                  )),
                  # colors
                  setSliderColor(c("#974063"),1), # #6495ed HEX code für cornflowerblue
                  setBackgroundColor(
                    color = "#FAF9F6",
                    shinydashboard = FALSE
                  )
                ))
