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
                               value = -3,
                               step = 0.5,
                               width = "90%"
                             ),
                             submitButton(" Apply changes!", icon = icon("tree")),
                             textOutput("result")
                           ),
                           mainPanel(
                             h5("CATE function"),
########################--PUT YOUR EXPLANATION IN HERE--########################
                             
                             p("This Graph plots the true CATE-Function 𝜏 (black) as well as the estimated 𝜏 (red) for different levels of X1. With increased sample size n, the estimated 𝜏 are more close to the true 𝜏."),
                             plotOutput("CatePlot"),
                             h5("Residual regression"),
########################--PUT YOUR EXPLANATION IN HERE--########################
                             p("This graph plots the residuals of W against the residuals of Y, showing how accurate the Causal forest predicts treatment W and outcome Y. Additionally, the size of the residuals is proportional to the weight they receive and the color indicates lower to higher values of X1.
Move the slider bar to have a look at how the weights change for different values of X."),
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
                                          br(),
                                          #textOutput("explanationformula"),
                                          br(),
                                          imageOutput("image2", height = 50),
                                          br(),
                                          br(),
                                          textOutput("explanation3"),
                                          br(),
                                          br(),
                                          textOutput("explanation4"),
                                          br(),
                                          imageOutput("split", height = 50),
                                          br(),
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
                                          h4("DGP 1: 𝜏(x) = 1[x > -0.5π]"), 
                                          p("Here, we can see at x = -0.5π an upwards jump of the true CATE from 0 to 1.
                                            Very close to x = -0.5π, the estimates differ a lot to the true CATEs.
                                            If we move further away from x = -0.5π,
                                            the estimated CATE gets closer to the true CATE.
                                            
                                            Regardless of the chosen treatment propensities, we see that for units with 
                                            small values of x (green points), their outcome model is well fitted. 
                                            Because these units present an underlying true treatment effect of zero
                                            (less variance of y-values), 
                                            this leads to small Y-residuals centered around zero.
                                            For units with a treatment effect of one (orange/red points),
                                            the outcome model is worse fitted, the Y-residuals are higher.
                                            
                                            Depending on the chosen treatment propensities, the W-residuals are centered
                                            around -0.5 & 0.5, or are more dispersed.
                                            
                                            Considering the balanced treatment share:
                                            
                                            For the estimation of the CATEs for small values, x < -1.5,
                                            the residuals of the green-points receive a higher weight
                                            (x falls into the same leaves as the green-units).
                                            The predicted CATEs are around 0.125. Hence, the RORR presents a flat regression-line.
                                            For the estimation of the CATEs for higher values than -1.5,
                                            the residuals of the orange/red-points are weighted more heavily.
                                            The predicted CATEs are close to one and the regression line gets steeper.
                                            
                                            Changing the treatment propensites to a non-constant share, i.e. the step-function, 
                                            we can better see that equally weighted units, hence similar units (fall into the same leaves),  
                                            are controls (with negative W-residuals)
                                            and treated (with positive W-residuals), used for estimating the within-leaves estimate.
                                            The more the controls refer to high negative Y-residuals
                                            and the treated to high positive Y-residuals, 
                                            the steeper the regression line is, and hence the estimated CATE.                                            "),
########################--PUT YOUR EXPLANATION IN HERE--########################
                                          h4("DGP 2: 𝜏(x) = exp(x)"), 
                                          p("The CATE is close to zero for individuals with a low value in x 
                                            and increases exponentially when increasing the value of x, which leads to an
                                            extreme difference between treated and untreated.
                                            
                                            At negative x-values (green points), low values and high values of res_W 
                                            co-occur with low values of res_y, since the treatment effect is close to 
                                            0 in this area and thus incorrect assigning has no big influence (treatment 
                                            and non-treatment don't make a difference concerning the outcome at these points).
                                            
                                            As the x-values start to increase (fading from yellow to red points), low values 
                                            of res_W co-occur with large negative values of res_y and high values of res_W 
                                            co-occur with high values of res_y. This is because mistakes in the prediction 
                                            of the treatment lead to big mistakes in the estimation of the outcome, since 
                                            the difference between treated and untreated gets quite substantial when x gets 
                                            bigger than 0. 
                                            
                                            At the points -3 to -0.5, where the slopes of the true IATE function is small, 
                                            units with low values of x receive most weight. Furthermore, controls (with negative 
                                            treatment residuals) and treated (with positive treatment residuals) of those with 
                                            large weights are quite similar, resulting in a nearly straight line being fitted for 
                                            the particular range. The slope of the line only increases slightly in that range.
                      
                                            The jump from 2.5 to 3 is most instructive. We see that as the slope of the true IATE function
                                            dramtically increases, units with larger values of x receive more weight and since controls and
                                            treated of those with large weights differ much, the line has a strongly positive slope."),
########################--PUT YOUR EXPLANATION IN HERE--########################
                                          h4("DGP 3: 𝜏(x) = 0.3x"),
                                          p("This DGP is defined by having a negative treatment effect for 
                            units with a value of x below 0, meaning the treated have a 
                            negative outcome in this area. An ambivalent treatment could also 
                            me imaginable, hence, it is considered here.
                            We can somewhat see that for estimating the treatment effect for 
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
                            more variance in y.
                            
                            Furthermore, for low values of x the regression line of the
                            residual-on-residual regression is negative because in this
                            particular area the treatment effect is negative, while for high
                            values of x the regression line is positive as the treatment
                            effect is positive. This phenomena occurs due to control 
                            observations with low levels of x having a positive y-residual, 
                            since they deviate upwards, while their treated counterparts
                            deviate downwards. The former show deviance upwards as
                            they are estimated to be somewhere between the estimated 
                            treatment-line in the negative area of the graph and y = 0. 
                            When estimating the treatment effect for a low x, excactly these 
                            observations get a high weight, which results in the negative slope 
                            of the residual-on-residual-regression line. When estimating a 
                            treatment effect for high values of x we can see the opposite 
                            pattern resulting in a positive slope. Described occurence 
                            underlines the symmetry of the results."),
                                          width = "60%"),
                             mainPanel()
                           )
                           
                  ),
                  tabPanel("Fun",
                           setBackgroundImage(
                             src = "https://images-wixmp-ed30a86b8c4ca887773594c2.wixmp.com/f/df03765c-4963-440f-b65c-9d892bd45ba3/dej1kki-36b93e62-9880-4753-a9be-87e585cf380a.png/v1/fill/w_1600,h_900,q_80,strp/blinding_light_by_natozuski_dej1kki-fullview.jpg?token=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1cm46YXBwOjdlMGQxODg5ODIyNjQzNzNhNWYwZDQxNWVhMGQyNmUwIiwiaXNzIjoidXJuOmFwcDo3ZTBkMTg4OTgyMjY0MzczYTVmMGQ0MTVlYTBkMjZlMCIsIm9iaiI6W1t7ImhlaWdodCI6Ijw9OTAwIiwicGF0aCI6IlwvZlwvZGYwMzc2NWMtNDk2My00NDBmLWI2NWMtOWQ4OTJiZDQ1YmEzXC9kZWoxa2tpLTM2YjkzZTYyLTk4ODAtNDc1My1hOWJlLTg3ZTU4NWNmMzgwYS5wbmciLCJ3aWR0aCI6Ijw9MTYwMCJ9XV0sImF1ZCI6WyJ1cm46c2VydmljZTppbWFnZS5vcGVyYXRpb25zIl19.KEKdAkMR4R097tQtgeuBF_DG-mdgj4BLzbLScCBteJY"
                             #"https://thumbs.dreamstime.com/z/creative-shiny-pink-color-background-flat-lay-abstract-decorations-soft-focus-163557328.jpg", shinydashboard = FALSE
                           ),
                           sidebarLayout(
                             mainPanel(imageOutput("image3")),
                             sidebarPanel(
                               h3("a song of praise to shiny"),
                               br(),
                                        p("Verse 1:"),
                                        p("I'm all about that web development"),
                                        p("Making apps that are sleek and inventive"),
                                        p("Shiny's my tool, it's the king of the game"),
                                        p("Creating interfaces that are just insane"),
                                        p("With the power of R, the possibilities are vast"),
                                        p("Gonna make an app that's gonna last"),
                                        br(),
                                        p("Chorus:"),
                                        p("Shiny, shiny, making apps so bright"),
                                        p("User friendly, it's a sight to see at night"),
                                        p("With its widgets and gizmos, you can't go wrong"),
                                        p("Shiny's where it's at, now come along"),
                                        br(),
                                        p("Verse 2:"),
                                        p("I can customize and make it mine"),
                                        p("With Shiny's help, it's one of a kind"),
                                        p("Data visualization, it's a snap"),
                                        p("Interactivity, that's the big rap"),
                                        p("With reactive programming, it's all so smooth"),
                                        p("Making apps with Shiny, it's just the proof"),
                                        br(),
                                        p("Chorus:"),
                                        p("Shiny, shiny, making apps so bright"),
                                        p("User friendly, it's a sight to see at night"),
                                        p("With its widgets and gizmos, you can't go wrong"),
                                        p("Shiny's where it's at, now come along"),
                                        br(),
                                        p("Bridge:"),
                                        p("From dashboards to simulations, it's all the same"),
                                        p("Shiny's got you covered, what's the fuss about the fam"),
                                        p("With its ease of use, it's just a breeze"),
                                        p("Making apps with Shiny, you'll be sure to please"),
                                        br(),
                                        p("Chorus:"),
                                        p("Shiny, shiny, making apps so bright"),
                                        p("User friendly, it's a sight to see at night"),
                                        p("With its widgets and gizmos, you can't go wrong"),
                                        p("Shiny's where it's at, now come along"),
                                        br(),
                                        p("Outro:"),
                                        p("Shiny's the future, it's where it's at"),
                                        p("With its power and grace, it's hard to beat that"),
                                        p("So let's make some apps, it's time to get started"),
                                        p("With Shiny by your side, you'll be the one who's charted."),
                                        br(),
                                        p("by ChatGPT")
                             )
                             
                             ), cellWidths = c("60%", "40%")
                           ),


                  tabPanel("So Shiny Its Blinding",
                           setBackgroundImage(
                             src = "https://images-wixmp-ed30a86b8c4ca887773594c2.wixmp.com/f/df03765c-4963-440f-b65c-9d892bd45ba3/dej1kki-36b93e62-9880-4753-a9be-87e585cf380a.png/v1/fill/w_1600,h_900,q_80,strp/blinding_light_by_natozuski_dej1kki-fullview.jpg?token=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1cm46YXBwOjdlMGQxODg5ODIyNjQzNzNhNWYwZDQxNWVhMGQyNmUwIiwiaXNzIjoidXJuOmFwcDo3ZTBkMTg4OTgyMjY0MzczYTVmMGQ0MTVlYTBkMjZlMCIsIm9iaiI6W1t7ImhlaWdodCI6Ijw9OTAwIiwicGF0aCI6IlwvZlwvZGYwMzc2NWMtNDk2My00NDBmLWI2NWMtOWQ4OTJiZDQ1YmEzXC9kZWoxa2tpLTM2YjkzZTYyLTk4ODAtNDc1My1hOWJlLTg3ZTU4NWNmMzgwYS5wbmciLCJ3aWR0aCI6Ijw9MTYwMCJ9XV0sImF1ZCI6WyJ1cm46c2VydmljZTppbWFnZS5vcGVyYXRpb25zIl19.KEKdAkMR4R097tQtgeuBF_DG-mdgj4BLzbLScCBteJY"
                            , shinydashboard = FALSE
                           )),
                                    # Last Panel
                  tabPanel("About", # panel name
                           br(),
                           br(), # space
                           sidebarPanel(
                             p("This app has been created as a group project by Maren Baumgärtner, Sophia Herrmann, Kevin Kopp, Alexandros Parginos Dös and Stella Rotter for the course \" DS407 Causal Machine Learing\"."
                               ),
                             br(),
                             tags$a(href="https://github.com/marenbaumgaertner/causal-forest-application.git", "Get the Code!"),
                             br(),
                             br(),
                             p("In case you have any further questions,
feel free to contact us via"), em("maren.baumgaertner@student.uni-tuebingen.de"), width = "60%"
                             
                             
                  )),
                  # colors
                  setSliderColor(c("#7e997b"),1), # #6495ed HEX code für cornflowerblue
                  setBackgroundColor(
                    color = "#FAF9F6",
                    shinydashboard = FALSE
                  )
                ))
