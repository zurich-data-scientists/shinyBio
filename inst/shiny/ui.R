############################# Shiny app #############################
## We set the page
ui <- fluidPage(
  introjsUI(), ## intro boxes
  navlistPanel(widths = c(2, 10),
               id = "tabset1",
               "Introduction",
               
               ## Introduction ---------------------------------------------------------------
               tabPanel("Introduction",
                        tags$h3("Introduction"),
                        htmlOutput("story"),
                        imageOutput("cornfield.image"),
                        htmlOutput("story2")),
               
               "Linear models",
               
               
               ## First panel - Linear model 1 continuous, 1 factor ---------------------------
               
               tabPanel("1. Linear model",
                        tags$h3("Linear model"),
                        tags$p(HTML("In this panel <em>Cob_weight</em> (response variable)
                        is explained by a linear model with 
                        one continuous variable (<em>Fertilizer</em>)
                               and one categorical variable (<em>Site</em>).")),
                        tags$p("The effect of the explanatory variables is additive."),
                        br(),
                        tags$h4("Model formula"),
                        tags$p(HTML("Cob_weight ~ Fertilizer + Site")),
                        br(),
                        ## Choose the values
                        fluidRow(column(width = 3,
                                        tags$h5(HTML("Choose the <em>Fertilizer</em>  effect")),
                                        sliderInput("slope.lm1",
                                                    label = " ",
                                                    value = slope.scenario1.lm["Zurich"],
                                                    min = 0,
                                                    max = 20,
                                                    step = 0.1,
                                                    ticks = FALSE),
                                        br(),
                                        tags$h5(HTML("Choose the <em>Site</em> effect")),
                                        sliderInput("intercept.lm.lausanne1",
                                                    label = "Lausanne",
                                                    value = intercept.scenario1.lm["Lausanne"],
                                                    min = 0,
                                                    max = 50,
                                                    step = 0.1,
                                                    ticks = FALSE),
                                        sliderInput("intercept.lm.locarno1",
                                                    label = "Locarno",
                                                    value = intercept.scenario1.lm["Locarno"],
                                                    min = 0,
                                                    max = 50,
                                                    step = 0.1,
                                                    ticks = FALSE),
                                        sliderInput("intercept.lm.zurich1",
                                                    label = "Zurich",
                                                    value = intercept.scenario1.lm["Zurich"],
                                                    min = 0,
                                                    max = 50,
                                                    step = 0.1,
                                                    ticks = FALSE),
                                        
                                        br(),
                                        sliderInput("sigma.lm1", 
                                                    label = HTML(paste0("<h5>",
                                                                        "Choose the standard deviation of the error &sigma;<sub>&epsilon;</sub>", 
                                                                        "<h5>")), 
                                                    min = 0, 
                                                    max = 20, 
                                                    value = 1,
                                                    step = 0.1,
                                                    ticks = FALSE),
                                        
                                        
                        ), ## End column1
                        
                        ## Marginal plot
                        column(width = 9,
                               tags$h4("Marginal plots"),
                               br(),
                               "The following plots only consider one predictor at the time.",
                               br(),
                               br(),
                               plotOutput("marginal.plot.lm1"),
                               tags$li(class = "dropdown",
                                       dropMenu(
                                         dropdownButton("Info", 
                                                        status = 'success', 
                                                        icon = icon('info')),
                                         size = "xs",
                                         br(),
                                         h5(paste0("On the marginal graphs we use",
                                                   " smoothers to avoid making 
                                                   the linearity assumption")),
                                         placement = "bottom",
                                         arrow = FALSE
                                       )),
                               htmlOutput("explanation.marginal.plot"),
                               plotOutput("marginal.plots.together"),
                               br(),
                               tags$p("Colours are used in these plot to inspect 
                                      whether the categorical variable site interacts 
                                      with the continuous predictor Fertilizer.")
                               
                        ) ## End column2
                        
                        ), ## End fluidRow
                        
                        
                        br(),
                        
                        ## Summary
                        htmlOutput("summary.title1"),
                        verbatimTextOutput("model.lm1"),
                        br(),
                        tags$h4("Model equation"),
                        introBox(
                          introBox(
                            introBox(
                              introBox(
                                introBox(
                                  introBox(
                                    introBox(
                                      introBox(
                                        introBox(
                                          htmlOutput("model.equation.1"),
                                          data.step = 1,
                                          data.intro = paste0(HTML('&beta;<sub>0</sub>'), 
                                                              " represents the intercept of 
                                                            the linear regression. <br>
                                                            In particular, it represents 
                                                            the Cob Weight in the reference Site
                                                            (i.e. Lausanne) when no Fertilizer is used.") #,
                                          #data.hint = "If desired, we can add like this",
                                          #data.position = "bottom-left_aligned"
                                        ),
                                        data.step = 2,
                                        data.intro = paste0(HTML('&beta;<sub>Fertilizer</sub>'),
                                                            " is the coefficient that estimates
                                                  the quantity of change in Cob 
                                                            Weight per unit change in Fertilizer.") 
                                      ),
                                      data.step = 3,
                                      data.intro = paste0(" x<sub>Fertilizer<sub>i</sub></sub> 
                                                represents the quantity of Fertilizer 
                                                for the observation <em>i</em>.") 
                                    ),
                                    data.step = 4,
                                    data.intro = paste0(HTML('&beta;<sub>LOC</sub>'),
                                                        " is the coefficient estimating the difference
                                                            between the reference Site and Locarno. <br>", 
                                                        HTML('&beta;<sub>0</sub>'), " + ", HTML('&beta;<sub>LOC</sub>'), 
                                                        " indicates the Cob Weight in Locarno when no Fertilizer is used.") 
                                  ),
                                  data.step = 5,
                                  data.intro = paste0("I<sub>LOC</sub> indicates whether or not
                                    the site of observation <em>i</em> corresponds to Locarno. <br>
                                                                I<sub>LOC<sub>i</sub></sub> = 1 
                                                        if the Site of observation <em>i</em> is Locarno, 
                                                        I<sub>LOC</sub> = 0 otherwise.") 
                                ),
                                data.step = 6,
                                data.intro = paste0(HTML('&beta;<sub>ZH</sub>'),
                                                    " is the coefficient estimating the difference
                                                            between the reference Site and Zurich. <br>", 
                                                    HTML('&beta;<sub>0</sub>'), 
                                                    " + ",
                                                    HTML('&beta;<sub>ZH</sub>'), 
                                                    " indicates the Cob Weight in 
                                                      Zurich when no Fertilizer is used.") 
                              ),
                              data.step = 7,
                              data.intro = paste0("I<sub>ZH<sub>i</sub></sub> 
                                indicates whether or not the site of observation 
                                <em>i</em> corresponds to Zurich. <br> I<sub>ZH<sub>i</sub></sub> = 1
                                                    if the Site of observation <em>i</em> is Zurich, 
                                                    I<sub>ZH<sub>i</sub></sub> = 0 otherwise.") 
                            ),
                            data.step = 8,
                            data.intro = paste0("<em>&epsilon;<sub>i</sub></em> is the residual term. <br>
                                            It represents the difference between the observed and 
                                                the estimated value for the response variable. <br>
                                                This quantity correspond to the vertical distance 
                                                between the observed value (i.e. the actual observation) 
                                                and the regression line.") 
                          ),
                          data.step = 9,
                          data.intro = paste0("<em>e</em> is the error term. <br>
                                            It captures the variability in the response variable 
                                            that is not explained by the regression equation.") 
                        ),
                        br(),
                        actionButton("help", label = "Explanation"),
                        br(),
                        br(),
                        
                        ## Plot model
                        htmlOutput("plot.title1"),
                        plotOutput("plot.lm1"),
                        checkboxInput("observed.values.lm1", 
                                      "Add observed values",
                                      FALSE),
                        br(),
                        tags$p(HTML("For such a simple model 
                                    (one continuous and one categorical variable as predictor) 
                                    it is possible to show the effect of all predictors contained 
                                    in the model on one plot. Indeed, the x-axis enables us to show 
                                    the effect of the continuous predictor <em>Fertilizer</em>, while colours 
                                    highlight the effect of the categorical predictor <em>Site</em>.")),
                        br(),
                        tags$p(HTML("For such a simple model, the fitted values lay on three lines, 
                                    one for each site. As there is no interaction in this model, 
                                    all sites have the same slope, which means that all three lines run parallel.")),
                        br(),
                        tags$p(HTML("For ore complex models 
                                    (e.g. models that contain two continuous predictors and one categorical) 
                                    such a simple and clear visualisation is not possible anymore.
                                    Indeed, in reality most models do contain several predictors, and therefore this
                                    very clear and simple visualisation of the fitted values is not possible. 
                                    In the next panels of this Shiny App we will show cases with more predictors.")),
                        br(),
                        tags$p(HTML("Adding the observed values to the plots gives us an idea of how much noise
                                    is present in the data. If the observations are very close to the three regression lines,
                                    then the noise is relatively small. If the observed values are scattered a lot around 
                                    the three fitted values lines, then the noise in the data is relatively large.
                                    You can change the standard deviation of the error to appreciate different situations.")),
                        br(),
                        
                        ## Diagnostic plots
                        bsCollapse(id = "collapsible.lm1",
                                   bsCollapsePanel("Diagnostic plots", 
                                                   "This panel shows",
                                                   "the diagnostic plots for a linear model",
                                                   plotOutput("plot.diagnostic.lm1")) #,
                                   
                                   # bsCollapsePanel("TA-plot", "This panel shows",
                                   #                 "the TA-plot for a linear model",
                                   #                 plotOutput("plot.residuals1")),
                                   # bsCollapsePanel("QQplot", "This panel shows",
                                   #                 "the QQ-plot for a linear model",
                                   #                 plotOutput("qqplot.lm1"))
                        ),
                        actionButton("openPlotButton_panel1", "Zoom Plot"),
                        uiOutput("modalUI_panel1"), 
                        br()
                        
                        
               ), ## End of first panel / tabPanel()
               
               
               ## 1.5 Panel - 2 continuous, 1 factor ------------------------------------------
               
               tabPanel("2. Linear model",
                        tags$h3("Linear model"),
                        tags$p(HTML("In this panel <em>Cob_weight</em> (response variable)
                        is explained by a linear model with 
                        two continuous variables (<em>Fertilizer</em> and <em>Plant_height</em>),
                               and one categorical variable (<em>Site</em>).")),
                        tags$p(HTML("<em>Plant_height</em> is log transformed 
                               due to its skeweness.")),
                        tags$p(HTML("The effect of the explanatory variables is additive.")),
                        br(),
                        tags$h4("Model formula"),
                        tags$p(HTML("Cob_weight ~ Fertilizer + 
                                    Site
                        <span style='color: red;'>+ log(Plant_height)</span>" )),
                        br(),
                        ## Choose the values
                        fluidRow(column(width = 3,
                                        tags$h5(HTML("Choose the <em>Fertilizer</em> effect")),
                                        sliderInput("slope.lm1.5",
                                                    label = "Fertilizer",
                                                    value = slope.scenario1.lm["Zurich"],
                                                    min = 0,
                                                    max = 20,
                                                    step = 0.1,
                                                    ticks = FALSE),
                                        
                                        br(),
                                        tags$h5(HTML("Choose the <em>Site</em> effect")),
                                        sliderInput("intercept.lm.lausanne1.5",
                                                    label = "Lausanne",
                                                    value = intercept.scenario1.lm["Lausanne"],
                                                    min = 0,
                                                    max = 50,
                                                    step = 0.1,
                                                    ticks = FALSE),
                                        sliderInput("intercept.lm.locarno1.5",
                                                    label = "Locarno",
                                                    value = intercept.scenario1.lm["Locarno"],
                                                    min = 0,
                                                    max = 50,
                                                    step = 0.1,
                                                    ticks = FALSE),
                                        sliderInput("intercept.lm.zurich1.5",
                                                    label = "Zurich",
                                                    value = intercept.scenario1.lm["Zurich"],
                                                    min = 0,
                                                    max = 50,
                                                    step = 0.1,
                                                    ticks = FALSE),
                                        br(),
                                        tags$h5(HTML("Choose the <em>Plant_height</em> effect")),
                                        sliderInput("slope.lm.height1",
                                                    label = "",
                                                    value = slope.scenario1.lm.height["Lausanne"],
                                                    min = 1.20,
                                                    max = 5,
                                                    step = 0.1,
                                                    ticks = FALSE),
                                        br(),
                                        sliderInput("sigma.lm1.5",
                                                    label = HTML(paste0("<h5>",
                                                                        "Choose the standard deviation of the error &sigma;<sub>&epsilon;</sub>",
                                                                        "<h5>")),
                                                    min = 0,
                                                    max = 20,
                                                    value = 1,
                                                    step = 0.1,
                                                    ticks = FALSE),
                                        # actionButton("simulate",
                                        #  label = "Simulate!"),
                                        
                        ), ## End column1
                        
                        ## Marginal plot
                        column(width = 9,
                               tags$h4("Marginal plots"),
                               plotOutput("marginal.plot.first.draft.lm1.5"),
                               tags$li(class = "dropdown",
                                       dropMenu(
                                         dropdownButton("Info",
                                                        status = 'success',
                                                        icon = icon('info')),
                                         size = "xs",
                                         br(),
                                         h5(paste0("On the marginal graphs we use",
                                                   " smoothers to avoid making the ",
                                                   "linearity assumption")),
                                         placement = "bottom",
                                         arrow = FALSE)),
                               br(),
                               htmlOutput("explanation.log"),
                               br(),
                               htmlOutput("explanation.not.variables.together"),
                               plotOutput("marginal.plot.lm1.5"),
                               tags$p("Colours are used in these plot to inspect 
                                      whether the categorical variable site interacts 
                                      with the continuous predictor Fertilizer.")
                               
                               
                        ) ## End column2
                        
                        ), ## End fluidRow
                        
                        br(),
                        
                        ## Summary
                        htmlOutput("summary.title1.5"),
                        verbatimTextOutput("model.lm1.5"),
                        br(),
                        tags$h4("Model equation"),
                        htmlOutput("model.equation.2"),
                        br(),
                        br(),
                        
                        ## Plot model
                        htmlOutput("plot.title1.5"),
                        plotOutput("plot.lm1.5"),
                        checkboxInput("observed.values.lm1.5",
                                      "Add observed values",
                                      FALSE),
                        tags$p(HTML('As discussed in the previous tab of this shiny
                        (ie. "1. linear model") we see on the two graphs above 
                        that the fitted values do not all fall nicely on a line.
                        The reason is that the model now contains two continuous 
                        variables and one categorical.')),
                        tags$p(HTML("Comparing the two plots above, 
                                    we can also note that <em>Fertilizer</em>
                                    seems to have a greater influence on the response variable. 
                                    This is confirmed by the model summary output (compare the t-values).")),
                        
                        br(),
                        #useShinyjs(),
                        
                        ## Diagnostic plots
                        bsCollapse(id = "collapsible.lm1.5",
                                   bsCollapsePanel("Diagnostic plots",
                                                   "This panel shows",
                                                   "the diagnostic plots for a linear model",
                                                   plotOutput("plot.diagnostic.lm1.5")) #,
                                   # bsCollapsePanel("TA-plot", "This panel shows",
                                   #                 "the TA-plot for a linear model",
                                   #                 plotOutput("plot.residuals1.5")),
                                   # bsCollapsePanel("QQplot", "This panel shows",
                                   #                 "the QQ-plot for a linear model",
                                   #                 plotOutput("qqplot.lm1.5"))
                        ),
                        
                        actionButton("openPlotButton_panel1.5", "Zoom Plot"),
                        uiOutput("modalUI_panel1.5"),
                        br()
               ), ## End of 1.5 panel / tabPanel()
               
               
               
               
               
               ## Second panel - Linear model 2 continuous, 2 factors -------------------------
               
               tabPanel("3. Linear model",
                        tags$h3("Linear model"),
                        tags$p(HTML("In this panel <em>Cob_weight</em> (response variable)
                        is explained by a linear model with 
                        two continuous variables (<em>Fertilizer</em> and <em>Plant_height</em>),
                               and two categorical variables, <em>Site</em> and <em>Variety.fac</em>.
                               <em>Variety.fac</em> has two levels, representing two cob varieties.")),
                        tags$p(HTML("As before, <em>Plant_height</em> is log transformed due to its skeweness.")),
                        tags$p(HTML("The effect of the explanatory variables is additive.")),
                        br(),
                        tags$h4("Model formula"),
                        tags$p(HTML("Cob_weight ~ Fertilizer + Site + 
                                    log(Plant_height) <span style='color: red;'>+ Variety.fac</span>")),
                        br(),
                        ## Choose the values
                        fluidRow(column(width = 3,
                                        tags$h5(HTML("Choose the <em>Fertilizer</em> effect")),
                                        sliderInput("slope.lm2",
                                                    label = "Fertilizer",
                                                    value = slope.scenario1.lm["Zurich"],
                                                    min = 0,
                                                    max = 20,
                                                    step = 0.1,
                                                    ticks = FALSE),
                                        br(),
                                        
                                        tags$h5(HTML("Choose the <em>Plant_height</em> effect")),
                                        sliderInput("slope.lm.height",
                                                    label = "Plant height",
                                                    value = slope.scenario1.lm.height["Lausanne"],
                                                    min = 1.20,
                                                    max = 5,
                                                    step = 0.1,
                                                    ticks = FALSE),
                                        br(),
                                        tags$h5(HTML("Choose the <em>Site</em> effect")),
                                        sliderInput("intercept.lm.lausanne2",
                                                    label = "Lausanne",
                                                    value = intercept.scenario1.lm["Lausanne"],
                                                    min = 0,
                                                    max = 50,
                                                    step = 0.1,
                                                    ticks = FALSE),
                                        sliderInput("intercept.lm.locarno2",
                                                    label = "Locarno",
                                                    value = intercept.scenario1.lm["Locarno"],
                                                    min = 0,
                                                    max = 50,
                                                    step = 0.1,
                                                    ticks = FALSE),
                                        sliderInput("intercept.lm.zurich2",
                                                    label = "Zurich",
                                                    value = intercept.scenario1.lm["Zurich"],
                                                    min = 0,
                                                    max = 50,
                                                    step = 0.1,
                                                    ticks = FALSE),
                                        br(),
                                        # sliderInput("intercept.scenario1.variety",
                                        #             label = "Variety of cob",
                                        #             value = intercept.scenario1.variety["Lausanne"],
                                        #             min = -20,
                                        #             max = 0,
                                        #             step = 0.1,
                                        #             ticks = FALSE),
                                        # radioButtons("intercept.scenario1.variety", 
                                        #              label = "Presence of two cob varieties?",
                                        #              choices = c("No", "Yes"),
                                        #              inline = TRUE),
                                        br(),
                                        sliderInput("sigma.lm2", 
                                                    label = HTML(paste0("<h5>",
                                                                        "Choose the standard deviation of the error &sigma;<sub>&epsilon;</sub>", 
                                                                        "<h5>")), 
                                                    min = 0, 
                                                    max = 20, 
                                                    value = 1,
                                                    step = 0.1,
                                                    ticks = FALSE),
                                        
                                        # actionButton("simulate", 
                                        #  label = "Simulate!"),
                                        
                        ), ## End column1
                        
                        ## Marginal plot
                        column(width = 9,
                               tags$h4("Marginal plots"),
                               plotOutput("marginal.plot.lm2"),
                               
                               tags$li(class = "dropdown",
                                       dropMenu(
                                         dropdownButton("Info", 
                                                        status = 'success', 
                                                        icon = icon('info')),
                                         size = "xs",
                                         br(),
                                         h5(paste0("On the marginal graph we use",
                                                   " smoothers to avoid making the ", 
                                                   "linearity assumption")),
                                         placement = "bottom",
                                         arrow = FALSE)),
                               br(),
                               htmlOutput("explanation.log.3"),
                               br(),
                               htmlOutput("explanation.plots.together3"),
                               br(),
                               plotOutput("marginal.plots.together3"),
                               tags$p("Colours are used in these plot to inspect 
                                      whether the categorical variable site interacts 
                                      with the continuous predictor Fertilizer.")
                               
                        ) ## End column2
                        
                        ), ## End fluidRow
                        
                        br(),
                        
                        ## Summary
                        htmlOutput("summary.title2"),
                        verbatimTextOutput("model.lm2"),
                        br(),
                        tags$h4("Model equation"),
                        htmlOutput("model.equation.3"),
                        br(),
                        br(),
                        
                        ## Plot model
                        htmlOutput("plot.title2"),
                        plotOutput("plot.lm2"),
                        checkboxInput("observed.values.lm2",
                                      "Add observed values", 
                                      FALSE),
                        br(),
                        #useShinyjs(),
                        
                        ## Diagnostic plots
                        bsCollapse(id = "collapsible.lm2",
                                   bsCollapsePanel("Diagnostic plots", 
                                                   "This panel shows",
                                                   "the diagnostic plots for a linear model",
                                                   plotOutput("plot.diagnostic.lm2")) #,
                                   # bsCollapsePanel("TA-plot", "This panel shows",
                                   #                 "the TA-plot for a linear model",
                                   #                 plotOutput("plot.residuals2")),
                                   # bsCollapsePanel("QQplot", "This panel shows",
                                   #                 "the QQ-plot for a linear model",
                                   #                 plotOutput("qqplot.lm2"))
                        ),
                        actionButton("openPlotButton_panel2", "Zoom Plot"),
                        uiOutput("modalUI_panel2"),
                        br()
                        
                        
               ), ## End of second panel / tabPanel()
               
               
               
               
               ## Third Panel - Linear model with interactions --------------------------------
               
               tabPanel("4. Linear model with interactions",
                        tags$h3("Linear model with interactions"),
                        tags$p(HTML("In this panel <em>Cob_weight</em> (response variable)
                        is explained by a linear model with 
                        one continuous variable (<em>Fertilizer</em>),
                               and one categorical variable (<em>Site</em>).")),
                        tags$p("An interaction between the two variables is supposed."),
                        br(), 
                        tags$h4("Model formula"),
                        tags$p(HTML("Cob_weight ~ Fertilizer <span style='color: red;'>*</span> Site")),
                        br(),
                        
                        fluidRow(column(width = 3,
                                        tags$h5(HTML("Choose the <em>Fertilizer</em> effect")),
                                        tags$p(HTML("The initial coefficients are without interaction")),
                                        sliderInput("slope.lm.lausanne.inter",
                                                    label = "In Lausanne",
                                                    value = slope.scenario1.lm.inter["Lausanne"],
                                                    min = 0,
                                                    max = 20,
                                                    step = 0.1,
                                                    ticks = FALSE),
                                        sliderInput("slope.lm.locarno.inter",
                                                    label = "In Locarno",
                                                    value = slope.scenario1.lm.inter["Locarno"],
                                                    min = 0,
                                                    max = 20,
                                                    step = 0.1,
                                                    ticks = FALSE),
                                        sliderInput("slope.lm.zurich.inter",
                                                    label = "In Zurich",
                                                    value = slope.scenario1.lm.inter["Zurich"],
                                                    min = 0,
                                                    max = 20,
                                                    step = 0.1,
                                                    ticks = FALSE),
                                        br(),
                                        tags$h5(HTML("Choose the <em>Site</em> effect")),
                                        sliderInput("intercept.lm.lausanne.inter",
                                                    label = "Lausanne",
                                                    value = intercept.scenario1.lm.inter["Lausanne"],
                                                    min = 0,
                                                    max = 50,
                                                    step = 0.1,
                                                    ticks = FALSE),
                                        sliderInput("intercept.lm.locarno.inter",
                                                    label = "Locarno",
                                                    value = intercept.scenario1.lm.inter["Locarno"],
                                                    min = 0,
                                                    max = 50,
                                                    step = 0.1,
                                                    ticks = FALSE),
                                        sliderInput("intercept.lm.zurich.inter",
                                                    label = "Zurich",
                                                    value = intercept.scenario1.lm["Zurich"],
                                                    min = 0,
                                                    max = 50,
                                                    step = 0.1,
                                                    ticks = FALSE),
                                        br(),
                                        sliderInput("sigma.lm.inter", 
                                                    label = HTML(paste0("<h5>", 
                                                                        "Choose the standard deviation of the error &sigma;<sub>&epsilon;</sub>", 
                                                                        "<h5>")),
                                                    min = 0, 
                                                    max = 20, 
                                                    value = 1,
                                                    step = 0.1,
                                                    ticks = FALSE),
                                        # actionButton("simulate.inter", 
                                        #  label = "Simulate!")
                        ), ## End column1
                        
                        
                        column(width = 9,
                               tags$h4("Marginal plots"),
                               plotOutput("marginal.plot.lm.inter"),
                               tags$p("In this specific situation, the variable <em>Fertilizer</em> can only take positive values. 
                                      However, in a general situation, it is possible for negative lines to appear"),
                               tags$li(class = "dropdown",
                                       dropMenu(
                                         dropdownButton("Info", 
                                                        status = 'success', 
                                                        icon = icon('info')),
                                         size = "xs",
                                         br(),
                                         h5(paste0("On the marginal graph we use",
                                                   " smoothers to avoid making the linearity assumption")),
                                         placement = "bottom",
                                         arrow = FALSE)),
                               br(),
                               htmlOutput("explanation.plots.together.inter"),
                               br(),
                               plotOutput("marginal.plots.together.inter"),
                               tags$p("Colours are used in these plot to inspect 
                                      whether the categorical variable site interacts 
                                      with the continuous predictor Fertilizer.")
                               
                        ) ## End column2
                        
                        ), ## End fluidRow
                        
                        br(),
                        htmlOutput("summary.title.inter"),
                        verbatimTextOutput("model.lm.inter"),
                        br(),
                        
                        tags$h4("Model equation"),
                        htmlOutput("model.equation.4"),
                        br(),
                        br(),
                        
                        htmlOutput("plot.title.inter"),
                        plotOutput("plot.lm.inter"),
                        checkboxInput("observed.values.lm.inter", 
                                      "Add observed values", 
                                      FALSE),
                        br(),
                        #useShinyjs(),
                        bsCollapse(id = "collapsible.lm.inter",
                                   bsCollapsePanel("Diagnostic plots", 
                                                   "This panel shows",
                                                   "the diagnostic plots for a linear model",
                                                   plotOutput("plot.diagnostic.lm.inter")) #,
                                   # bsCollapsePanel("TA-plot", "This panel shows",
                                   #                 "the TA-plot for a linear model",
                                   #                 "with interactions",
                                   #                 plotOutput("plot.residuals.inter")),
                                   # bsCollapsePanel("QQplot", "This panel shows",
                                   #                 "the QQ-plot for a linear model",
                                   #                 "with interactions",
                                   #                 plotOutput("qqplot.lm.inter"))
                        ),
                        actionButton("openPlotButton_panel3", "Zoom Plot"),
                        uiOutput("modalUI_panel3"),
                        br()
                        
               ), ## End of third panel / tabPanel()
               
               
               
               ## Forth panel - Linear model with non-linear effects --------------------------
               tabPanel("5. Linear model with non-linear effects (quadratic example)",
                        tags$h3("Linear model with quadratic effect"),
                        tags$p(HTML("In this panel <em>Cob_weight</em> (response variable)
                        is explained by a linear model with 
                        one continuous variable (<em>Fertilizer</em>),
                               and one categorical variable (<em>Site</em>).")),
                        tags$p(HTML("We suppose that <em>Fertilizer</em> has a
                               quadratic effect on the response variable.")),
                        tags$p(HTML("The effect of the explanatory variables is additive.")),
                        br(),
                        tags$h4("Model Formula"),
                        tags$p(HTML("Cob_weight ~ Fertilizer + 
                                    Site <span style='color: red;'>+ I(Fertilizer^2)</span>")),
                        br(),
                        fluidRow(
                          column(width = 3,
                                 tags$h5(HTML("Choose the <em>Fertilizer</em> effect")),
                                 sliderInput("slope.lm.x.quad",
                                             label = "Linear",
                                             value = slope.x.scenario1.lm.quad["Lausanne"],
                                             min = 5,
                                             max = 10,
                                             step = 0.01,
                                             ticks = FALSE),
                                 br(),
                                 sliderInput("slope.lm.x.sq.quad",
                                             label = "Quadratic",
                                             value = slope.x.sq.scenario1.lm.quad["Lausanne"],
                                             min = -0.1,
                                             max = 0,
                                             step = 0.0001,
                                             ticks = FALSE),
                                 br(),
                                 tags$h5(HTML("Choose the <em>Site</em> effect")),
                                 sliderInput("intercept.lm.lausanne.quad",
                                             label = "Lausanne",
                                             value = intercept.scenario1.lm.quad["Lausanne"],
                                             min = 0,
                                             max = 50,
                                             step = 0.1,
                                             ticks = FALSE),
                                 sliderInput("intercept.lm.locarno.quad",
                                             label = "Locarno",
                                             value = intercept.scenario1.lm.quad["Locarno"],
                                             min = 0,
                                             max = 50,
                                             step = 0.1,
                                             ticks = FALSE),
                                 sliderInput("intercept.lm.zurich.quad",
                                             label = "Zurich",
                                             value = intercept.scenario1.lm.quad["Zurich"],
                                             min = 0,
                                             max = 50,
                                             step = 0.1,
                                             ticks = FALSE),
                                 br(),
                                 sliderInput("sigma.lm.quad", 
                                             label = HTML(paste0("<h5>",
                                                                 "Choose the standard deviation of the error &sigma;<sub>&epsilon;</sub>", 
                                                                 "</h5>")),
                                             min = 0, 
                                             max = 20, 
                                             value = 1,
                                             step = 0.1,
                                             ticks = FALSE)), ## End column 1
                          
                          column(width = 9,
                                 tags$h4("Marginal plots"),
                                 plotOutput("marginal.plot.lm.quad"),
                                 tags$li(class = "dropdown",
                                         dropMenu(
                                           dropdownButton("Info", 
                                                          status = 'success', 
                                                          icon = icon('info')),
                                           size = "xs",
                                           br(),
                                           h5(paste0("On the marginal graph we use",
                                                     " smoothers to avoid making the ", 
                                                     "linearity assumption")),
                                           placement = "bottom",
                                           arrow = FALSE)),
                                 br(),
                                 htmlOutput("explanation.plots.together.quad"),
                                 plotOutput("marginal.plots.together.quad"),
                                 tags$p("Colours are used in these plot to inspect
                                        whether the categorical variable site interacts 
                                        with the continuous predictor Fertilizer.")
                          )
                          
                          
                          
                          
                        ), ## End fluidRow
                        br(),
                        htmlOutput("summary.title.quad"),
                        verbatimTextOutput("model.lm.quad"),
                        br(),
                        
                        tags$h4("Model equation"),
                        htmlOutput("model.equation.5"),
                        br(),
                        tags$h5("Explanation of the quadratic effect"),
                        tags$p(HTML("If the curve has a concave shape, it indicates that
                                    the coefficient for the quadratic effect is negative.")),
                        tags$p(HTML("Additionally, the narrower the curve is, 
                                    the larger the coefficient becomes in absolute value.")),
                        tags$p(HTML("A linear effect is reflected as a tilt of the curve.")),
                        br(),
                        htmlOutput("plot.title.quad"),
                        plotOutput("plot.lm.quad"),
                        checkboxInput("observed.values.lm.quad", 
                                      "Add observed values", 
                                      FALSE),
                        br(),
                        #useShinyjs(),
                        bsCollapse(id = "collapsible.lm.quad",
                                   bsCollapsePanel("Diagnostic plots", 
                                                   "This panel shows",
                                                   "the diagnostic plots for a linear model",
                                                   plotOutput("plot.diagnostic.lm.quad")) #,
                                   # bsCollapsePanel("TA-plot", "This panel shows",
                                   #                 "the TA-plot for a linear model",
                                   #                 "with quadratic effects",
                                   #                 plotOutput("plot.residuals.quad")),
                                   # bsCollapsePanel("QQplot", "This panel shows",
                                   #                 "the QQ-plot for a linear model",
                                   #                 "with quadratic effects",
                                   #                 plotOutput("qqplot.lm.quad"))
                        ),
                        actionButton("openPlotButton_panel4", "Zoom Plot"),
                        uiOutput("modalUI_panel4"),
                        br()
               ), ## End forth panel 
               
               
               
               "Generalised linear models",
               
               
               ## Sixth panel - Generalised linear model - binomial ---------------------------
               
               tabPanel("6. Generalised linear model - Binomial",
                        tags$h3("Generalised linear model - Binomial"),
                        tags$p(HTML("In this panel, the variable <em>Cob_pres</em>
                        is considered as the response variable;
                        <em>Cob_pres</em> indicates whether the plant germinated or not.")),
                        tags$p(HTML("<em>Cob_pres</em> is explained by a generalised linear model 
                        with logit link function. 
                        The logit link function captures the relationship between 
                        the explanatory variables and the log-odds of <em>Cob_pres</em>.")), 
                        tags$p(HTML("The explanatory variables are, as before,
                                    <em>Fertilizer</em> (continuous) and <em>Site</em> (categorical).")),
                        tags$p(HTML("The effect of the explanatory variables is additive.")),
                        tags$p(HTML("The response variable is binary; therefore the binomial
                                    family is used in the model.")),
                        tags$p(HTML("The quasibinomial family is not even taken into consideration. The reason is that
                        this family is only considered when the response data is binomial.")),
                        br(),
                        tags$h4("Model formula"),
                        tags$p(HTML("Cob_pres ~ Fertilizer + Site, 
                                    <span style='color: red;'>family = binomial</span>")),
                        br(),
                        fluidRow(
                          column(width = 3,
                                 sliderInput("slope.glm.bin",
                                             label = HTML("<h5> Choose the <em>Fertilizer</em> effect </h5>"),
                                             value = slope.scenario1.glm.bin["Lausanne"],
                                             min = 0,
                                             max = 50,
                                             step = 0.1,
                                             ticks = FALSE),
                                 br(),
                                 tags$h5(HTML("Choose the <em>Site</em> effect")),
                                 sliderInput("intercept.glm.lausanne.bin",
                                             label = "Lausanne",
                                             value = intercept.scenario1.glm.bin["Lausanne"],
                                             min = 0,
                                             max = 20,
                                             step = 0.1,
                                             ticks = FALSE),
                                 sliderInput("intercept.glm.locarno.bin",
                                             label = "Locarno",
                                             value = intercept.scenario1.glm.bin["Locarno"],
                                             min = 0,
                                             max = 20,
                                             step = 0.1,
                                             ticks = FALSE),
                                 
                                 sliderInput("intercept.glm.zurich.bin",
                                             label = "Zurich",
                                             value = intercept.scenario1.glm.bin["Zurich"],
                                             min = 0,
                                             max = 20,
                                             step = 0.1,
                                             ticks = FALSE),
                          ), ## End column1
                          
                          column(width = 9,
                                 tags$h4("Marginal plots"),
                                 plotOutput("marginal.plot.glm.bin"),
                                 tags$li(class = "dropdown",
                                         dropMenu(
                                           dropdownButton("Info", 
                                                          status = 'success', 
                                                          icon = icon('info')),
                                           size = "xs",
                                           br(),
                                           h5(paste0("On the marginal graph we use",
                                                     " smoothers to avoid making the ", 
                                                     "linearity assumption")),
                                           placement = "bottom",
                                           arrow = FALSE)),
                                 br(),
                                 htmlOutput("explanation.plots.together.bin"),
                                 plotOutput("marginal.plots.together.bin"),
                                 tags$p("Colours are used in these plot to inspect 
                                        whether the categorical variable site interacts 
                                        with the continuous predictor Fertilizer.")
                                 
                          ) ## End column2
                          
                          
                        ), ## End Fluidrow
                        br(),
                        htmlOutput("summary.title.glm.bin"),
                        verbatimTextOutput("model.glm.bin"),
                        br(),
                        tags$h4("Model equation"),
                        htmlOutput("model.equation.6"),
                        br(),
                        br(),
                        htmlOutput("plot.title.glm.bin"),
                        plotOutput("plot.glm.bin"),
                        #checkboxInput("observed.values.glm", 
                        #              "Add observed values", 
                        #              FALSE),
                        #useShinyjs(),
                        # bsCollapse(id = "collapsible.glm.bin",
                        #            bsCollapsePanel("TA-plot", "This panel shows",
                        #                            "the TA-plot for a generalised linear",
                        #                            "model with binary response",
                        #                            plotOutput("plot.residuals.bin"))
                        #            
                        # ),
                        
                        
               ), ## End of sixth Panel
               
               
               
               ## Seventh Panel - Generalised linear model with interactions - binomial -------
               tabPanel("7. Generalised linear model with interactions - Binomial",
                        tags$h3("Generalised linear model with interactions - Binomial"),
                        tags$p(HTML("In this panel <em>Cob_pres</em>
                        is explained by a generalised linear model with logit link function. 
                        The explanatory variables are <em>Fertilizer</em> and <em>Site</em>.")),
                        
                        tags$p("An interaction between the variables is supposed"),
                        tags$h4("Model formula"),
                        tags$p(HTML("Cob_pres ~ Fertilizer <span style = 'color: red;'>*</span> Site, 
                                    <span style = 'color: red;'>family = binomial</span>")),
                        br(),
                        fluidRow(
                          column(width = 3,
                                 tags$h5(HTML("Choose the <em>Fertilizer</em> effect")),
                                 sliderInput("slope.glm.lausanne.bin.inter",
                                             label = "in Lausanne",
                                             value = slope.scenario1.glm.bin.inter["Lausanne"],
                                             min = 0,
                                             max = 20,
                                             step = 0.1,
                                             ticks = FALSE),
                                 sliderInput("slope.glm.locarno.bin.inter",
                                             label = "in Locarno",
                                             value = slope.scenario1.glm.bin.inter["Locarno"],
                                             min = 0,
                                             max = 20,
                                             step = 0.1,
                                             ticks = FALSE),
                                 sliderInput("slope.glm.zurich.bin.inter",
                                             label = "in Zurich",
                                             value = slope.scenario1.glm.bin.inter["Zurich"],
                                             min = 0,
                                             max = 20,
                                             step = 0.1,
                                             ticks = FALSE),
                                 br(),
                                 tags$h5(HTML("Choose the <em>Site</em> effect")),
                                 sliderInput("intercept.glm.lausanne.bin.inter",
                                             label = "Lausanne",
                                             value = intercept.scenario1.glm.bin.inter["Lausanne"],
                                             min = 0,
                                             max = 20,
                                             step = 0.1,
                                             ticks = FALSE),
                                 sliderInput("intercept.glm.locarno.bin.inter",
                                             label = "Locarno",
                                             value = intercept.scenario1.glm.bin.inter["Locarno"],
                                             min = 0,
                                             max = 20,
                                             step = 0.1,
                                             ticks = FALSE),
                                 sliderInput("intercept.glm.zurich.bin.inter",
                                             label = "Zurich",
                                             value = intercept.scenario1.glm.bin.inter["Zurich"],
                                             min = 0,
                                             max = 20,
                                             step = 0.1,
                                             ticks = FALSE),
                          ), ## End column1
                          column(width = 9,
                                 tags$h4("Marginal plots"),
                                 plotOutput("marginal.plot.inter.glm.bin"),
                                 tags$li(class = "dropdown",
                                         dropMenu(
                                           dropdownButton("Info", 
                                                          status = 'success', 
                                                          icon = icon('info')),
                                           size = "xs",
                                           br(),
                                           h5(paste0("On the marginal graph we use",
                                                     " smoothers to avoid making the ", 
                                                     "linearity assumption")),
                                           placement = "bottom",
                                           arrow = FALSE)),
                                 br(),
                                 htmlOutput("explanation.plots.together.bin.inter"),
                                 plotOutput("marginal.plots.together.bin.inter"),
                                 tags$p("Colours are used in these plot to inspect 
                                        whether the categorical variable site interacts 
                                        with the continuous predictor Fertilizer.")
                                 
                          ) ## End column3
                        ), ## End Fluidrow
                        br(),
                        htmlOutput("summary.title.glm.bin.inter"),
                        verbatimTextOutput("model.glm.bin.inter"),
                        tags$h4("Model equation"),
                        htmlOutput("model.equation.7"),
                        br(),
                        
                        br(),
                        htmlOutput("plot.title.glm.bin.inter"),
                        plotOutput("plot.glm.bin.inter"),
                        # checkboxInput("observed.values.glm.bin.inter", 
                        #               "Add observed values", 
                        #               FALSE) #,
                        #useShinyjs(),
                        # bsCollapse(id = "collapsible.glm.bin.inter",
                        #            bsCollapsePanel("TA-plot", "This panel shows",
                        #                            "the TA-plot for a generalised linear",
                        #                            "model with interactions and",
                        #                            "binary response",
                        #                            plotOutput("plot.residuals.bin.inter"))
                        # ) ## End bs Collapse
                        
               ), ## End of Seventh Panel
               
               
               
               ## Eighth Panel - Generalised linear model with quadratic effects - Binomial---------------
               
               tabPanel("8. Generalised linear model with quadratic effects - Binomial",
                        tags$h3("Generalised linear model with quadratic effect - Binomial"),
                        tags$p(HTML("In this panel <em>Cob_pres</em> 
                        is explained by a generalised linear model with logit link function. 
                        The explanatory variables are <em>Fertilizer</em> and <em>Site</em>.")),
                        
                        tags$p(HTML("<em>Fertilizer</em> is supposed to have
                                    a quadratic effect on the response variable")),
                        tags$h4("Model formula"),
                        tags$p(HTML("Cob_pres ~ Fertilizer + 
                                    Site <span style = 'color: red;'> +I(Fertilizer^2), 
                                    family = binomial")),
                        br(),
                        fluidRow(
                          column(width = 3,
                                 tags$h5(HTML("Choose the <em>Fertilizer</em> effect")),
                                 sliderInput("slope.x.glm.quad",
                                             label = "Linear",
                                             value = slope.x.scenario1.glm.quad.eff["Lausanne"],
                                             min = 5,
                                             max = 10,
                                             step = 0.1,
                                             ticks = FALSE),
                                 sliderInput("slope.x.sq.glm.quad",
                                             label = "Quadratic",
                                             value = slope.x.sq.scenario1.glm.quad.eff["Locarno"],
                                             min = -0.1,
                                             max = 0,
                                             step = 0.1,
                                             ticks = FALSE),
                                 br(),
                                 tags$h5(HTML("Choose the <em>Site</em> effect")),
                                 sliderInput("intercept.glm.lausanne.quad",
                                             label = "Lausanne",
                                             value = intercept.scenario1.glm.quad.eff["Lausanne"],
                                             min = 0,
                                             max = 20,
                                             step = 0.1,
                                             ticks = FALSE),
                                 sliderInput("intercept.glm.locarno.quad",
                                             label = "Locarno",
                                             value = intercept.scenario1.glm.quad.eff["Locarno"],
                                             min = 0,
                                             max = 20,
                                             step = 0.1,
                                             ticks = FALSE),
                                 sliderInput("intercept.glm.zurich.quad",
                                             label = "Zurich",
                                             value = intercept.scenario1.glm.quad.eff["Zurich"],
                                             min = 0,
                                             max = 20,
                                             step = 0.1,
                                             ticks = FALSE),
                          ),
                          column(width = 9,
                                 tags$h4("Marginal plots"),
                                 plotOutput("marginal.plot.glm.bin.quad"),
                                 tags$li(class = "dropdown",
                                         dropMenu(
                                           dropdownButton("Info", 
                                                          status = 'success', 
                                                          icon = icon('info')),
                                           size = "xs",
                                           br(),
                                           h5(paste0("On the marginal graph we use",
                                                     " smoothers to avoid making the ", 
                                                     "linearity assumption")),
                                           placement = "bottom",
                                           arrow = FALSE)),
                                 br(),
                                 htmlOutput("explanation.plots.together.bin.quad"),
                                 plotOutput("marginal.plots.together.bin.quad"),
                                 tags$p("Colours are used in these plot to inspect 
                                        whether the categorical variable site interacts
                                        with the continuous predictor Fertilizer.")
                          ) ## End column 2
                          
                          
                        ), ## End fluidRow
                        br(),
                        htmlOutput("summary.title.glm.quad.eff"),
                        verbatimTextOutput("model.glm.quad.eff"),
                        tags$h4("Model equation"),
                        htmlOutput("model.equation.8"),
                        br(),
                        br(),
                        htmlOutput("plot.title.glm.quad.eff"),
                        plotOutput("plot.glm.quad.eff") #,
                        #useShinyjs(),
                        # bsCollapse(id = "collapsible.glm.quad",
                        #            bsCollapsePanel("TA-plot", "This panel shows",
                        #                            "the TA-plot for a generalised linear",
                        #                            "model with quadratic effects",
                        #                            "and binary response",
                        #                            plotOutput("plot.residuals.glm.quad"))
                        #) ## End bsCollapse
                        
                        
               ), ## End Eighth Panel
               
               
               ## Ninth Panel - Generalised linear model - Poisson ----------------------------
               
               tabPanel("9. Generalised linear model - Poisson",
                        tags$h3("Generalised linear model - Poisson"),
                        tags$p(HTML("In this panel, the response variable <em>Cob_nb</em> 
                                    represents the count of plants that have germinated.
                                    To analyze this count data, a generalised linear model 
                                    is employed with the square root function as the link function.")),
                        
                        tags$p(HTML("The explanatory variables remain the same as before, 
                                    namely <em>Fertilizer</em> and <em>Site</em>, 
                                    and they have an additive effect.")),
                        
                        
                        tags$h4("Model formula"),
                        tags$p(HTML("Cob_nb ~ Fertilizer + Site, <span style = 'color: red;'>family = poisson</span>")),
                        br(),
                        fluidRow(
                          column(width = 3,
                                 tags$h5(HTML("Choose the <em>Site</em> effect to put on log scale")),
                                 sliderInput("intercept.glm.lausanne.pois",
                                             label = "Lausanne",
                                             value = intercept.scenario1.glm.pois["Lausanne"],
                                             min = 1,
                                             max = 20,
                                             step = 0.1,
                                             ticks = FALSE),
                                 sliderInput("intercept.glm.locarno.pois",
                                             label = "Locarno",
                                             value = intercept.scenario1.glm.pois["Locarno"],
                                             min = 1,
                                             max = 20,
                                             step = 0.01,
                                             ticks = FALSE),
                                 sliderInput("intercept.glm.zurich.pois",
                                             label = "Zurich",
                                             value = intercept.scenario1.glm.pois["Zurich"],
                                             min = 1,
                                             max = 20,
                                             step = 0.01,
                                             ticks = FALSE),
                                 br(),
                                 sliderInput("slope.glm.pois",
                                             label = HTML(paste0("<h5>", 
                                                                 "Choose the Fertilizer effect to put on log scale",
                                                                 "</h5>")),
                                             value = slope.scenario1.glm.pois["Lausanne"],
                                             min = 1,
                                             max = 1.1,
                                             step = 0.001,
                                             ticks = FALSE)
                          ), ## End column1
                          column(width = 9,
                                 tags$h4("Marginal plots"),
                                 plotOutput("marginal.plot.glm.pois"),
                                 tags$li(class = "dropdown",
                                         dropMenu(
                                           dropdownButton("Info", 
                                                          status = 'success', 
                                                          icon = icon('info')),
                                           size = "xs",
                                           br(),
                                           h5(paste0("On the marginal graph we use",
                                                     " smoothers to avoid making the ", 
                                                     "linearity assumption")),
                                           placement = "bottom",
                                           arrow = FALSE)),
                                 br(),
                                 htmlOutput("explanation.plots.together.poisson"),
                                 plotOutput("marginal.plot.together.glm.pois"),
                                 tags$p("Colours are used in these plot to inspect
                                        whether the categorical variable site interacts
                                        with the continuous predictor Fertilizer.")
                          ) 
                          
                        ), ## End fluidRow
                        
                        br(),
                        htmlOutput("summary.title.glm.pois"),
                        verbatimTextOutput("model.glm.pois"),
                        br(),
                        tags$h4("Model equation"),
                        htmlOutput("model.equation.9"),
                        
                        br(),
                        br(),
                        htmlOutput("plot.title.glm.pois"),
                        plotOutput("plot.glm.pois") #,
                        #useShinyjs(),
                        # bsCollapse(id = "collapsible.glm.pois",
                        #            bsCollapsePanel("TA-plot", "This panel shows",
                        #                            "the TA-plot for a generalised linear",
                        #                            "model with a poisson distribution",
                        #                            plotOutput("plot.residuals.pois"))
                        # ) ## End bsCollapse
                        
               ), ## End of Ninth Panel
               
               
               ## Tenth Panel - gGeneralised linear model with interaction - Poisson -----------
               tabPanel("10. Generalised linear model with interaction - Poisson",
                        tags$h3("Generalised linear model with interaction - Poisson"),
                        tags$p(HTML("In this panel <em>Cob_nb</em> (taken as a count)
                        is explained by a generalised linear model with square root as link function. 
                        The explanatory variables are <em>Fertilizer</em> and <em>Site</em>.")),
                        
                        tags$p("This model is more flexible than the previous ones 
                               as it allows the predictors to interact.
                               So, it does not assume a purely additive of them."),
                        tags$p("In other words, this model allows the effect of Fertilizer to depend on site."),
                        
                        tags$h4("Model formula"),
                        tags$p(HTML("Cob_nb ~ Fertilizer <span style='color: red;'>*</span> Site, 
                                    <span style='color: red;'>family = poisson</span>")),
                        br(),
                        fluidRow(
                          column(width = 3,
                                 tags$h5(HTML("Choose the <em>Fertilizer</em> effect to put on log scale")),
                                 sliderInput("slope.glm.lausanne.pois.inter",
                                             label = "in Lausanne",
                                             value = slope.scenario1.glm.pois.inter["Lausanne"],
                                             min = 1,
                                             max = 1.1,
                                             step = 0.001,
                                             ticks = FALSE),
                                 sliderInput("slope.glm.locarno.pois.inter",
                                             label = "in Locarno",
                                             value = slope.scenario1.glm.pois.inter["Locarno"],
                                             min = 1,
                                             max = 1.1,
                                             step = 0.001,
                                             ticks = FALSE),
                                 sliderInput("slope.glm.zurich.pois.inter",
                                             label = "in Zurich",
                                             value = slope.scenario1.glm.pois.inter["Zurich"],
                                             min = 1,
                                             max = 1.1,
                                             step = 0.001,
                                             ticks = FALSE),
                                 br(),
                                 tags$h5(HTML("Choose the <em>Site</em> effect to put on log scale")),
                                 sliderInput("intercept.glm.lausanne.pois.inter",
                                             label = "Lausanne",
                                             value = intercept.scenario1.glm.pois.inter["Lausanne"],
                                             min = 1,
                                             max = 20,
                                             step = 0.1,
                                             ticks = FALSE),
                                 sliderInput("intercept.glm.locarno.pois.inter",
                                             label = "Locarno",
                                             value = intercept.scenario1.glm.pois.inter["Locarno"],
                                             min = 1,
                                             max = 20,
                                             step = 0.01,
                                             ticks = FALSE),
                                 sliderInput("intercept.glm.zurich.pois.inter",
                                             label = "Zurich",
                                             value = intercept.scenario1.glm.pois.inter["Zurich"],
                                             min = 1,
                                             max = 20,
                                             step = 0.01,
                                             ticks = FALSE)
                                 
                          ), ## End column1
                          column(width = 9,
                                 tags$h4("Marginal plots"),
                                 plotOutput("marginal.plot.glm.pois.inter"),
                                 tags$li(class = "dropdown",
                                         dropMenu(
                                           dropdownButton("Info", 
                                                          status = 'success', 
                                                          icon = icon('info')),
                                           size = "xs",
                                           br(),
                                           h5(paste0("On the marginal graph we use",
                                                     " smoothers to avoid making the ", 
                                                     "linearity assumption")),
                                           placement = "bottom",
                                           arrow = FALSE)),
                                 br(),
                                 htmlOutput("explanation.marignal.plot.together.glm.pois.inter"),
                                 plotOutput("marginal.plot.together.glm.pois.inter"),
                                 tags$p("Colours are used in these plot to inspect whether
                                        the categorical variable site interacts with the 
                                        continuous predictor Fertilizer.")
                                 
                          ) ## End column 2
                        ), ## End fluidRow
                        br(),
                        htmlOutput("summary.title.glm.pois.inter"),
                        verbatimTextOutput("model.glm.pois.inter"),
                        br(),
                        tags$h4("Model equation"),
                        htmlOutput("model.equation.10"),
                        br(),
                        br(),
                        htmlOutput("plot.title.glm.pois.inter"),
                        plotOutput("plot.glm.pois.inter")#,
                        #useShinyjs(),
                        # bsCollapse(id = "collapsible.glm.pois.inter",
                        #            bsCollapsePanel("TA-plot", "This panel shows",
                        #                            "the TA-plot for a generalised linear",
                        #                            "model with interactions",
                        #                            "and a poisson distribution",
                        #                            plotOutput("plot.residuals.pois.inter"))
                        # )
                        
               ), ## End of tenth Panel
               
               "Violating the assumptions",
               
               
               
               ## Eleven panel - violating linear model ----------------------------------------
               
               tabPanel("Linear model",
                        tags$h3("Violating the assumptions of a linear model"),
                        tags$p("This panel shows what happens when 
                               one of the different model assumptions is violated."),
                        selectInput("type.error", 
                                    label = "How to violate the model assumptions?",
                                    choices = c("Log-normal distribution of the error",
                                                #"Uniform distribution of the error",
                                                "Fit a model without interactions when needed",
                                                "Non-constant variance",
                                                "Fit a model without quadratic effects when needed")), 
                        actionButton("simulate.errors",
                                     label = "Simulate!"),
                        br(),
                        br(),
                        htmlOutput("description.violation.lin"),
                        br(),
                        htmlOutput("summary.title.errors"),
                        verbatimTextOutput("model.errors"),
                        br(),
                        htmlOutput("plot.title.errors"),
                        plotOutput("plot.errors"),
                        br(),
                        htmlOutput("something.amiss"),
                        br(),
                        bsCollapse(id = "collapsible.lm.errors",
                                   bsCollapsePanel("Diagnostic plots", 
                                                   "This panel shows",
                                                   "the diagnostic plots",
                                                   plotOutput("plot.diagnostics.errors"))#,
                                   # bsCollapsePanel("TA-plot", "This panel shows",
                                   #                 "the plot of residuals against fitted values",
                                   #                 plotOutput("plot.residuals.errors")),
                                   # bsCollapsePanel("QQplot", "This panel shows",
                                   #                 "the QQ-plot",
                                   #                 plotOutput("qqplot.errors"))
                        ),
                        actionButton("openPlotButton_panel11", "Zoom Plot"),
                        uiOutput("modalUI_panel11"), 
                        br(),
                        htmlOutput("explanation.diagnostic.plots"),
                        br()
                        
               ), ## End tabPanel
               
               
               ## Twelve panel - violating generalised linear model - binomial -----------------
               
               # tabPanel("Generalised linear model - Binomial",
               #          selectInput("type.error.glm.bin", 
               #                      label = "How to violate the model assumptions?",
               #                      choices = c("Linear instead of quadratic effects",
               #                                  "open to suggestion")), 
               #          actionButton("simulate.errors.glm.bin",
               #                       label = "Simulate!"),
               #          br(),
               #          htmlOutput("summary.title.errors.glm.bin"),
               #          verbatimTextOutput("model.errors.glm.bin"),
               #          br(),
               #          htmlOutput("plot.title.errors.glm.bin"),
               #          plotOutput("plot.errors.glm.bin"),
               #          br(),
               #          htmlOutput("residuals.title.errors.glm.bin"),
               #          br(),
               #          plotOutput("plot.residuals.errors.glm.bin"),
               #          br(),
               #          htmlOutput("qqplot.title.errors.glm.bin"),
               #          br(),
               #          plotOutput("qqplot.errors.glm.bin")
               # ), ## End tabPanel
               
               
               ## Thirteen panel - violating Generalised linear model - poisson ---------------
               
               # tabPanel("Generalised linear model - Poisson",
               #          selectInput("type.error.glm.pois", 
               #                      label = "How to violate the model assumptions?",
               #                      choices = c("Linear instead of quadratic effects",
               #                                  "open to suggestion")), 
               #          actionButton("simulate.errors.glm.pois",
               #                       label = "Simulate!"),
               #          br(),
               #          htmlOutput("summary.title.errors.glm.pois"),
               #          verbatimTextOutput("model.errors.glm.pois"),
               #          br(),
               #          htmlOutput("plot.title.errors.glm.pois"),
               #          plotOutput("plot.errors.glm.pois"),
               #          br(),
               #          htmlOutput("residuals.title.errors.glm.pois"),
               #          br(),
               #          plotOutput("plot.residuals.errors.glm.pois"),
               #          br(),
               #          htmlOutput("qqplot.title.errors.glm.pois"),
               #          br(),
               #          plotOutput("qqplot.errors.glm.pois")
               # ), ## End tabPanel
               
               
               ## fourteen panel - overdispersion ---------------------------------------------
               
               tabPanel("Over-dispersion",
                        tags$h3("Over-dispersion"),
                        br(),
                        htmlOutput("description"),
                        br(),
                        actionButton("simulate.errors.over.disp",
                                     label = "Simulate!"),
                        br(),
                        ## poisson
                        # htmlOutput("summary.title.errors.over.disp"),
                        # verbatimTextOutput("model.errors.over.disp"),
                        # br(),
                        # htmlOutput("plot.title.errors.over.disp"),
                        # br(),
                        # plotOutput("plot.errors.over.disp"),
                        # br(),
                        ## quasi-poisson
                        tags$h5("Marginal plots"),
                        plotOutput("marginal.plot.overdispersion"),
                        htmlOutput("summary.title.errors.over.disp.quasi"),
                        br(),
                        verbatimTextOutput("model.errors.over.disp.quasi"),
                        br(),
                        htmlOutput("plot.title.errors.over.disp.quasi"),
                        br(),
                        plotOutput("plot.errors.over.disp.quasi"),
                        ## negative binomial
                        htmlOutput("summary.title.errors.over.disp.nb"),
                        br(),
                        verbatimTextOutput("model.errors.over.disp.nb"),
                        br(),
                        htmlOutput("plot.title.errors.over.disp.nb"),
                        br(),
                        plotOutput("plot.errors.over.disp.nb")
               ) ## End tabPanel
  ) ## End navlist
  
) ## End fluidPage