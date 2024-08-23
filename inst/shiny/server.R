
########################### server ##############################

server <- function(input, output, session){
  library(ggplot2); theme_set(theme_bw())
  
  ## Introduction -------------------------------------------------------------
  
  output$story <- renderText({
    
    HTML("<p> A dedicated team of researchers is working on improving the yield of cornfields. </p>")
    
  })
  
  
  output$cornfield.image <- renderImage({
    
    list(src = "cornfield_image_no_copyright.jpg",
         width = 300,
         height = 400)
  }, 
  deleteFile = FALSE)
  
  output$story2 <- renderText({
    HTML("<br><p> To achieve this goal, they conducted a study to determine the effects of 
    different variables on the weight of corn cobs. </p>
      <p> They monitored the growth of corn 306 plants and recorded the weights of their cobs as they matured,
    while varying different variables. </p> 
      Ultimately, they discovered that the most influential factors were
    the amount of fertilizer used, the location of the cornfield, 
    the height of the plant, 
    and periods of drought. </p>
      <p> After completing this study, a second one was conducted, 
         this time focusing on the observation of the number of corn cobs 
         rather than their weight. The study was again based on 306 corn plants.</p> <br><br><br><br> <p>
         Nisia Trisconi and Matteo Tanadini (from Zurich Data Scientists) collaborated to develop the concepts present in
         this application, with Nisia handling the coding and implementation.</p> </p><br><br><br><br>
         <p> This work is licensed under a CC-BY-SA 4.0 licence. More info are available at https://creativecommons.org/licenses/by-sa/4.0/deed.en</p>")
    
    
    
    
  })
  ## Define reactive variables ---------------------------------------------------
  
  
  ## We define variables in a reactive way
  # n.simulate <- reactive(input$simulate)
  # n.simulate.inter <- reactive(input$simulate.inter)
  # n.simulate.quad <- reactive(input$simulate.quad)
  # n.simulate.glm.bin <- reactive(input$simulate.glm.bin)
  # n.simulate.glm.bin.inter <- reactive(input$simulate.glm.bin.inter)
  # n.simulate.glm.pois <- reactive(input$simulate.glm.pois)
  # n.simulate.glm.pois.inter <- reactive(input$simulate.glm.pois.inter)
  # n.simulate.glm.quad.eff <- reactive(input$simulate.glm.quad.eff) 
  
  
  ## First Panel -------------------------------------------------
  
  v.intercept.lm1 <- reactive({
    c("Lausanne" = input$intercept.lm.lausanne1,
      "Locarno" = input$intercept.lm.locarno1,
      "Zurich" = input$intercept.lm.zurich1)
  })
  
  v.slope.lm1 <- reactive({
    c("Lausanne" = input$slope.lm1,
      "Locarno" = input$slope.lm1,
      "Zurich" = input$slope.lm1)
  })
  
  
  ## 1.5 Panel -------------------------------------------------
  
  v.intercept.lm1.5 <- reactive({
    c("Lausanne" = input$intercept.lm.lausanne1.5,
      "Locarno" = input$intercept.lm.locarno1.5,
      "Zurich" = input$intercept.lm.zurich1.5)
  })
  
  v.slope.lm1.5 <- reactive({
    c("Lausanne" = input$slope.lm1.5,
      "Locarno" = input$slope.lm1.5,
      "Zurich" = input$slope.lm1.5)
  })
  
  
  
  v.slope.height1 <- reactive({
    c("Lausanne" = input$slope.lm.height1,
      "Locarno" = input$slope.lm.height1,
      "Zurich" = input$slope.lm.height1)
  })
  
  
  
  ## Second Panel -------------------------------------------------
  
  v.intercept.lm2 <- reactive({
    c("Lausanne" = input$intercept.lm.lausanne2,
      "Locarno" = input$intercept.lm.locarno2,
      "Zurich" = input$intercept.lm.zurich2)
  })
  
  v.slope.lm2 <- reactive({
    c("Lausanne" = input$slope.lm2,
      "Locarno" = input$slope.lm2,
      "Zurich" = input$slope.lm2)
  })
  
  v.intercept.variety <- reactive({
    
    c("Lausanne" = intercept.scenario1.variety[["Lausanne"]],
      "Locarno" = intercept.scenario1.variety[["Lausanne"]],
      "Zurich" = intercept.scenario1.variety[["Lausanne"]])
    
  })
  
  
  v.slope.height <- reactive({
    c("Lausanne" = input$slope.lm.height,
      "Locarno" = input$slope.lm.height,
      "Zurich" = input$slope.lm.height)
  })
  
  
  
  ## Third Panel -------------------------------------------------
  
  
  v.intercept.lm.inter <- reactive({
    c("Lausanne" = input$intercept.lm.lausanne.inter,
      "Locarno" = input$intercept.lm.locarno.inter,
      "Zurich" = input$intercept.lm.zurich.inter)
  })
  
  v.slope.lm.inter <- reactive({
    c("Lausanne" = input$slope.lm.lausanne.inter,
      "Locarno" = input$slope.lm.locarno.inter,
      "Zurich" = input$slope.lm.zurich.inter)
  })
  
  ## Forth Panel -------------------------------------------------
  
  v.intercept.lm.quad <- reactive({
    c("Lausanne" = input$intercept.lm.lausanne.quad,
      "Locarno" = input$intercept.lm.locarno.quad,
      "Zurich" = input$intercept.lm.zurich.quad)
  })
  
  v.slope.x.lm.quad <- reactive({
    c("Lausanne" = input$slope.lm.x.quad,
      "Locarno" = input$slope.lm.x.quad,
      "Zurich" = input$slope.lm.x.quad)
  })
  
  v.slope.x.sq.lm.quad <- reactive({
    c("Lausanne" = input$slope.lm.x.sq.quad,
      "Locarno" = input$slope.lm.x.sq.quad,
      "Zurich" = input$slope.lm.x.sq.quad)
  })
  
  
  ## Fifth Panel -------------------------------------------------
  
  
  v.intercept.glm.bin <- reactive({
    c("Lausanne" = input$intercept.glm.lausanne.bin,
      "Locarno" = input$intercept.glm.locarno.bin,
      "Zurich" = input$intercept.glm.zurich.bin)
  })
  
  v.slope.glm.bin <- reactive({
    c("Lausanne" = input$slope.glm.bin,
      "Locarno" = input$slope.glm.bin,
      "Zurich" = input$slope.glm.bin)
  })
  
  ## Sixth Panel -------------------------------------------------
  
  v.intercept.glm.bin.inter <- reactive({
    c("Lausanne" = input$intercept.glm.lausanne.bin.inter,
      "Locarno" = input$intercept.glm.locarno.bin.inter,
      "Zurich" = input$intercept.glm.zurich.bin.inter)
  })
  
  v.slope.glm.bin.inter <- reactive({
    c("Lausanne" = input$slope.glm.lausanne.bin.inter,
      "Locarno" = input$slope.glm.locarno.bin.inter,
      "Zurich" = input$slope.glm.zurich.bin.inter)
  })
  
  
  ## Seventh Panel -------------------------------------------------
  
  
  v.intercept.glm.quad <- reactive({
    c("Lausanne" = input$intercept.glm.lausanne.quad,
      "Locarno" = input$intercept.glm.locarno.quad,
      "Zurich" = input$intercept.glm.zurich.bin.inter)
  })
  
  v.slope.x.glm.quad <- reactive({
    c("Lausanne" = input$slope.x.glm.quad,
      "Locarno" = input$slope.x.glm.quad,
      "Zurich" = input$slope.x.glm.quad)
  })
  
  v.slope.x.sq.glm.quad <- reactive({
    c("Lausanne" = input$slope.x.sq.glm.quad,
      "Locarno" = input$slope.x.sq.glm.quad,
      "Zurich" = input$slope.x.sq.glm.quad)
  })
  
  
  ## Eight Panel -------------------------------------------------
  
  v.intercept.glm.pois <- reactive({
    log(c("Lausanne" = input$intercept.glm.lausanne.pois,
          "Locarno" = input$intercept.glm.locarno.pois,
          "Zurich" = input$intercept.glm.zurich.pois))
  })
  
  v.slope.glm.pois <- reactive({
    log(c("Lausanne" = input$slope.glm.pois,
          "Locarno" = input$slope.glm.pois,
          "Zurich" = input$slope.glm.pois))
  })
  
  
  
  v.intercept.glm.pois.inter <- reactive({
    log(c("Lausanne" = input$intercept.glm.lausanne.pois.inter,
          "Locarno" = input$intercept.glm.locarno.pois.inter,
          "Zurich" = input$intercept.glm.zurich.pois.inter))
  })
  
  v.slope.glm.pois.inter <- reactive({
    log(c("Lausanne" = input$slope.glm.lausanne.pois.inter,
          "Locarno" = input$slope.glm.locarno.pois.inter,
          "Zurich" = input$slope.glm.zurich.pois.inter))
  })
  
  n.simulate.errors <- reactive(input$simulate.errors)
  n.simulate.errors.glm.bin <- reactive(input$simulate.errors.glm.bin)
  n.simulate.errors.glm.pois <- reactive(input$simulate.errors.glm.pois)
  n.simulate.errors.over.disp <- reactive(input$simulate.errors.over.disp)
  
  
  ## Eleven Panel -------------------------------------------------
  
  c.type.error <- reactive(input$type.error)
  
  
  ############################### First Panel - Linear model 1 continuous, 1 factor ###############################
  
  
  # start introjs when button is pressed with custom options and events
  observeEvent(input$help,
               introjs(session, options = list("nextLabel"="Next",
                                               "prevLabel"="Prev",
                                               "close"),
                       # events = list("oncomplete"=I('alert("Glad that is over")')) 
               ))
  
  
  
  ########################### Simulate dataset ##############################
  
  
  
  
  ##  ObserveEvent - Explanation  ---------------------------------------------
  
  
  
  ## ObserveEvent - creation dataset ---------------------------------------------
  
  # dataset <- eventReactive(n.simulate(), {
  dataset1 <- reactive({ 
    ## Sample a scenario
    scenario <- sample(nb.scenarios, 
                       size = 1)
    
    if (scenario == 1) {
      
      ## Sample fertilizer
      d.tmp <- data.frame(Site = rep(c("Lausanne", "Locarno", "Zurich"), 
                                     each = dim.dataset/3), 
                          Fertilizer = sample(50, size = dim.dataset, 
                                              replace = TRUE))
      
      
      ## Simulate response variable
      Cob_weight <-  v.intercept.lm1()[d.tmp$Site] + 
        v.slope.lm1()[d.tmp$Site] * d.tmp$Fertilizer + 
        ## simulate errors
        rnorm(dim.dataset, 
              mean = 0, 
              sd = input$sigma.lm1)
      
      ## Create a dataset
      d.tmp %>% 
        mutate(Cob_weight = Cob_weight)
      
    } #else if (scenario == 2){}
    
    
    
  }) ## End eventReactive
  
  
  # 
  # observeEvent(n.simulate(), {
  #   
  #   ## Show collapsible boxes after the first time clicking on simulate
  #   
  #   ifelse(n.simulate() == 0, 
  #          shinyjs::hide(id = "collapsible.lm"),
  #          shinyjs::show(id = "collapsible.lm")
  #   )
  # })
  
  
  ## Model equation --------------------------------------------------------------
  
  output$model.equation.1 <- renderText({
    HTML(paste0("y<sub>i</sub> = &beta;<sub>0</sub> + 
         &beta;<sub>Fertilizer</sub> * x<sub>Fertilizer<sub>i</sub></sub>
         + &beta;<sub>LOC</sub> * I<sub>LOC<sub>i</sub></sub> + 
         &beta;<sub>ZH</sub> * I<sub>ZH<sub>i</sub></sub>  + 
         &epsilon;<sub>i</sub>, &ensp; i = 1,..., ", dim.dataset), 
         ",&ensp; <em>e</em> ~ N(&mu; , &sigma;<sub>&epsilon; </sub><sup>2</sup>)")
  })
  
  ## Plots -----------------------------------------------------------------------
  
  ## Marginal plots
  output$marginal.plot.lm1 <- renderPlot({
    # req(n.simulate())
    
    gg.fertilizer <- ggplot(dataset1(), mapping = aes(x = Fertilizer, 
                                                      y = Cob_weight)) +
      geom_point(alpha = 0.3) +
      geom_smooth(se = TRUE) +
      ggtitle("Cob_weight against Fertilizer")
    
    gg.site <- ggplot(dataset1(), mapping = aes(x = Site, 
                                                y = Cob_weight, 
                                                colour = Site)) +
      geom_boxplot() +
      geom_point(alpha = 0.2) +
      ggtitle("Cob_weight against Site")
    
    ggarrange(gg.fertilizer, gg.site)
    
  })
  
  
  output$explanation.marginal.plot <- renderText({
    
    HTML("<p> In this particular case, it is possible to put these two marginal
         plots together as follows </p>")
    
  })
  
  output$marginal.plots.together <- renderPlot({
    # req(n.simulate())
    
    ggplot(dataset1(), mapping = aes(x = Fertilizer, 
                                     y = Cob_weight,
                                     colour = Site)) +
      geom_point(alpha = 0.3) +
      geom_smooth(se = TRUE) +
      ggtitle("Cob_weight against Fertilizer, highlighting Site")
    
  })
  
  ## Linear model 
  InputModel.lm1 <- reactive({
    lm(Cob_weight ~ Fertilizer + Site, 
       data = dataset1())
  }) 
  
  ## Title for summary
  output$summary.title1 <- renderText({
    #req(n.simulate())
    
    HTML(paste0("<h4>","Summary of the model","</h4>"))
  })
  
  ## Render summary of the linear model
  output$model.lm1 <- renderPrint({
    #req(n.simulate())
    
    summary(InputModel.lm1())
  })
  
  ## Title for plot
  output$plot.title1 <- renderText({
    #req(n.simulate())
    
    HTML(paste0("<h4>","Fitted values of the linear model","</h4>"))
  })
  
  ## Plot the dataset and the linear model
  output$plot.lm1 <- renderPlot({
    #req(n.simulate())
    
    coef.lm <- coef(InputModel.lm1())
    
    ## Add predicted values to the dataset
    dataset.predict <- dataset1() %>%
      mutate( fitted.lm = predict(InputModel.lm1()))
    
    
    gg.lm <- ggplot(dataset.predict,
                    mapping = aes(x = Fertilizer, y = fitted.lm)) +
      geom_point(alpha = 0.3, mapping = aes(colour = Site)) +
      
      
      ## Linear model for Site Lausanne
      geom_abline(intercept = coef.lm["(Intercept)"],
                  slope = coef.lm["Fertilizer"],
                  alpha = 0.5,
                  color = "red") +
      
      ## Linear model for Site Locarno
      geom_abline(intercept = coef.lm["(Intercept)"] + coef.lm["SiteLocarno"],
                  slope = coef.lm["Fertilizer"],
                  alpha = 0.5, 
                  color = "green4") +
      
      ## Linear model for Site Zurich
      geom_abline(intercept = coef.lm["(Intercept)"] + coef.lm["SiteZurich"],
                  slope = coef.lm["Fertilizer"],
                  alpha = 0.5,
                  color = "blue")
    
    if ( input$observed.values.lm1) {
      
      gg.lm +
        geom_point(alpha = 0.3, mapping = aes(y = Cob_weight), 
                   alpha = 0.5, colour = "darkgrey") 
      
    } else {
      
      gg.lm
      
    }
    
  })
  
  
  
  
  
  InputModel.fit1 <- reactive({
    dataset1() %>% 
      mutate(fit = fitted(InputModel.lm1())) %>% 
      mutate(res = residuals(InputModel.lm1()))
  })
  
  
  output$plot.diagnostic.lm1 <- renderPlot({
    par(mfrow = c(2, 2)#, 
        #mar = c(1, 2, 2, 2)
        )
    plot(InputModel.lm1(), which = c(1, 2, 3, 5))
    par(mfrow = c(1, 1))
  })
  
  
  

  observeEvent(input$openPlotButton_panel1, {
    output$modalUI_panel1 <- renderUI({
      modalDialog(
        id = "plotModal_panel1",
        title = "Zoomed Plot",
        plotOutput("plot.diagnostic.lm1_zoomed"),
        easyClose = TRUE,
      )

    })
  })

  output$plot.diagnostic.lm1_zoomed <- renderPlot({
    par(mfrow = c(2, 2) #, mar = c(0.2, 1, 2, 1)
        )
    plot(InputModel.lm1.5(), which = c(1, 2, 3, 5))
    par(mfrow = c(1, 1))
  })

  
  # ## plot the residuals
  # output$plot.residuals1 <- renderPlot({
  #   
  #   InputModel.fit1() %>% 
  #     ggplot(mapping = aes(x = fit, y = res)) +
  #     geom_point(alpha = 0.3) +
  #     geom_smooth() +
  #     labs(x = "Fitted values", y = "Residuals") +
  #     geom_hline(yintercept = 0, colour = "violet")
  #   
  # })
  
  
  
  
  # output$qqplot.lm1 <- renderPlot({
  # 
  #   InputModel.fit1() %>%
  #     ggplot(aes(sample = res)) +
  #     geom_qq(na.rm = TRUE) +
  #     geom_qq_line(color = "red", na.rm = TRUE) +
  #     theme_bw() +
  #     labs(y = "Residuals quantiles", x = "Theoretical quantiles")
  # })
  
  
  
  
  ########################### 1.5 Panel- Linear model 2 continuous, 1 factors ##############################
  
  ########################### Simulate dataset ##############################
  
  
  
  
  ## ObserveEvent - creation dataset ---------------------------------------------
  
  # dataset <- eventReactive(n.simulate(), {
  dataset1.5 <- reactive({ 
    ## Sample a scenario
    scenario <- sample(nb.scenarios, 
                       size = 1)
    
    if (scenario == 1) {
      
      ## Sample fertilizer
      d.tmp <- data.frame(Site = rep(c("Lausanne", "Locarno", "Zurich"), 
                                     each = dim.dataset/3), 
                          Fertilizer = sample(50, 
                                              size = dim.dataset, 
                                              replace = TRUE),
                          Plant_height = rlnorm(dim.dataset)
      )
      
      
      ## Simulate response variable
      Cob_weight <- v.intercept.lm1.5()[d.tmp$Site] + 
        v.slope.lm1.5()[d.tmp$Site] * d.tmp$Fertilizer + 
        v.slope.height1()[d.tmp$Site] * log(d.tmp$Plant_height) +
        
        ## simulate errors
        rnorm(dim.dataset, 
              mean = 0, 
              sd = input$sigma.lm1.5)
      
      ## Create a dataset
      d.tmp %>% 
        mutate(Cob_weight = Cob_weight)
      
    } #else if (scenario == 2){}
    
    
    
  }) ## End eventReactive
  
  
  # 
  # observeEvent(n.simulate(), {
  #   
  #   ## Show collapsible boxes after the first time clicking on simulate
  #   
  #   ifelse(n.simulate() == 0, 
  #          shinyjs::hide(id = "collapsible.lm"),
  #          shinyjs::show(id = "collapsible.lm")
  #   )
  # })
  
  
  
  ## Model equation --------------------------------------------------------------
  
  output$model.equation.2 <- renderText({
    HTML(paste0("y<sub>i</sub> = &beta;<sub>0</sub> + 
         &beta;<sub>Fertilizer</sub> * x<sub>Fertilizer<sub>i</sub></sub> + 
    &beta;<sub>LOC<sub>i</sub></sub> * I<sub>LOC<sub>i</sub></sub> + 
         &beta;<sub>ZH</sub> * I<sub>ZH<sub>i</sub></sub> +  
         &beta;<sub>Plant_height</sub> * log(x<sub>Plant_height<sub>i</sub></sub>) +
                &epsilon;<sub>i</sub>, &ensp; i = 1,..., ", dim.dataset), 
         ",&ensp; <em>e</em> ~ N(&mu; , &sigma;<sub>&epsilon; </sub><sup>2</sup>)")
  })
  
  
  ## Plots -----------------------------------------------------------------------
  
  
  ## Marginal plot
  
  output$marginal.plot.first.draft.lm1.5 <- renderPlot({
    # req(n.simulate())
    
    gg.fertilizer <- ggplot(dataset1.5(),
                            mapping = aes(x = Fertilizer, 
                                          y = Cob_weight)) +
      geom_point(alpha = 0.3) +
      geom_smooth(se = TRUE) +
      
      ggtitle("Cob_weight against Fertilizer")
    
    gg.height <- ggplot(dataset1.5(), 
                        mapping = aes(x = Plant_height, 
                                      y = Cob_weight)) +
      geom_point(alpha = 0.3) +
      geom_smooth(se = TRUE) +
      
      ggtitle("Cob_weight against Plant_height")
    
    gg.site <- ggplot(dataset1.5(), 
                      mapping = aes(x = Site, 
                                    y = Cob_weight,
                                    colour = Site)) +
      geom_boxplot() +
      geom_point(alpha = 0.2) +
      ggtitle("Cob_weight against Site")
    
    
    gg.height.log <- ggplot(dataset1.5(), 
                            mapping = aes(x = Plant_height, 
                                          y = Cob_weight)) +
      geom_point(alpha = 0.3) +
      scale_x_log10() +
      geom_smooth(se = TRUE) +
      
      ggtitle("Cob_weight against Plant_height in log scale")
    
    
    ggarrange(gg.fertilizer, gg.site, gg.height, gg.height.log)
    
    
  })
  
  output$explanation.log <- renderText({
    
    HTML('<p>The variable <em>Plant_height</em> is right skewed (see plot just above on the left). 
         Indeed, there are very many observations on the left-hand side of the plot and become 
         less dense as you go to the right. This behaviour is absolutely expected for continuous 
         variables that can only take positive values, the so called "amounts". 
         In order to improve the situation, amounts are log-transformed 
         (see the plot just above on the right hand side). 
         For this reason we have done a second plot by log transforming this variable </p>')
  })
  
  
  output$explanation.not.variables.together <- renderText({
    
    HTML("<p> In the following plots we added a dimension to the plots. Indeed, 
         we highlighted the sites by colouring each of them with a different colour.
         <br>
         Unfortunately, putting all three explanatory variables in a single graph 
         would result in a plot that is too complicated to be understood.</p>")
    
  })
  
  output$marginal.plot.lm1.5 <- renderPlot({
    # req(n.simulate())
    
    gg.fertilizer <- ggplot(dataset1.5(),
                            mapping = aes(x = Fertilizer, 
                                          y = Cob_weight, 
                                          colour = Site)) +
      geom_point(alpha = 0.3) +
      geom_smooth(se = TRUE) +
      
      ggtitle("Cob_weight against Fertilizer, highlighting Site")
    
    gg.height <- ggplot(dataset1.5(), 
                        mapping = aes(x = Plant_height, 
                                      y = Cob_weight, 
                                      colour = Site)) +
      geom_point(alpha = 0.3) +
      scale_x_log10() +
      geom_smooth(se = TRUE) +
      
      ggtitle("Cob_weight against Plant_height in log scale, highlighting Site")
    
    ggarrange(gg.fertilizer, gg.height, nrow = 2)
    
    
  })
  
  
  
  ## Linear model 
  InputModel.lm1.5 <- reactive({
    lm(Cob_weight ~ Fertilizer + Site + log(Plant_height), 
       data = dataset1.5())
  }) 
  
  ## Title for summary
  output$summary.title1.5 <- renderText({
    #req(n.simulate())
    
    HTML(paste0("<h4>","Summary of the model","</h4>"))
  })
  
  ## Render summary of the linear model
  output$model.lm1.5 <- renderPrint({
    #req(n.simulate())
    
    summary(InputModel.lm1.5())
  })
  
  ## Title for plot
  output$plot.title1.5 <- renderText({
    #req(n.simulate())
    
    HTML(paste0("<h4>","Fitted values of the linear model","</h4>"))
  })
  
  ## Plot the dataset and the linear model
  output$plot.lm1.5 <- renderPlot({
    #req(n.simulate())
    
    coef.lm <- coef(InputModel.lm1.5())
    
    ## Add predicted values to the dataset
    dataset.predict <- dataset1.5() %>%
      mutate( fitted.lm = predict(InputModel.lm1.5()))
    
    log.mean.height <- log(mean(dataset1.5()$Plant_height))
    gg.fertilizer <- ggplot(dataset.predict,
                            mapping = aes(x = Fertilizer, 
                                          y = fitted.lm)) +
      geom_point(alpha = 0.3, mapping = aes(colour = Site))# +
      
      ## Linear model for Site Lausanne
      # geom_abline(intercept = coef.lm["(Intercept)"] +
      #               coef.lm["log(Plant_height)"] * log.mean.height,
      #             slope = coef.lm["Fertilizer"],
      #             alpha = 0.5,
      #             color = "red") +
      # 
      # ## Linear model for Site Locarno
      # geom_abline(intercept = coef.lm["(Intercept)"] + 
      #               coef.lm["SiteLocarno"] +
      #               coef.lm["log(Plant_height)"] * log.mean.height,
      #             slope = coef.lm["Fertilizer"],
      #             alpha = 0.5,
      #             color = "green4") +
      # 
      # ## Linear model for Site Zurich
      # geom_abline(intercept = coef.lm["(Intercept)"] + 
      #               coef.lm["SiteZurich"] + 
      #               coef.lm["log(Plant_height)"] * log.mean.height,
      #             slope = coef.lm["Fertilizer"],
      #             alpha = 0.5,
      #             color = "blue") +
      # ggtitle("Fitted values of the model") 
    
    
    
    gg.fertilizer.values <- if ( input$observed.values.lm1.5 ) {
      gg.fertilizer + 
        geom_point(alpha = 0.3, mapping = aes(y = Cob_weight), 
                   alpha = 0.5, 
                   colour = "darkgrey") 
    } else {
      
      gg.fertilizer
      
    }
    
    mean.fertilizer <- mean(dataset1.5()$Fertilizer)
    
    gg.height <- ggplot(dataset.predict,
                        mapping = aes(x = Plant_height, 
                                      y = fitted.lm)) +
      geom_point(alpha = 0.3, mapping = aes(colour = Site)) +
      scale_x_log10() #+
      # 
      # ## Linear model for Site Lausanne
      # geom_abline(intercept = coef.lm["(Intercept)"] + 
      #               coef.lm["Fertilizer"] * mean.fertilizer,
      #             slope = exp(coef.lm["log(Plant_height)"]),
      #             alpha = 0.5,
      #             color = "red") +
      # 
      # 
      # ## Linear model for Site Locarno
      # geom_abline(intercept = coef.lm["(Intercept)"] + 
      #               coef.lm["SiteLocarno"] + 
      #               coef.lm["Fertilizer"] * mean.fertilizer,
      #             slope = exp(coef.lm["log(Plant_height)"]),
      #             alpha = 0.5,
      #             color = "green4") +
      # 
      # ## Linear model for Site Zurich
      # geom_abline(intercept = coef.lm["(Intercept)"] + 
      #               coef.lm["SiteZurich"] + 
      #               coef.lm["Fertilizer"] * mean.fertilizer,
      #             slope = exp(coef.lm["log(Plant_height)"]),
      #             alpha = 0.5,
      #             color = "blue") +
      # ggtitle("Fitted values of the linear model") 
      # 
    
    
    gg.height.values <- if ( input$observed.values.lm1.5 ) {
      gg.height + 
        geom_point(alpha = 0.3, mapping = aes(y = Cob_weight), 
                   alpha = 0.5, 
                   colour = "darkgrey") 
    } else {
      
      gg.height
      
    }
    
    ggarrange(gg.fertilizer.values, gg.height.values)
    
    
  })
  
  
  
  
  
  InputModel.fit1.5 <- reactive({
    dataset1.5() %>% 
      mutate(fit = fitted(InputModel.lm1.5())) %>% 
      mutate(res = residuals(InputModel.lm1.5()))
  })
  
  output$plot.diagnostic.lm1.5 <- renderPlot({
    par(mfrow = c(2, 2)#, mar = c(0.2, 1, 2, 1)
        )
    plot(InputModel.lm1.5(), which = c(1, 2, 3, 5))
    par(mfrow = c(1, 1))
  })
  # 
  # ## plot the residuals
  # output$plot.residuals1.5 <- renderPlot({
  #   
  #   InputModel.fit1.5() %>% 
  #     ggplot(mapping = aes(x = fit, y = res)) +
  #     geom_hline(yintercept = 0, colour = "violet") +
  #     geom_point(alpha = 0.3) +
  #     geom_smooth() +
  #     labs(x = "Fitted values", y = "Residuals") 
  #   
  # })
  # 
  # 
  # 
  # 
  # output$qqplot.lm1.5 <- renderPlot({
  #   
  #   InputModel.fit1.5() %>%
  #     ggplot(aes(sample = res)) +
  #     geom_qq(na.rm = TRUE) +
  #     geom_qq_line(color = "red", na.rm = TRUE) +
  #     theme_bw() +
  #     labs(y = "Residuals quantiles", x = "Theoretical quantiles")
  #   
  #   
  # })
  
  
  observeEvent(input$openPlotButton_panel1.5, {
    output$modalUI_panel1.5 <- renderUI({
      modalDialog(
        id = "plotModal_panel1.5",
        title = "Zoomed Plot",
        plotOutput("plot.diagnostic.lm1.5_zoomed"),
        easyClose = TRUE,
      )
      
    })
  })
  
  output$plot.diagnostic.lm1.5_zoomed <- renderPlot({
    par(mfrow = c(2, 2)#, mar = c(0.2, 1, 2, 1)
        )
    plot(InputModel.lm1.5(), which = c(1, 2, 3, 5))
    par(mfrow = c(1, 1))
  })
  
  
  
  
  ########################### Second Panel- Linear model 2 continuous, 2 factors ##############################
  
  
  ########################### Simulate dataset ##############################
  
  
  
  
  ## ObserveEvent - creation dataset ---------------------------------------------
  
  # dataset <- eventReactive(n.simulate(), {
  dataset2 <- reactive({ 
    ## Sample a scenario
    scenario <- sample(nb.scenarios, 
                       size = 1)
    
    if (scenario == 1) {
      
      
      ## Sample fertilizer
      d.tmp <- data.frame(Site = rep(c("Lausanne", "Locarno", "Zurich"), 
                                     each = dim.dataset/3), 
                          Variety = sample(c(1, 0), ## Two types of variety
                                           size = 102, 
                                           replace = TRUE),
                          Fertilizer = sample(50, 
                                              size = dim.dataset, 
                                              replace = TRUE),
                          Plant_height = rlnorm(dim.dataset)
      )
      
      
      ## Simulate response variable
      Cob_weight <- v.intercept.lm2()[d.tmp$Site] + 
        v.intercept.variety()[d.tmp$Site] * d.tmp$Variety +
        v.slope.lm2()[d.tmp$Site] * d.tmp$Fertilizer + 
        v.slope.height()[d.tmp$Site] * log(d.tmp$Plant_height) +
        
        ## simulate errors
        rnorm(dim.dataset, 
              mean = 0, 
              sd = input$sigma.lm2)
      
      ## Create a dataset
      d.tmp %>% 
        mutate(Cob_weight = Cob_weight)
      
    } #else if (scenario == 2){}
    
    
    
  }) ## End eventReactive
  
  
  # 
  # observeEvent(n.simulate(), {
  #   
  #   ## Show collapsible boxes after the first time clicking on simulate
  #   
  #   ifelse(n.simulate() == 0, 
  #          shinyjs::hide(id = "collapsible.lm"),
  #          shinyjs::show(id = "collapsible.lm")
  #   )
  # })
  
  
  ## Model equation --------------------------------------------------------------
  
  output$model.equation.3 <- renderText({
    HTML(paste0("y<sub>i</sub> = &beta;<sub>0</sub> +
         &beta;<sub>Fertilizer</sub> * x<sub>Fertilizer<sub>i</sub></sub> +
         &beta;<sub>LOC</sub> * I<sub>LOC<sub>i</sub></sub> + 
         &beta;<sub>ZH</sub> * I<sub>ZH<sub>i</sub></sub> + 
         &beta;<sub>Plant_height</sub> * log(x<sub>Plant_height<sub>i</sub></sub>) +
         &beta;<sub>Variety<sub>2</sub></sub> * I<sub>Variety<sub>2<sub>i</sub></sub></sub> + 
         &epsilon;<sub>i</sub>, &ensp; i = 1,..., ", dim.dataset), 
         ",&ensp; <em>e</em> ~ N(&mu; , &sigma;<sub>&epsilon; </sub><sup>2</sup>)")
  })
  
  
  
  ## Plots -----------------------------------------------------------------------
  
  dataset2.fac <- reactive({
    
    ## Transform Variety into a factor, for a nicer look
    Variety.chr <- as.character(dataset2()$Variety)
    dataset2() %>% 
      mutate(Variety.fac = factor(Variety.chr,
                                  levels = c("1", "0"),
                                  labels = c("Grain", "Silage")))
  })
  
  
  ## Marginal plot
  output$marginal.plot.lm2 <- renderPlot({
    # req(n.simulate())
    
    gg.fertilizer <- ggplot(dataset2.fac(),
                            mapping = aes(x = Fertilizer, 
                                          y = Cob_weight)) +
      geom_point(alpha = 0.3) +
      geom_smooth(se = TRUE) +
      #facet_grid(~ Variety) +
      ggtitle("Cob_weight against Fertilizer")
    
    gg.height <- ggplot(dataset2.fac(), 
                        mapping = aes(x = Plant_height, 
                                      y = Cob_weight)) +
      geom_point(alpha = 0.3) +
      geom_smooth(se = TRUE) +
      ggtitle("Cob_weight against Plant_height")
    
    
    
    gg.variety <- ggplot(dataset2.fac(), 
                         mapping = aes(x = Variety.fac, 
                                       y = Cob_weight, 
                                       group = Variety.fac,
                                       colour = Variety.fac )) +
      geom_boxplot() +
      geom_point(alpha = 0.1) +
      ggtitle("Cob_weight against Variety")
    
    
    gg.site <- ggplot(dataset2.fac(), 
                      mapping = aes(x = Site, 
                                    y = Cob_weight, 
                                    group = Site,
                                    colour = Site)) +
      geom_boxplot() +
      geom_point(alpha = 0.1) +
      ggtitle("Cob_weight against Site")
    
    
    gg.height.log <- ggplot(dataset2.fac(), 
                            mapping = aes(x = Plant_height, 
                                          y = Cob_weight)) +
      geom_point(alpha = 0.3) +
      geom_smooth(se = TRUE) +
      #facet_grid(~ Variety) +
      scale_x_log10() +
      ggtitle("Cob_weight against the log of Plant_height")
    
    ggarrange(gg.fertilizer, gg.site, gg.height, gg.height.log, gg.variety, 
              ncol = 2, nrow = 3)
    
    
  })
  
  
  output$explanation.log.3 <- renderText({
    HTML("The variable <em>Plant_height</em> is right skewed; 
    for this reason a second graph is plotted, where this variable is 
      log-transformed.")
  })
  
  
  output$explanation.plots.together3 <- renderText({
    HTML("It is possible to add a dimension to the previous plots.")
  })
  
  
  output$marginal.plots.together3 <- renderPlot({
    
    gg.fertilizer.site <- ggplot(dataset2.fac(),
                                 mapping = aes(x = Fertilizer, 
                                               y = Cob_weight, 
                                               colour = Site)) +
      geom_point(alpha = 0.3) +
      geom_smooth(se = TRUE) +
      ggtitle("Cob_weight against Fertilizer")
    
    gg.height.site <- ggplot(dataset2.fac(), 
                             mapping = aes(x = Plant_height, 
                                           y = Cob_weight, 
                                           colour = Site)) +
      geom_point(alpha = 0.3) +
      geom_smooth(se = TRUE) +
      scale_x_log10() +
      ggtitle("Cob_weight against log of Plant_height")
    
    
    gg.fertilizer.variety <- ggplot(dataset2.fac(), 
                                    mapping = aes(x = Fertilizer, 
                                                  y = Cob_weight, 
                                                  colour = Variety.fac)) +
      geom_point(alpha = 0.3) +
      geom_smooth(se = TRUE) +
      ggtitle("Cob_weight against Fertilizer")
    
    
    gg.height.variety <- ggplot(dataset2.fac(), 
                                mapping = aes(x = Plant_height, 
                                              y = Cob_weight, 
                                              colour = Variety.fac)) +
      geom_point(alpha = 0.3) +
      geom_smooth(se = TRUE) +
      scale_x_log10() +
      ggtitle("Cob_weight against log of Plant_height")
    
    
    ggarrange(gg.fertilizer.site, gg.height.site, 
              gg.fertilizer.variety, gg.height.variety)
    
    
  })
  
  
  
  ## Linear model 
  InputModel.lm2 <- reactive({
    lm(Cob_weight ~ Fertilizer + Site + log(Plant_height) + Variety.fac, 
       data = dataset2.fac())
  }) 
  
  ## Title for summary
  output$summary.title2 <- renderText({
    #req(n.simulate())
    
    HTML(paste0("<h4>","Summary of the model","</h4>"))
  })
  
  ## Render summary of the linear model
  output$model.lm2 <- renderPrint({
    #req(n.simulate())
    
    summary(InputModel.lm2())
  })
  
  ## Title for plot
  output$plot.title2 <- renderText({
    #req(n.simulate())
    
    HTML(paste0("<h4>","Fitted values of the linear model","</h4>"))
  })
  
  # ## Plot the dataset and the linear model
  # output$plot.lm2 <- renderPlot({
  #   #req(n.simulate())
  #   
  #   coef.lm <- coef(InputModel.lm2())
  #   
  #   ## Add predicted values to the dataset
  #   dataset.predict <- dataset2.fac() %>%
  #     mutate( fitted.lm = predict(InputModel.lm2()))
  #   
  #   
  #   ## Plot fitted values
  #   ## First for Fertilizer/Plant_height, highlighting Site
  #   mean.fertilizer <- mean(dataset.predict$Fertilizer)
  #   log.mean.height <- log(mean(dataset.predict$Plant_height))
  #   
  #   ## Plant_height, highlighting site
  #   gg.height.site <- ggplot(dataset.predict,
  #                            mapping = aes(x = Plant_height,
  #                                          y = fitted.lm)) +
  #     geom_point(alpha = 0.3, mapping = aes(colour = Site)) +
  #     scale_x_log10() +
  #     ggtitle("Fitted values against log of Plant_height, highlighting Site") +
  #     
  #     
  #     ## Linear model for Site Lausanne
  #     geom_abline(intercept = coef.lm["(Intercept)"] + 
  #                   coef.lm["Fertilizer"] * mean.fertilizer,
  #                 slope = exp(coef.lm["log(Plant_height)"]),
  #                 alpha = 0.5,
  #                 color = "red") +
  #     
  #     
  #     ## Linear model for Site Locarno
  #     geom_abline(intercept = coef.lm["(Intercept)"] + 
  #                   coef.lm["SiteLocarno"] + 
  #                   coef.lm["Fertilizer"] * mean.fertilizer,
  #                 slope = exp(coef.lm["log(Plant_height)"]),
  #                 alpha = 0.5,
  #                 color = "green4") +
  #     
  #     ## Linear model for Site Zurich
  #     geom_abline(intercept = coef.lm["(Intercept)"] + 
  #                   coef.lm["SiteZurich"] + 
  #                   coef.lm["Fertilizer"] * mean.fertilizer,
  #                 slope = exp(coef.lm["log(Plant_height)"]),
  #                 alpha = 0.5,
  #                 color = "blue") 
  #   
  #   
  #   ## Add (or not) observed values
  #   gg.height.values <- if ( input$observed.values.lm2 ) {
  #     gg.height.site + 
  #       geom_point(alpha = 0.3, mapping = aes(y = Cob_weight), 
  #                  alpha = 0.5, 
  #                  colour = "darkgrey") 
  #   } else {
  #     
  #     gg.height.site
  #     
  #   }
  #   
  #   
  #   
  #   ## Fertilizer highlighting site
  #   gg.fertilizer.site <- ggplot(dataset.predict,
  #                                mapping = aes(x = Fertilizer,
  #                                              y = fitted.lm)) +
  #     geom_point(alpha = 0.3, mapping = aes(colour = Site)) +
  #     ggtitle("Fitted values against Fertilizer, highlighting Site") +
  #     
  #     
  #     ## Linear model for Site Lausanne
  #     geom_abline(intercept = coef.lm["(Intercept)"] +
  #                   coef.lm["log(Plant_height)"] * log.mean.height,
  #                 slope = coef.lm["Fertilizer"],
  #                 alpha = 0.5,
  #                 color = "red") +
  #     
  #     ## Linear model for Site Locarno
  #     geom_abline(intercept = coef.lm["(Intercept)"] + 
  #                   coef.lm["SiteLocarno"] +
  #                   coef.lm["log(Plant_height)"] * log.mean.height,
  #                 slope = coef.lm["Fertilizer"],
  #                 alpha = 0.5,
  #                 color = "green4") +
  #     
  #     ## Linear model for Site Zurich
  #     geom_abline(intercept = coef.lm["(Intercept)"] + 
  #                   coef.lm["SiteZurich"] + 
  #                   coef.lm["log(Plant_height)"] * log.mean.height,
  #                 slope = coef.lm["Fertilizer"],
  #                 alpha = 0.5,
  #                 color = "blue") 
  #   
  #   
  #   
  #   ## Add (or not) observed values
  #   gg.fertilizer.values <- if ( input$observed.values.lm2 ) {
  #     gg.fertilizer.site + 
  #       geom_point(alpha = 0.3, mapping = aes(y = Cob_weight), 
  #                  alpha = 0.5, 
  #                  colour = "darkgrey") 
  #   } else {
  #     
  #     gg.fertilizer.site
  #     
  #   }
  #   
  #     
  #     ## Plant_height highlighting variety
  #     gg.height.variety <- ggplot(dataset.predict,
  #                                 mapping = aes(x = Plant_height,
  #                                               y = fitted.lm)) +
  #       geom_point(alpha = 0.3, mapping = aes(colour = Variety.fac)) +
  #       scale_x_log10() +
  #       ggtitle("Fitted values against log of Plant_height, highlighting Variety") +
  #       
  #       
  #       ## Linear model for First variety
  #       geom_abline(intercept = coef.lm["(Intercept)"] + 
  #                     coef.lm["Fertilizer"] * mean.fertilizer,
  #                   slope = exp(coef.lm["log(Plant_height)"]),
  #                   alpha = 0.5,
  #                   color = "red") +
  #       
  #       
  #       ## Linear model for second variety
  #       geom_abline(intercept = coef.lm["(Intercept)"] + 
  #                     coef.lm["Variety.facSilage"] + 
  #                     coef.lm["Fertilizer"] * mean.fertilizer,
  #                   slope = exp(coef.lm["log(Plant_height)"]),
  #                   alpha = 0.5,
  #                   color = "green4") 
  #     
  #     
  #     ## Add (or not) observed values
  #     gg.height.variety.values <- if ( input$observed.values.lm2 ) {
  #       gg.height.variety + 
  #         geom_point(alpha = 0.3, mapping = aes(y = Cob_weight), 
  #                    alpha = 0.5, 
  #                    colour = "darkgrey") 
  #     } else {
  #       
  #       gg.height.variety
  #       
  #     }
  #     
  #     
  #     ## Fertilizer, highlighting Variety
  #     gg.fertilizer.variety <- ggplot(dataset.predict,
  #                                     mapping = aes(x = Fertilizer,
  #                                                   y = fitted.lm)) +
  #       geom_point(alpha = 0.3, mapping = aes(colour = Variety.fac)) +
  #       ggtitle("Fitted values against Fertilizer, highlighting Variety") +
  #       
  #       
  #       ## Linear model for First variety
  #       geom_abline(intercept = coef.lm["(Intercept)"] +
  #                     coef.lm["log(Plant_height)"] * log.mean.height,
  #                   slope = coef.lm["Fertilizer"],
  #                   alpha = 0.5,
  #                   color = "red") +
  #       
  #       ## Linear model for second variety
  #       geom_abline(intercept = coef.lm["(Intercept)"] + 
  #                     coef.lm["Variety.facSilage"] +
  #                     coef.lm["log(Plant_height)"] * log.mean.height,
  #                   slope = coef.lm["Fertilizer"],
  #                   alpha = 0.5,
  #                   color = "green4") 
  #     
  #     
  #     
  #     
  #     
  #     ## Add (or not) observed values
  #     gg.fertilizer.variety.values <- if ( input$observed.values.lm2 ) {
  #       gg.fertilizer.variety + 
  #         geom_point(alpha = 0.3, mapping = aes(y = Cob_weight), 
  #                    alpha = 0.5, 
  #                    colour = "darkgrey") 
  #     } else {
  #       
  #       gg.fertilizer.variety
  #       
  #     }
  #     
  #   
  #   
  #   ggarrange(gg.fertilizer.values, gg.height.values,
  #             gg.fertilizer.variety.values, gg.height.variety.values)
  #   
  # 
  #   
  # })
  
  ## Plot the dataset and the linear model
  output$plot.lm2 <- renderPlot({
    
    coef.lm <- coef(InputModel.lm2())
    
    ## Add predicted values to the dataset
    dataset.predict <- dataset2.fac() %>%
      mutate( fitted.lm = predict(InputModel.lm2()))
    
    
    ## Plot fitted values
    mean.fertilizer <- mean(dataset.predict$Fertilizer)
    log.mean.height <- log(mean(dataset.predict$Plant_height))
    
    ## Plant_height
    gg.height <- ggplot(dataset.predict,
                        mapping = aes(x = Plant_height,
                                      y = fitted.lm)) +
      geom_point(alpha = 0.3, colour = "violet") +
      scale_x_log10() +
      ggtitle("Fitted values for Grain variety of cob against log of Plant_height in Lausanne") #+
      
      
      ## Linear model for Site Lausanne and Grain variety of cob
      # geom_abline(intercept = coef.lm["(Intercept)"] + 
      #               coef.lm["Fertilizer"] * mean.fertilizer,
      #             slope = exp(coef.lm["log(Plant_height)"]),
      #             alpha = 0.5,
      #             color = "red") 
    
    
    
    ## Add (or not) observed values
    gg.height.values <- if ( input$observed.values.lm2 ) {
      gg.height + 
        geom_point(alpha = 0.3, mapping = aes(y = Cob_weight), 
                   colour = "darkgrey") 
    } else {
      
      gg.height
      
    }
    
    
    
    ## Fertilizer 
    gg.fertilizer <- ggplot(dataset.predict,
                            mapping = aes(x = Fertilizer,
                                          y = fitted.lm)) +
      geom_point(alpha = 0.3, colour = "violet") +
      ggtitle("Fitted values for Grain variety of cob against Fertilizer in Lausanne")# +
      
      
      ## Linear model for Site Lausanne and Grain variety of cob
      # geom_abline(intercept = coef.lm["(Intercept)"] +
      #               coef.lm["log(Plant_height)"] * log.mean.height,
      #             slope = coef.lm["Fertilizer"],
      #             alpha = 0.5,
      #             color = "red") 
      # 
    
    ## Add (or not) observed values
    gg.fertilizer.values <- if ( input$observed.values.lm2 ) {
      gg.fertilizer + 
        geom_point(alpha = 0.3, mapping = aes(y = Cob_weight), 
                   alpha = 0.5, 
                   colour = "darkgrey") 
    } else {
      
      gg.fertilizer
      
    }
    
    ## Create dataframe for fitted values, when controlling for plant_height, fertilizer and variety
    mean.values.site <- data.frame(Site = c("Lausanne", "Locarno", "Zurich"), 
                                   fitted.lm = c(coef.lm["log(Plant_height)"] * log.mean.height +
                                                   coef.lm["Fertilizer"] * mean.fertilizer, 
                                                 coef.lm["(Intercept)"] +
                                                   coef.lm["log(Plant_height)"] * log.mean.height +
                                                   coef.lm["Fertilizer"] * mean.fertilizer + 
                                                   coef.lm["SiteZurich"], 
                                                 coef.lm["(Intercept)"] +
                                                   coef.lm["log(Plant_height)"] * log.mean.height +
                                                   coef.lm["Fertilizer"] * mean.fertilizer + 
                                                   coef.lm["SiteLocarno"]))
    
    ## Site
    gg.site <- ggplot(dataset.predict,
                      mapping = aes(x = Site,
                                    y = fitted.lm)) +
      geom_boxplot() +
      geom_point(alpha = 0.1, colour = "violet") +
      ggtitle("Fitted values against Site for Grain variety of cob")# +
      
      # geom_point(mean.values.site, mapping = aes(x = Site, y = fitted.lm, 
      #                                            colour = "red")) +
      # scale_colour_discrete(name = "Prediction", 
      #                        breaks = c("red"), 
      #                        labels = c("fitted value"))
    
    
    
    
    ## Add (or not) observed values
    gg.site.values <- if ( input$observed.values.lm2 ) {
      gg.site + 
        geom_point(alpha = 0.3, mapping = aes(y = Cob_weight), 
                   alpha = 0.5, 
                   colour = "darkgrey") 
    } else {
      
      gg.site
      
    }
    
    
    
    ## Create dataframe for fitted values, when controlling for plant_height, fertilizer and Site
    mean.values.variety <- data.frame(Variety.fac = c("Grain", "Silage"), 
                                      fitted.lm = c(coef.lm["log(Plant_height)"] * log.mean.height +
                                                      coef.lm["Fertilizer"] * mean.fertilizer, 
                                                    coef.lm["(Intercept)"] +
                                                      coef.lm["log(Plant_height)"] * log.mean.height +
                                                      coef.lm["Fertilizer"] * mean.fertilizer + 
                                                      coef.lm["Variety.facSilage"]))
    
    
    ## Variety
    gg.variety <- ggplot(dataset.predict,
                         mapping = aes(x = Variety.fac,
                                       y = fitted.lm)) +
      geom_boxplot() +
      geom_point(alpha = 0.1, colour = "violet") +
      ggtitle("Fitted values against Variety.fac in Lausanne")# +
      
      
      # geom_point(mean.values.variety, mapping = aes(x = Variety.fac, 
      #                                               y = fitted.lm, 
      #                                               colour = "red")) +
      # scale_colour_discrete(name = "Prediction", 
      #                        breaks = c("red"), 
      #                        labels = c("fitted value"))
    
    
    ## Add (or not) observed values
    gg.variety.values <- if ( input$observed.values.lm2 ) {
      gg.variety + 
        geom_point(alpha = 0.3, mapping = aes(y = Cob_weight), 
                   alpha = 0.5, 
                   colour = "darkgrey") 
    } else {
      
      gg.variety
      
    }
    
    ggarrange(gg.fertilizer.values, gg.height.values,
              gg.site.values, gg.variety.values)
    
    
    
  })
  
  
  
  
  InputModel.fit2 <- reactive({
    dataset2.fac() %>% 
      mutate(fit = fitted(InputModel.lm2())) %>% 
      mutate(res = residuals(InputModel.lm2()))
  })
  
  output$plot.diagnostic.lm2 <- renderPlot({
    par(mfrow = c(2, 2)#, mar = c(0.2, 1, 2, 1)
        )
    plot(InputModel.lm2(), which = c(1, 2, 3, 5))
    par(mfrow = c(1, 1))
  })
  
  
  
  
  observeEvent(input$openPlotButton_panel2, {
    output$modalUI_panel2 <- renderUI({
      modalDialog(
        id = "plotModal_panel2",
        title = "Zoomed Plot",
        plotOutput("plot.diagnostic.lm2_zoomed"),
        easyClose = TRUE,
      )
      
    })
  })
  
  output$plot.diagnostic.lm2_zoomed <- renderPlot({
    par(mfrow = c(2, 2)#, mar = c(0.2, 1, 2, 1)
        )
    plot(InputModel.lm2(), which = c(1, 2, 3, 5))
    par(mfrow = c(1, 1))
  })
  
  
  
  
  ## plot the residuals
  # output$plot.residuals2 <- renderPlot({
  #   
  #   InputModel.fit2() %>% 
  #     ggplot(mapping = aes(x = fit, y = res)) +
  #     geom_hline(yintercept = 0, colour = "violet") +
  #     geom_point(alpha = 0.3) +
  #     geom_smooth() +
  #     labs(x = "Fitted values", y = "Residuals")
  #   
  # })
  # 
  # 
  # 
  # 
  # output$qqplot.lm2 <- renderPlot({
  #   
  #   InputModel.fit2() %>%
  #     ggplot(aes(sample = res)) +
  #     geom_qq(na.rm = TRUE) +
  #     geom_qq_line(color = "red", na.rm = TRUE) +
  #     theme_bw() +
  #     labs(y = "Residuals quantiles", x = "Theoretical quantiles")
  #   
  #   
  # })
  
  
  
  
  
  
  
  
  
  #################### Third Panel - Linear model with interactions #######################
  
  
  ## eventReactive - creation dataset --------------------------------------------
  
  
  # dataset.inter <- eventReactive(n.simulate.inter(), {
  dataset.inter <- reactive({
    
    ## Sample a scenario
    scenario <- sample(nb.scenarios, 
                       size = 1)
    
    if (scenario == 1) {
      
      ## Sample fertilizer
      d.tmp <- data.frame(Site = rep(c("Lausanne", "Locarno", "Zurich"), 
                                     each = dim.dataset/3), 
                          Fertilizer = sample(50, size = dim.dataset, 
                                              replace = TRUE))
      
      
      ## Simulate response variable
      Cob_weight <-  v.intercept.lm.inter()[d.tmp$Site] + 
        v.slope.lm.inter()[d.tmp$Site] * d.tmp$Fertilizer + 
        ## simulate errors
        rnorm(dim.dataset, 
              mean = 0, 
              sd = input$sigma.lm.inter)
      
      ## Create a dataset
      d.tmp %>% 
        mutate(Cob_weight = Cob_weight)
      
    } #else if (scenario == 2){}
    
  }) ## End eventReactive
  
  
  ## Model equation --------------------------------------------------------------
  
  output$model.equation.4 <- renderText({
    HTML(paste0("y<sub>i</sub> = &beta;<sub>0</sub> +
         &beta;<sub>Fertilizer</sub> * x<sub>Fertilizer<sub>i</sub></sub> + 
         &beta;<sub>LOC</sub> * I<sub>LOC<sub>i</sub></sub> + 
         &beta;<sub>ZH</sub> * I<sub>ZH<sub>i</sub></sub>  + 
         &beta;<sub>LOC , Fertilizer </sub> * 
         I<sub>LOC<sub>i</sub></sub> * x<sub>Fertilizer<sub>i</sub></sub> + 
         &beta;<sub>ZH , Fertilizer</sub> * 
         I<sub>ZH<sub>i</sub></sub> * x<sub>Fertilizer<sub>i</sub></sub> +
         &epsilon;<sub>i</sub>, &ensp; i = 1,..., ", dim.dataset), 
         ",&ensp; <em>e</em> ~ N(&mu; , &sigma;<sub>&epsilon; </sub><sup>2</sup>)")
  })
  
  
  ## Plots -----------------------------------------------------------------------
  
  ## Marginal plot
  output$marginal.plot.lm.inter <- renderPlot({
    gg.fertilizer <- ggplot(dataset.inter(), mapping = aes(x = Fertilizer, 
                                                           y = Cob_weight)) +
      geom_point(alpha = 0.3) +
      geom_smooth(se = TRUE) +
      ggtitle("Cob_weight against Fertilizer")
    
    gg.site <- ggplot(dataset.inter(), mapping = aes(x = Site, 
                                                     y = Cob_weight,
                                                     colour = Site)) +
      geom_boxplot() +
      geom_point(alpha = 0.2) +
      ggtitle("Cob_weight against Site")
    
    ggarrange(gg.fertilizer, gg.site)
  })
  
  output$explanation.plots.together.inter <- renderText(
    HTML("We can put these two marginal plots together, 
         i.e. we can plot Cob_weight against Fertilizer, 
         highlighting the different sites")
  )
  
  output$marginal.plots.together.inter <- renderPlot(
    ggplot(dataset.inter(), mapping = aes(x = Fertilizer, 
                                          y = Cob_weight,
                                          colour = Site)) +
      geom_point(alpha = 0.3) +
      geom_smooth(se = TRUE) +
      ggtitle("Cob_weight against Fertilizer, highlighting Site")
  )
  
  ## Linear model with interactions
  InputModel.lm.inter <- reactive({
    lm(Cob_weight ~ Fertilizer * Site, data = dataset.inter())
  })
  
  ## Title for summary
  output$summary.title.inter <- renderText({
    #req(n.simulate.inter())
    
    HTML(paste0("<h4>","Summary of the model","</h4>"))
  })
  
  ## Render summary of the model
  output$model.lm.inter <- renderPrint({
    # req(n.simulate.inter())
    
    summary(InputModel.lm.inter())
    
  })
  
  ## Title for plot
  output$plot.title.inter <- renderText({
    #req(n.simulate.inter())
    
    HTML(paste0("<h4>","Fitted values of the linear model","</h4>"))
    
  })
  
  ## Plot the dataset and the model
  output$plot.lm.inter <- renderPlot({
    #req(n.simulate.inter())
    
    
    coef.lm.inter <- coef(InputModel.lm.inter())
    dataset.fitted <- dataset.inter() %>% 
      mutate(fitted = predict(InputModel.lm.inter()))
    
    gg.inter <- ggplot(dataset.fitted,
                       mapping = aes(x = Fertilizer, y = fitted)) +
      geom_point(alpha = 0.3, mapping = aes(colour = Site)) +
      
      ## Linear model with interactions for Lausanne
      geom_abline(intercept = coef.lm.inter["(Intercept)"],
                  slope = coef.lm.inter["Fertilizer"],
                  alpha = 0.5,
                  color = "red") +
      
      ## Linear model with interactions for Locarno 
      geom_abline(intercept = coef.lm.inter["(Intercept)"] + 
                    coef.lm.inter["SiteLocarno"],
                  slope = coef.lm.inter["Fertilizer"] +
                    coef.lm.inter["Fertilizer:SiteLocarno"],
                  alpha = 0.5, 
                  color = "green4") +
      
      ## Linear model with interactions for Zurich
      geom_abline(intercept = coef.lm.inter["(Intercept)"] + 
                    coef.lm.inter["SiteZurich"],
                  slope = coef.lm.inter["Fertilizer"] + 
                    coef.lm.inter["Fertilizer:SiteZurich"],
                  alpha = 0.5,
                  color = "blue")
    
    if ( input$observed.values.lm.inter ) {
      
      gg.inter + 
        geom_point(alpha = 0.3, mapping = aes(y = Cob_weight), 
                   colour = "darkgrey",
                   alpha = 0.5)
      
    } else {
      
      gg.inter
      
    }
  })
  
  
  
  InputModel.fit.inter <- reactive({
    dataset.inter() %>% 
      mutate(fit = fitted(InputModel.lm.inter())) %>% 
      mutate(res = residuals(InputModel.lm.inter()))
  })
  
  
  ## Diagnostic plots
  output$plot.diagnostic.lm.inter <- renderPlot({
    par(mfrow = c(2, 2)#, mar = c(0.2, 1, 2, 1)
        )
    plot(InputModel.lm.inter(), which = c(1, 2, 3, 5))
    par(mfrow = c(1, 1))
  })
  
  
  
  observeEvent(input$openPlotButton_panel3, {
    output$modalUI_panel3 <- renderUI({
      modalDialog(
        id = "plotModal_panel3",
        title = "Zoomed Plot",
        plotOutput("plot.diagnostic.lm3_zoomed"),
        easyClose = TRUE,
      )
      
    })
  })
  
  output$plot.diagnostic.lm3_zoomed <- renderPlot({
    par(mfrow = c(2, 2)#, mar = c(0.2, 1, 2, 1)
        )
    plot(InputModel.lm.inter(), which = c(1, 2, 3, 5))
    par(mfrow = c(1, 1))
  })
  
  ## plot the residuals
  # output$plot.residuals.inter <- renderPlot({
  #   
  #   InputModel.fit.inter() %>%
  #     ggplot(mapping = aes(x = fit, y = res)) +
  #     geom_hline(yintercept = 0, colour = "violet") +
  #     geom_point(alpha = 0.3) +
  #     geom_smooth() +
  #     labs(x = "Fitted values", y = "Residuals") 
  #   
  # })
  # 
  # 
  # ## QQ-plot
  # output$qqplot.lm.inter <- renderPlot({
  #   
  #   InputModel.fit.inter() %>%
  #     ggplot(aes(sample = res)) +
  #     geom_qq(na.rm = TRUE) +
  #     geom_qq_line(color = "red", na.rm = TRUE) +
  #     theme_bw() +
  #     labs(y = "Residuals quantiles", x = "Theoretical quantiles")
  #   
  # })
  
  
  #################### Forth Panel - Linear model with quadratic effects #######################
  
  
  ## Simulate dataset ----------------------------------------------------------
  
  
  #dataset.quad <- eventReactive(n.simulate.quad(), {
  dataset.quad <- reactive({
    
    ## Sample a scenario
    scenario <- sample(nb.scenarios, 
                       size = 1)
    
    if (scenario == 1) {
      
      ## Sample fertilizer
      d.tmp <- data.frame(Site = rep(c("Lausanne", "Locarno", "Zurich"), 
                                     each = dim.dataset/3), 
                          Fertilizer = sample(50, size = dim.dataset, 
                                              replace = TRUE))
      
      
      ## Simulate response variable
      Cob_weight <- v.intercept.lm.quad()[d.tmp$Site] + 
        v.slope.x.lm.quad()[d.tmp$Site] * d.tmp$Fertilizer + 
        v.slope.x.sq.lm.quad()[d.tmp$Site] * d.tmp$Fertilizer^2 +
        ## simulate errors
        rnorm(dim.dataset, 
              mean = 0, 
              sd = input$sigma.lm.quad)
      
      ## Create a dataset
      d.tmp %>% 
        mutate(Cob_weight = Cob_weight)
      
    } #else if (scenario == 2){}
    
  }) ## End eventReactive
  
  
  ## Model equation --------------------------------------------------------------
  
  output$model.equation.5 <- renderText({
    HTML(paste0("y<sub>i</sub> = &beta;<sub>0</sub> + 
    &beta;<sub>Fertilizer</sub> * x<sub>Fertilizer<sub>i</sub></sub> +
    &beta;<sub>LOC</sub> * I<sub>LOC<sub>i</sub></sub> + 
         &beta;<sub>ZH</sub> * I<sub>ZH<sub>i</sub></sub> + 
         &beta;<sub>Fertilizer<sup>2</sup></sub> *
         x<sup>2</sup><sub>Fertilizer<sup>2</sup><sub>i</sub></sub> + 
         &epsilon;<sub>i</sub>, &ensp; i = 1,..., ", dim.dataset), 
         ",&ensp; <em>e</em> ~ N(&mu; , &sigma;<sub>&epsilon; </sub><sup>2</sup>)")
  })
  
  
  ## Plots ---------------------------------------------------------------------
  
  ## Marginal plot
  output$marginal.plot.lm.quad <- renderPlot({
    gg.fertilizer <- ggplot(dataset.quad(), mapping = aes(x = Fertilizer, 
                                                          y = Cob_weight)) +
      geom_point(alpha = 0.3) +
      geom_smooth(se = TRUE) +
      ggtitle("Cob_weight against Fertilizer")
    
    gg.site <-  ggplot(dataset.quad(), mapping = aes(x = Site, 
                                                     y = Cob_weight,
                                                     colour = Site)) +
      geom_boxplot() +
      geom_point(alpha = 0.2) +
      ggtitle("Cob_weight against Site")
    
    ggarrange(gg.fertilizer, gg.site)
  })
  
  ## Linear model with quadratic effects
  InputModel.lm.quad <- reactive({
    lm(Cob_weight ~  Fertilizer + Site + I(Fertilizer^2), 
       data = dataset.quad())
  })
  
  output$explanation.plots.together.quad <- renderText({
    HTML("We can put together these two marginal graphs,
         by plotting Cob_weight against Fertilizer, highlighting the Site")
  })
  
  output$marginal.plots.together.quad <- renderPlot({
    ggplot(dataset.quad(), mapping = aes(x = Fertilizer, 
                                         y = Cob_weight, 
                                         colour = Site)) +
      geom_point(alpha = 0.3) +
      geom_smooth(se = TRUE) +
      ggtitle("Cob_weight against Fertilizer, highlighting Site")
    
  })
  
  
  ## Title for summary
  output$summary.title.quad <- renderText({
    # req(n.simulate.quad())
    
    HTML(paste0("<h4>","Summary of the model","</h4>"))
  })
  
  ## Render summary of the model
  output$model.lm.quad <- renderPrint({
    # req(n.simulate.quad())
    
    summary(InputModel.lm.quad())
    
  })
  
  ## Title for plot
  output$plot.title.quad <- renderText({
    #req(n.simulate.quad())
    
    HTML(paste0("<h4>","Fitted values of the linear model","</h4>"))
    
  })
  
  ## Plot the dataset and the model
  output$plot.lm.quad <- renderPlot({
    #req(n.simulate.quad())
    
    coef.lm.quad <- coef(InputModel.lm.quad())
    dataset.fitted <- dataset.quad() %>% 
      mutate(fitted = predict(InputModel.lm.quad()))
    
    gg.quad <- ggplot(dataset.fitted,
                      mapping = aes(x = Fertilizer, 
                                    y = fitted,
                                    colour = Site)) +
      geom_point(alpha = 0.3) +
      
      ## Linear model with interactions for Lausanne
      stat_function(fun = function(x){
        coef.lm.quad["(Intercept)"] + 
          coef.lm.quad["Fertilizer"] * x +
          coef.lm.quad["I(Fertilizer^2)"] * x^2},
        alpha = 0.5,
        color = "red") +
      
      ## Linear model with interactions for Locarno
      stat_function(fun = function(x){
        coef.lm.quad["(Intercept)"] +
          coef.lm.quad["SiteLocarno"] +
          coef.lm.quad["Fertilizer"] * x +
          coef.lm.quad["I(Fertilizer^2)"] * x^2},
        alpha = 0.5,
        color = "green") +
      
      ## Linear model with interactions for Zurich
      stat_function(fun = function(x){
        coef.lm.quad["(Intercept)"] +
          coef.lm.quad["SiteZurich"] +
          coef.lm.quad["Fertilizer"] * x +
          coef.lm.quad["I(Fertilizer^2)"] * x^2},
        alpha = 0.5,
        color = "blue")
    
    if ( input$observed.values.lm.quad ) {
      
      gg.quad +
        geom_point(alpha = 0.3, mapping = aes(y = Cob_weight),
                   colour = "darkgrey",
                   alpha = 0.5)
      
    } else {
      
      gg.quad
      
    }
    
    
  })
  
  
  InputModel.fit.quad <- reactive({
    dataset.quad() %>% 
      mutate(fit = fitted(InputModel.lm.quad())) %>% 
      mutate(res = residuals(InputModel.lm.quad()))
  })
  
  
  ## Diagnostic plots
  output$plot.diagnostic.lm.quad <- renderPlot({
    par(mfrow = c(2, 2)#, mar = c(0.2, 1, 2, 1)
        )
    plot(InputModel.lm.quad(), which = c(1, 2, 3, 5))
    par(mfrow = c(1, 1))
  })
  
  
  observeEvent(input$openPlotButton_panel4, {
    output$modalUI_panel4 <- renderUI({
      modalDialog(
        id = "plotModal_panel4",
        title = "Zoomed Plot",
        plotOutput("plot.diagnostic.lm4_zoomed"),
        easyClose = TRUE,
      )
      
    })
  })
  
  output$plot.diagnostic.lm4_zoomed <- renderPlot({
    par(mfrow = c(2, 2)#, mar = c(0.2, 1, 2, 1)
        )
    plot(InputModel.lm.quad(), which = c(1, 2, 3, 5))
    par(mfrow = c(1, 1))
  })
  
  
  ## plot the residuals
  # output$plot.residuals.quad <- renderPlot({
  #   
  #   InputModel.fit.quad() %>%
  #     ggplot(mapping = aes(x = fit, y = res)) +
  #     geom_hline(yintercept = 0, colour = "violet") +
  #     geom_point(alpha = 0.3) +
  #     geom_smooth() +
  #     labs(x = "Fitted values", y = "Residuals") 
  #   
  # })
  # 
  # 
  # ## QQ-plot
  # 
  # output$qqplot.lm.quad <- renderPlot({
  #   
  #   InputModel.fit.quad() %>%
  #     ggplot(aes(sample = res)) +
  #     geom_qq(na.rm = TRUE) +
  #     geom_qq_line(color = "red", na.rm = TRUE) +
  #     theme_bw() +
  #     labs(y = "Residuals quantiles", x = "Theoretical quantiles")
  #   
  # })
  
  
  
  #################### Fifth Panel - Generalised linear model - Binomial #######################
  
  
  ## Simulate dataset -----------------------------------------------------------
  
  
  #dataset.glm.bin <- eventReactive(n.simulate.glm.bin(), {
  dataset.glm.bin <- reactive({
    ## Sample a scenario
    scenario <- sample(nb.scenarios,
                       size = 1)
    
    if (scenario == 1) {
      
      ## Sample fertilizer
      Fertilizer <- runif(dim.dataset, min = 0, max = 100)
      
      # Rescale Fertilizer
      Fertilizer.rescaled <- (Fertilizer - 50) / sd(Fertilizer)
      
      
      d.tmp <- data.frame(Site = rep(c("Lausanne", "Locarno", "Zurich"), 
                                     each = dim.dataset/3),
                          Fertilizer = Fertilizer)
      
      
      ## Simulate a binary response variable
      logistic_prob <- plogis(v.intercept.glm.bin()[d.tmp$Site] + 
                                v.slope.glm.bin()[d.tmp$Site] * Fertilizer.rescaled)
      
      Cob_pres <- rbinom(dim.dataset, size = 1, prob = logistic_prob)
      
      
      ## Create a dataset
      d.tmp %>%
        mutate(Cob_pres = Cob_pres)
      
    } #else if (scenario == 2){}
    
  }) ## End eventReactive
  
  
  
  
  ## Model equation --------------------------------------------------------------
  
  output$model.equation.6 <- renderText({
    HTML(paste0("log( <sup>p<sub>i</sub></sup> &frasl; <sub>1-p<sub>i</sub></sub> ) = &beta;<sub>0</sub> + 
         &beta;<sub>Fertilizer</sub> * x<sub>Fertilizer<sub>i</sub></sub> +
         &beta;<sub>LOC</sub> * I<sub>LOC<sub>i</sub></sub> + 
         &beta;<sub>ZH</sub> * I<sub>ZH<sub>i</sub></sub>, &ensp;
                i = 1,..., ", dim.dataset))
  })
  
  ## Plots ---------------------------------------------------------------------
  
  ## Marginal plot
  output$marginal.plot.glm.bin <- renderPlot({
    gg.fertilizer <- ggplot(dataset.glm.bin(), mapping = aes(x = Fertilizer, 
                                                             y = Cob_pres)) +
      geom_point(alpha = 0.3) +
      geom_smooth(method = "glm",
                  se = TRUE,
                  method.args = list(family = "binomial")) +
      ggtitle("Cob_pres against Fertilizer")
    
    ## CI ------------
    
    
    d.perc.success <- dataset.glm.bin() %>%
      ## Create counts for number of successes and not on measuring unit level 
      group_by(`Site`, `Cob_pres`) %>%
      count() %>% ## count will automatically create a count column with name "n"
      ungroup() %>%
      ##
      ## Turn into wide format 
      pivot_wider(names_from = `Cob_pres`,
                  values_from = "n") %>%
      ##
      ## Replace missing with 0 
      replace_na(list(`0` = 0,
                      `1` = 0)) %>%
      ##
      ## Add total samples per measuring unit
      group_by(`Site`) %>%
      mutate(n_all_samples = `0` + `1`) %>%
      ungroup() %>%
      ##
      ## Compute percent success
      mutate(`perc.success (%)` = round(100 * `1` / n_all_samples)) %>%
      ##
      ## Arrange results by number of samples
      arrange(`Site`)
    
    
    d.perc.success_CI <- cbind(
      binom.confint(
        x = d.perc.success$`1`,
        n = d.perc.success$n_all_samples,
        methods = "wilson"),
      d.perc.success[, c("Site")]
    )
    new_label <- paste0(as.data.frame(d.perc.success)[, "Site"])
    
    gg.site <- ggplot(data = d.perc.success_CI,
                      mapping = aes(y = mean,
                                    x = Site,
                                    colour = Site, 
                                    #x = new_label,
                                    ymin = lower,
                                    ymax = upper)) +
      scale_y_continuous(minor_breaks = NULL) +
      geom_hline(yintercept = c(0,1), colour = "gray") +
      geom_point(position = position_dodge(0.25)) +
      geom_linerange(position = position_dodge(0.25)) +
      theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
      labs(title = "Percentage affirmative cases per group", 
           x = "Site", y = "Percentage") +
      ggtitle("Cob_weight against Site")
    
    
    
    ggarrange(gg.fertilizer, gg.site)
    
  })
  
  output$explanation.plots.together.bin <- renderText({
    HTML("We can put these two graphs together, 
         by plotting Con_nb against Fertilizer, highlighting Site")
  })
  
  output$marginal.plots.together.bin <- renderPlot({
    ggplot(dataset.glm.bin(), mapping = aes(x = Fertilizer, 
                                            y = Cob_pres,
                                            colour = Site)) +
      geom_point(alpha = 0.3) +
      geom_smooth(method = "glm",
                  se = TRUE,
                  method.args = list(family = "binomial")) +
      ggtitle("Cob_pres against Fertilizer, highlighting Site")
  })
  
  ## Generalised linear model
  InputModel.glm.bin <- reactive({
    glm(Cob_pres ~ Fertilizer + Site,
        family = binomial,
        data = dataset.glm.bin())
  })
  
  ## Title for summary
  output$summary.title.glm.bin <- renderText({
    #req(n.simulate.glm.bin())
    
    HTML(paste0("<h4>","Summary of the model","</h4>"))
  })
  
  ## Render summary of the model
  output$model.glm.bin <- renderPrint({
    #req(n.simulate.glm.bin())
    
    summary(InputModel.glm.bin())
  })
  
  ## Title for plot
  output$plot.title.glm.bin <- renderText({
    #req(n.simulate.glm.bin())
    
    HTML(paste0("<h4>","Fitted values of the generalised linear model","</h4>"))
  })
  
  
  ## Plot the dataset and the linear model
  output$plot.glm.bin <- renderPlot({
    #req(n.simulate.glm.bin())
    
    coef.glm.bin <- coef(InputModel.glm.bin())
    
    
    ggplot(dataset.glm.bin(),
           mapping = aes(x = Fertilizer, y = Cob_pres)) +
      geom_point(alpha = 0.3, mapping = aes(colour = Site)) +
      
      
      ## Generalised linear model for Lausanne
      stat_function(fun = function(x){
        ilogit(coef.glm.bin["(Intercept)"] + 
                 coef.glm.bin["Fertilizer"] * x)},
        color = "red") +
      
      ## Generalised linear model for Locarno
      stat_function(fun = function(x){
        ilogit(coef.glm.bin["(Intercept)"] + 
                 coef.glm.bin["SiteLocarno"] + 
                 coef.glm.bin["Fertilizer"] * x)},
        color = "green4") +
      
      ## Linear model with interactions for Zurich
      stat_function(fun = function(x){
        ilogit(coef.glm.bin["(Intercept)"] + 
                 coef.glm.bin["SiteZurich"] + 
                 coef.glm.bin["Fertilizer"] * x)},
        color = "blue")
    
    
  })
  
  
  
  # ## Plot the residuals
  # output$plot.residuals.bin <- renderPlot({
  #   TA.plot(InputModel.glm.bin(),
  #           main = "TA-plot for GLM with binary response",
  #           show.call = FALSE)
  # })
  
  #################### Sixth Panel - Generalised linear model with interactions - Binomial #######################
  
  
  ## Simulate dataset ----------------------------------------------------------
  
  
  #dataset.glm.bin.inter <- eventReactive(n.simulate.glm.bin.inter(), {
  dataset.glm.bin.inter <- reactive({
    
    ## Sample a scenario
    scenario <- sample(nb.scenarios,
                       size = 1)
    
    if (scenario == 1) {
      
      ## Sample fertilizer
      Fertilizer <- runif(dim.dataset, min = 0, max = 100)
      
      # Rescale Fertilizer
      Fertilizer.rescaled <- (Fertilizer - 50) / sd(Fertilizer)
      
      
      d.tmp <- data.frame(Site = rep(c("Lausanne", "Locarno", "Zurich"), 
                                     each = dim.dataset/3),
                          Fertilizer = Fertilizer)
      
      
      
      ## Simulate a binary response variable
      logistic_prob <- plogis(v.intercept.glm.bin.inter()[d.tmp$Site] + 
                                v.slope.glm.bin.inter()[d.tmp$Site] * Fertilizer.rescaled)
      
      Cob_pres <- rbinom(dim.dataset, size = 1, prob = logistic_prob)
      
      ## Create a dataset
      d.tmp %>%
        mutate(Cob_pres = Cob_pres)
      
    } #else if (scenario == 2){}
    
  }) ## End eventReactive
  
  
  
  ## Titles --------------------------------------------------------------------
  
  output$intercept.title.glm.bin.inter <- renderText({
    
    HTML("<h5> Choose the Site effect </h5>")
  })
  
  output$slope.title.inter.bin.glm <- renderText({
    
    HTML("<h5> Choose the Fertilizer effect </h5>")
    
  })
  
  
  ## Model equation --------------------------------------------------------------
  
  output$model.equation.7 <- renderText({
    HTML(paste0("log(<sup>p<sub>i</sub></sup> &frasl; <sub>1-p<sub>i</sub></sub> ) = 
    &beta;<sub>0</sub> + 
    &beta;<sub>Fertilizer</sub> * x<sub>Fertilizer<sub>i</sub></sub> +
    &beta;<sub>LOC</sub> * I<sub>LOC<sub>i</sub></sub> + 
         &beta;<sub>ZH</sub> * I<sub>ZH<sub>i</sub></sub> + 
         &beta;<sub>LOC , Fertilizer</sub> * I<sub>LOC<sub>i</sub></sub> * 
         x<sub>Fertilizer<sub>i</sub></sub> + 
         &beta;<sub>ZH , Fertilizer</sub> * I<sub>ZH<sub>i</sub></sub> * 
                x<sub>Fertilizer<sub>i</sub></sub>, &ensp; i = 1,..., ", dim.dataset))
  })
  
  ## Plots --------------------------------------------------------------------
  
  ## Marginal plot
  
  output$marginal.plot.inter.glm.bin <- renderPlot({
    gg.fertilizer <- ggplot(dataset.glm.bin.inter(), 
                            mapping = aes(x = Fertilizer, 
                                          y = Cob_pres)) +
      geom_point(alpha = 0.3) +
      geom_smooth(method = "glm",
                  se = TRUE,
                  method.args = list(family = "binomial")) 
    
    
    ## CI ------------
    
    
    d.perc.success <- dataset.glm.bin.inter() %>%
      ## Create counts for number of successes and not on measuring unit level 
      group_by(`Site`, `Cob_pres`) %>%
      count() %>% ## count will automatically create a count column with name "n"
      ungroup() %>%
      ##
      ## Turn into wide format 
      pivot_wider(names_from = `Cob_pres`,
                  values_from = "n") %>%
      ##
      ## Replace missing with 0 
      replace_na(list(`0` = 0,
                      `1` = 0)) %>%
      ##
      ## Add total samples per measuring unit
      group_by(`Site`) %>%
      mutate(n_all_samples = `0` + `1`) %>%
      ungroup() %>%
      ##
      ## Compute percent success
      mutate(`perc.success (%)` = round(100 * `1` / n_all_samples)) %>%
      ##
      ## Arrange results by number of samples
      arrange(`Site`)
    
    
    d.perc.success_CI <- cbind(
      binom.confint(
        x = d.perc.success$`1`,
        n = d.perc.success$n_all_samples,
        methods = "wilson"),
      d.perc.success[, c("Site")]
    )
    new_label <- paste0(as.data.frame(d.perc.success)[, "Site"])
    
    gg.site <- ggplot(data = d.perc.success_CI,
                      mapping = aes(y = mean,
                                    x = Site,
                                    colour = Site, 
                                    #x = new_label,
                                    ymin = lower,
                                    ymax = upper)) +
      scale_y_continuous(minor_breaks = NULL) +
      geom_hline(yintercept = c(0,1), colour = "gray") +
      geom_point(position = position_dodge(0.25)) +
      geom_linerange(position = position_dodge(0.25)) +
      theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
      labs(title = "Percentage affirmative cases per group", 
           x = "Site", y = "Percentage") +
      ggtitle("Cob_weight against Site")
    
    
    
    ggarrange(gg.fertilizer, gg.site)
    
  })
  
  output$explanation.plots.together.bin.inter <- renderText({
    
    HTML("We can put these two graphs together, by plotting Con_nb against Fertilizer, highlighting Site")
  })
  
  output$marginal.plots.together.bin.inter <- renderPlot({
    
    ggplot(dataset.glm.bin.inter(), 
           mapping = aes(x = Fertilizer, 
                         y = Cob_pres, colour = Site)) +
      geom_point(alpha = 0.3) +
      geom_smooth(method = "glm",
                  se = TRUE,
                  method.args = list(family = "binomial")) 
  })
  
  
  
  ## Generalised linear model
  InputModel.glm.bin.inter <- reactive({
    glm(Cob_pres ~  Fertilizer * Site,
        family = binomial,
        data = dataset.glm.bin.inter())
  })
  
  ## Title for summary
  output$summary.title.glm.bin.inter <- renderText({
    #req(n.simulate.glm.bin.inter())
    
    HTML(paste0("<h4>","Summary of the model","</h4>"))
  })
  
  ## Render summary of the model
  output$model.glm.bin.inter <- renderPrint({
    # req(n.simulate.glm.bin.inter())
    
    summary(InputModel.glm.bin.inter())
  })
  
  ## Title for plot
  output$plot.title.glm.bin.inter <- renderText({
    #req(n.simulate.glm.bin.inter())
    
    HTML(paste0("<h4>","Fitted values of the generalised linear model","</h4>"))
  })
  # 
  ## Plot the dataset and the linear model
  output$plot.glm.bin.inter <- renderPlot({
    #req(n.simulate.glm.bin.inter())
    
    coef.glm.bin.inter <- coef(InputModel.glm.bin.inter())
    
    ggplot(dataset.glm.bin.inter(),
           mapping = aes(x = Fertilizer, y = Cob_pres)) +
      geom_point(alpha = 0.3, mapping = aes(color = Site)) +
      
      
      ## Generalised linear model for Lausanne
      stat_function(fun = function(x){
        ilogit(coef.glm.bin.inter["(Intercept)"] +
                 coef.glm.bin.inter["Fertilizer"] * x)},
        color = "red") +
      
      ## Generalised linear model for Locarno
      stat_function(fun = function(x){
        ilogit(coef.glm.bin.inter["(Intercept)"] + 
                 coef.glm.bin.inter["SiteLocarno"] + 
                 coef.glm.bin.inter["Fertilizer"] * x +
                 coef.glm.bin.inter["Fertilizer:SiteLocarno"] * x)},
        color = "green4") +
      
      ## Linear model with interactions for Zurich
      stat_function(fun = function(x){
        ilogit(coef.glm.bin.inter["(Intercept)"] +
                 coef.glm.bin.inter["SiteZurich"] + 
                 coef.glm.bin.inter["Fertilizer"] * x +
                 coef.glm.bin.inter["Fertilizer:SiteZurich"] * x)},
        color = "blue")
  })
  
  
  ## Plot the residuals
  output$plot.residuals.bin.inter <- renderPlot({
    TA.plot(InputModel.glm.bin.inter(),
            main = "TA- plot for a GLM with binary response",
            show.call = FALSE)
  })
  
  #################### Seventh Panel - Generalised linear model with quadratic effects - binomial #######################
  
  
  ## Simulate dataset ----------------------------------------------------------
  
  
  # dataset.glm.quad.eff <- eventReactive(n.simulate.glm.quad.eff(), {
  dataset.glm.quad.eff <- reactive({
    
    ## Sample a scenario
    scenario <- sample(nb.scenarios,
                       size = 1)
    
    if (scenario == 1) {
      
      ## Sample fertilizer
      Fertilizer <- runif(dim.dataset, min = 0, max = 100)
      
      Fertilizer.rescaled <- (Fertilizer - 50) / sd(Fertilizer)
      
      d.tmp <- data.frame(Site = rep(c("Lausanne", "Locarno", "Zurich"), 
                                     each = dim.dataset/3),
                          Fertilizer = Fertilizer)
      
      
      ## Calculate the linear predictor
      lin.pred <- v.intercept.glm.quad()[d.tmp$Site] + 
        v.slope.x.glm.quad()[d.tmp$Site] * Fertilizer.rescaled +
        v.slope.x.sq.glm.quad()[d.tmp$Site] * Fertilizer.rescaled^2
      
      ## Simulate the response variable
      Cob_nb <- rbinom(dim.dataset, size = 1, prob = plogis(lin.pred))
      
      
      
      ## Create a dataset
      d.tmp %>%
        mutate(Cob_nb = Cob_nb)
      
    } #else if (scenario == 2){}
    
  }) ## End eventReactive
  
  
  
  
  ## Model equation --------------------------------------------------------------
  
  output$model.equation.8 <- renderText({
    paste0(HTML("log( <sup>p<sub>i</sub></sup> &frasl; <sub>1-p<sub>i</sub></sub> ) = 
    &beta;<sub>0</sub> +
    &beta;<sub>Fertilizer</sub> * x<sub>Fertilizer<sub>i</sub></sub> +
    &beta;<sub>LOC</sub> * I<sub>LOC<sub>i</sub></sub> + 
         &beta;<sub>ZH</sub> * I<sub>ZH<sub>i</sub></sub> + 
          &beta;<sub>Fertilizer<sup>2</sup></sub> * x<sup>2</sup><sub>Fertilizer<sub>i</sub><sup>2</sup></sub>, 
        &ensp; i = 1,..., ", dim.dataset))
  })
  
  
  
  ## Plots ---------------------------------------------------------------------
  
  ## Marginal plot
  output$marginal.plot.glm.bin.quad <- renderPlot({
    
    gg.fertilizer <- ggplot(dataset.glm.quad.eff(),
                            mapping = aes(x = Fertilizer,
                                          y = Cob_nb)) +
      geom_point(alpha = 0.3) +
      geom_smooth(method = "glm",
                  se = TRUE,
                  method.args = list(family = "binomial")) 
    
    ## CI ------------
    
    
    d.perc.success <- dataset.glm.bin.inter() %>%
      ## Create counts for number of successes and not on measuring unit level 
      group_by(`Site`, `Cob_pres`) %>%
      count() %>% ## count will automatically create a count column with name "n"
      ungroup() %>%
      ##
      ## Turn into wide format 
      pivot_wider(names_from = `Cob_pres`,
                  values_from = "n") %>%
      ##
      ## Replace missing with 0 
      replace_na(list(`0` = 0,
                      `1` = 0)) %>%
      ##
      ## Add total samples per measuring unit
      group_by(`Site`) %>%
      mutate(n_all_samples = `0` + `1`) %>%
      ungroup() %>%
      ##
      ## Compute percent success
      mutate(`perc.success (%)` = round(100 * `1` / n_all_samples)) %>%
      ##
      ## Arrange results by number of samples
      arrange(`Site`)
    
    
    d.perc.success_CI <- cbind(
      binom.confint(
        x = d.perc.success$`1`,
        n = d.perc.success$n_all_samples,
        methods = "wilson"),
      d.perc.success[, c("Site")]
    )
    new_label <- paste0(as.data.frame(d.perc.success)[, "Site"])
    
    gg.site <- ggplot(data = d.perc.success_CI,
                      mapping = aes(y = mean,
                                    x = Site,
                                    colour = Site, 
                                    #x = new_label,
                                    ymin = lower,
                                    ymax = upper)) +
      scale_y_continuous(minor_breaks = NULL) +
      geom_hline(yintercept = c(0,1), colour = "gray") +
      geom_point(position = position_dodge(0.25)) +
      geom_linerange(position = position_dodge(0.25)) +
      theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
      labs(title = "Percentage affirmative cases per group", 
           x = "Site", y = "Percentage") +
      ggtitle("Cob_weight against Site")
    
    
    
    ggarrange(gg.fertilizer, gg.site)
  })
  
  
  
  
  output$explanation.plots.together.bin.quad <- renderText({
    
    HTML("We can put these two graphs together, 
         by plotting Con_nb against Fertilizer, highlighting Site")
  })
  
  output$marginal.plots.together.bin.quad <- renderPlot({
    
    ggplot(dataset.glm.quad.eff(),
           mapping = aes(x = Fertilizer,
                         y = Cob_nb, 
                         colour = Site)) +
      geom_point(alpha = 0.3) +
      geom_smooth(method = "glm",
                  se = TRUE,
                  method.args = list(family = "binomial")) 
    
  })
  
  
  
  
  ## Generalised linear model
  InputModel.glm.quad.eff <- reactive({
    glm(Cob_nb ~ Fertilizer + Site + I(Fertilizer^2),
        family = binomial,
        data = dataset.glm.quad.eff())
  })
  
  ## Title for summary
  output$summary.title.glm.quad.eff <- renderText({
    # req(n.simulate.glm.quad.eff())
    
    HTML(paste0("<h4>","Summary of the model","</h4>"))
  })
  
  ## Render summary of the model
  output$model.glm.quad.eff <- renderPrint({
    #  req(n.simulate.glm.quad.eff())
    
    summary(InputModel.glm.quad.eff())
  })
  
  ## Title for plot
  output$plot.title.glm.quad.eff <- renderText({
    # req(n.simulate.glm.quad.eff())
    
    HTML(paste0("<h4>","Fitted values of the generalised linear model","</h4>"))
  })
  # 
  ## Plot the dataset and the model
  output$plot.glm.quad.eff <- renderPlot({
    # req(n.simulate.glm.quad.eff())
    
    coef.glm.quad.eff <- coef(InputModel.glm.quad.eff())
    
    
    ggplot(dataset.glm.quad.eff(),
           mapping = aes(x = Fertilizer, y = Cob_nb)) +
      geom_point(alpha = 0.3, mapping = aes(color = Site)) +
      
      
      ## Generalised linear model for Lausanne
      stat_function(fun = function(x){
        ilogit(coef.glm.quad.eff["(Intercept)"] + 
                 coef.glm.quad.eff["Fertilizer"] * x + 
                 coef.glm.quad.eff["I(Fertilizer^2)"] * x^2
        )},
        color = "red") +
      
      ## Generalised linear model for Locarno
      stat_function(fun = function(x){
        ilogit(coef.glm.quad.eff["(Intercept)"] +
                 coef.glm.quad.eff["SiteLocarno"] + 
                 coef.glm.quad.eff["Fertilizer"] * x +
                 coef.glm.quad.eff["I(Fertilizer^2)"] * x^2 
        )},
        color = "green4") +
      
      ## Linear model with interactions for Zurich
      stat_function(fun = function(x){
        ilogit(coef.glm.quad.eff["(Intercept)"] + 
                 coef.glm.quad.eff["SiteZurich"] + 
                 coef.glm.quad.eff["Fertilizer"] * x +
                 coef.glm.quad.eff["I(Fertilizer^2)"] * x^2 )},
        color = "blue")
  })
  
  
  ## Plot the residuals
  output$plot.residuals.glm.quad <- renderPlot({
    TA.plot(InputModel.glm.quad.eff(),
            main = "TA-plot GLM with quadratic effects", 
            show.call = FALSE)
  })
  
  
  
  #################### Ninth Panel - Generalised linear model - poisson #######################
  
  
  
  ## Simulate dataset ----------------------------------------------------------
  
  
  # dataset.glm.pois <- eventReactive(n.simulate.glm.pois(), {
  dataset.glm.pois <- reactive({
    
    ## Sample a scenario
    scenario <- sample(nb.scenarios,
                       size = 1)
    
    if (scenario == 1) {
      
      ## Sample fertilizer
      Fertilizer = runif(dim.dataset, min = 0, max = 100)
      
      d.tmp <- data.frame(Site = rep(c("Lausanne", "Locarno", "Zurich"), 
                                     each = dim.dataset/3),
                          Fertilizer = Fertilizer)
      
      ## Calculate the linear predictor
      lin.pred <- v.intercept.glm.pois()[d.tmp$Site] + 
        v.slope.glm.pois()[d.tmp$Site] * Fertilizer
      
      ## Simulate the response variable
      Cob_nb <- rpois(dim.dataset, lambda = exp(lin.pred))
      
      
      ## Create a dataset
      d.tmp %>%
        mutate(Cob_nb = Cob_nb)
      
    } #else if (scenario == 2){}
    
  }) ## End eventReactive
  
  
  
  ## Model equation --------------------------------------------------------------
  
  output$model.equation.9 <- renderText({
    HTML(paste0("log(y<sub>i</sub>) = &beta;<sub>0</sub> +
         &beta;<sub>Fertilizer</sub> * x<sub>Fertilizer<sub>i</sub></sub>
          + &beta;<sub>LOC</sub> * I<sub>LOC<sub>i</sub></sub> + 
         &beta;<sub>ZH</sub> * I<sub>ZH<sub>i</sub></sub>,&ensp; i = 1,..., ", dim.dataset))
  })
  
  
  
  ## Plots --------------------------------------------------------------------
  
  ## Marginal plot
  output$marginal.plot.glm.pois <- renderPlot({
    
    gg.fertilizer <- ggplot(dataset.glm.pois(), mapping = aes(x = Fertilizer, 
                                                              y = Cob_nb)) +
      geom_point(alpha = 0.3) + 
      geom_smooth(se = TRUE) +
      ggtitle("Cob_nb against Fertilizer")
    
    gg.site <- ggplot(dataset.glm.pois(), mapping = aes(x = Site, 
                                                        y = Cob_nb, 
                                                        colour = Site)) +
      geom_boxplot() +
      geom_point(alpha = 0.2) +
      ggtitle("Cob_nb against Site")
    ggarrange(gg.fertilizer, gg.site)
    
    
  })
  
  output$explanation.plots.together.poisson <- renderText({
    HTML("The two marginal plots can be putted together")
  })
  
  output$marginal.plot.together.glm.pois <- renderPlot({
    
    
    ggplot(dataset.glm.pois(), mapping = aes(x = Fertilizer, 
                                             y = Cob_nb, 
                                             colour = Site)) +
      geom_point(alpha = 0.3) + 
      geom_smooth(se = TRUE) +
      ggtitle("Cob_nb against Fertilizer, highlighting Site")
    
  })
  
  
  
  ## Generalised linear model
  InputModel.glm.pois <- reactive({
    glm(Cob_nb ~ Fertilizer + Site,
        family = poisson,
        data = dataset.glm.pois())
  })
  
  
  ## Title for summary
  output$summary.title.glm.pois <- renderText({
    # req(n.simulate.glm.pois())
    
    HTML(paste0("<h4>","Summary of the model","</h4>"))
  })
  
  ## Render summary of the model
  output$model.glm.pois <- renderPrint({
    #req(n.simulate.glm.pois())
    
    summary(InputModel.glm.pois())
  })
  
  ## Title for plot
  output$plot.title.glm.pois <- renderText({
    # req(n.simulate.glm.pois())
    
    HTML(paste0("<h4>","Fitted values of the generalised linear model","</h4>"))
  })
  # 
  ## Plot the dataset and the model
  output$plot.glm.pois <- renderPlot({
    #req(n.simulate.glm.pois())
    
    coef.glm.pois <- coef(InputModel.glm.pois())
    
    ggplot(dataset.glm.pois(),
           mapping = aes(x = Fertilizer, y = Cob_nb)) +
      geom_point(alpha = 0.3, mapping = aes(color = Site)) +
      
      
      ## Model for Lausanne
      geom_line(mapping = aes(x = sort(Fertilizer), 
                              y = exp(predict(InputModel.glm.pois(), 
                                              newdata = data.frame(Fertilizer = sort(Fertilizer), 
                                                                   Site = "Lausanne")))),
                lwd = 1, 
                col = "red") +
      
      ## Model for Locarno
      geom_line(mapping = aes(x = sort(Fertilizer), 
                              y = exp(predict(InputModel.glm.pois(), 
                                              newdata = data.frame(Fertilizer = sort(Fertilizer), 
                                                                   Site = "Locarno")))),
                lwd = 1, 
                col = "green4") +
      
      
      ## Model for Zurich
      geom_line(mapping = aes(x = sort(Fertilizer), 
                              y = exp(predict(InputModel.glm.pois(), 
                                              newdata = data.frame(Fertilizer = sort(Fertilizer), 
                                                                   Site = "Zurich")))),
                lwd = 1, 
                col = "blue") 
  })
  
  
  ## Plot the residuals
  output$plot.residuals.pois <- renderPlot({
    TA.plot(InputModel.glm.pois(),
            main = "TA-plot for a GLM with count data",
            show.call = FALSE)
  })
  
  
  #################### Tenth Panel - Generalised linear model with interactions - poisson #######################
  
  ## Simulate dataset ----------------------------------------------------------
  
  
  # dataset.glm.pois.inter <- eventReactive(n.simulate.glm.pois.inter(), {
  dataset.glm.pois.inter <- reactive({  
    
    ## Sample a scenario
    scenario <- sample(nb.scenarios,
                       size = 1)
    
    if (scenario == 1) {
      
      ## Sample fertilizer
      Fertilizer = runif(dim.dataset, min = 0, max = 100)
      
      d.tmp <- data.frame(Site = rep(c("Lausanne", "Locarno", "Zurich"), 
                                     each = dim.dataset/3),
                          Fertilizer = Fertilizer)
      
      ## Calculate the linear predictor
      lin.pred <- v.intercept.glm.pois.inter()[d.tmp$Site] + 
        v.slope.glm.pois.inter()[d.tmp$Site] * Fertilizer
      
      ## Simulate the response variable
      Cob_nb <- rpois(dim.dataset, lambda = exp(lin.pred))
      
      
      ## Create a dataset
      d.tmp %>%
        mutate(Cob_nb = Cob_nb)
      
    } #else if (scenario == 2){}
    
  }) ## End eventReactive
  
  
  ## Generalised linear model
  InputModel.glm.pois.inter <- reactive({
    glm(Cob_nb ~  Fertilizer * Site,
        family = poisson,
        data = dataset.glm.pois.inter())
  })
  
  ## Title for summary
  output$summary.title.glm.pois.inter <- renderText({
    # req(n.simulate.glm.pois.inter())
    
    HTML(paste0("<h4>","Summary of the model","</h4>"))
  })
  
  
  ## Model equation --------------------------------------------------------------
  
  output$model.equation.10 <- renderText({
    HTML(paste0("log(y<sub>i</sub>) = &beta;<sub>0</sub> +
         &beta;<sub>F</sub> * x<sub>F<sub>i</sub></sub> +
    &beta;<sub>LOC</sub> * I<sub>LOC<sub>i</sub></sub> + 
         &beta;<sub>ZH</sub> * I<sub>ZH<sub>i</sub></sub> +
          &beta;<sub>LOC , Fertilizer</sub> * 
          I<sub>LOC<sub>i</sub></sub> * x<sub>Fertilizer<sub>i</sub></sub> + 
         &beta;<sub>ZH , Fertilizer</sub> * 
                I<sub>ZH<sub>i</sub></sub> * x<sub>Fertilizer<sub>i</sub></sub>,
                &ensp; i = 1,..., ", dim.dataset))
  })
  
  ## Plots --------------------------------------------------------------------
  
  ## Marginal plot
  output$marginal.plot.glm.pois.inter <- renderPlot({
    
    
    gg.fertilizer <- ggplot(dataset.glm.pois.inter(),
                            mapping = aes(x = Fertilizer, 
                                          y = Cob_nb)) +
      geom_point(alpha = 0.3) +
      geom_smooth(se = TRUE) +
      ggtitle("Cob_nb against Fertilizer")
    
    gg.site <- ggplot(dataset.glm.pois.inter(),
                      mapping = aes(x = Site, 
                                    y = Cob_nb, 
                                    colour = Site)) +
      geom_boxplot() +
      geom_point(alpha = 0.2) +
      ggtitle("Cob_nb against Site")
    
    ggarrange(gg.fertilizer, gg.site)
    
    
  })
  
  output$explanation.marignal.plot.together.glm.pois.inter <- renderText({
    HTML("The two marginal plots can be putted together")
  })
  
  
  output$marginal.plot.together.glm.pois.inter <- renderPlot({
    
    ggplot(dataset.glm.pois.inter(),
           mapping = aes(x = Fertilizer, 
                         y = Cob_nb, 
                         colour = Site)) +
      geom_point(alpha = 0.3) +
      geom_smooth(se = TRUE) +
      ggtitle("Cob_nb against Fertilizer, highlighting Site")
    
  })
  
  ## Render summary of the model
  output$model.glm.pois.inter <- renderPrint({
    # req(n.simulate.glm.pois.inter())
    
    summary(InputModel.glm.pois.inter())
  })
  
  ## Title for plot
  output$plot.title.glm.pois.inter <- renderText({
    # req(n.simulate.glm.pois.inter())
    
    HTML(paste0("<h4>","Fitted values of the generalised linear model","</h4>"))
  })
  # 
  ## Plot the dataset and the model
  output$plot.glm.pois.inter <- renderPlot({
    # req(n.simulate.glm.pois.inter())
    
    coef.glm.pois.inter <- coef(InputModel.glm.pois.inter())
    
    ggplot(dataset.glm.pois.inter(),
           mapping = aes(x = Fertilizer, y = Cob_nb)) +
      geom_point(alpha = 0.3, mapping = aes(color = Site)) +
      
      
      ## Model for Lausanne
      geom_line(mapping = aes(x = sort(Fertilizer), 
                              y = exp(predict(InputModel.glm.pois.inter(), 
                                              newdata = data.frame(Fertilizer = sort(Fertilizer), 
                                                                   Site = "Lausanne")))),
                lwd = 1, 
                col = "red") +
      
      ## Model for Locarno
      geom_line(mapping = aes(x = sort(Fertilizer), 
                              y = exp(predict(InputModel.glm.pois.inter(), 
                                              newdata = data.frame(Fertilizer = sort(Fertilizer), 
                                                                   Site = "Locarno")))),
                lwd = 1, 
                col = "green4") +
      
      
      ## Model for Zurich
      geom_line(mapping = aes(x = sort(Fertilizer), 
                              y = exp(predict(InputModel.glm.pois.inter(), 
                                              newdata = data.frame(Fertilizer = sort(Fertilizer), 
                                                                   Site = "Zurich")))),
                lwd = 1, 
                col = "blue") 
  })
  
  
  ## Plot the residuals
  output$plot.residuals.pois.inter <- renderPlot({
    TA.plot(InputModel.glm.pois.inter(),
            main = "TA-plot for a GLM with count data",
            show.call = FALSE)
  })
  
  
  #################### Eleventh Panel - Violating the assumptions for a linear model #######################
  
  ## Description of the problem --------------------------------
  
  output$description.violation.lin <- eventReactive(n.simulate.errors(), {
    
    if (c.type.error() == "Log-normal distribution of the error") {
      
      HTML("<p>The following situation represents some data fitted
      with a simple linear model when, in reality, the assumption of
      normality of the error does not hold.</p>
           <p>Indeed, the errors are log-normally distributed.</p>")
      
    } else if (c.type.error() == "Fit a model without interactions when needed") {
      
      HTML("<p>The following situation represents some data fitted 
      with a simple linear model without interaction when, in reality,
      some interaction is needed.</p>
      <p>Suppose we are studying the effect of a new medication on blood pressure, 
      and we are interested in examining how the effect differs based on the age
      group of the patients.</p>
      <p>In this particular case, there will be two errors. 
           Indeed, we will see that the model without interaction will predict 
           no significant effect of medication on blood pressure for both age groups,
           whereas the effect is clearly present:
           positive for old people and negative for young people.</p>")
    
    } else if (c.type.error() == "Non-constant variance") {
        
      HTML("<p> The following situation represents some data fitted
      with a simple linear model when, in reality, the assumption of
      constant variance does not hold.</p>
           <p>Indeed, the variance increases with the increase of <em>Fertilizer</em>.</p>")
      
    } else if (c.type.error() == "Fit a model without quadratic effects when needed") {
        
      HTML("<p> The following situation represents some data fitted
      with a simple linear model without any quadratic effect when, in reality,
      a quadratic effect is needed.
           <p>Indeed, <em>Fertilizer</em> has a quadratic effect on the response variable.</p>")
      }
    
  })
  
  
  
  
  
  ## Simulate dataset ------------------------------------------
  
  
  dataset.errors <- eventReactive(n.simulate.errors(), {
    
    ## Sample a scenario
    scenario <- sample(nb.scenarios,
                       size = 1)
    
    if (scenario == 1) {
      
      ## Sample fertilizer
      d.tmp <- data.frame(Site = rep(c("Lausanne", "Locarno", "Zurich"), 
                                     each = dim.dataset/3),
                          Fertilizer = sample(50, size = dim.dataset,
                                              replace = TRUE))
      
      
      ## Structure response variable
      if (c.type.error() == "Fit a model without quadratic effects when needed") {
        
        Cob_weight <- intercept.scenario1.lm.quad[d.tmp$Site] +
          slope.x.scenario1.lm.quad[d.tmp$Site] * d.tmp$Fertilizer +
          slope.x.sq.scenario1.lm.quad[d.tmp$Site] * d.tmp$Fertilizer^2
        
        v.error <- rnorm(dim.dataset)
        
      } else if (c.type.error() == "Fit a model without interactions when needed") {
        
        d.tmp2 <- data.frame(Group = rep(c("Young", "Old"), 
                                         each = dim.dataset/2),
                             Medication = sample(20, size = dim.dataset, ## mg/day
                                                 replace = TRUE))
        
        blood.pressure <- intercept.inter[d.tmp2$Group] +
          slope.lm.inter.violating.assumptions[d.tmp2$Group] * d.tmp2$Medication +
          slope.lm.inter.violating.assumptions[d.tmp2$Group] * d.tmp2$Medication * 
          intercept.inter[d.tmp2$Group]
        
        v.error <- rnorm(dim.dataset)
        
      } else {
        
        Cob_weight <- intercept.scenario1.lm[d.tmp$Site] +
          slope.scenario1.lm[d.tmp$Site] * d.tmp$Fertilizer
        
        ## Simulate error using the type selected in the ui
        if (c.type.error() == "Log-normal distribution of the error") {
          
          v.error <- rlnorm(dim.dataset, meanlog = 0, sdlog = 2)
          
          # } else if (c.type.error() == "Uniform distribution of the error") {
          #   
          #   v.error <- runif(dim.dataset, min = -2, max = 2)
          
        } else if (c.type.error() == "Non-constant variance") {
          
          v.error <- rnorm(n = dim.dataset, sd = Cob_weight / 10)
          
        } ## End else
      } ## End else
      
      
      ## Create a dataset with simulated response variable
      
      if (c.type.error() != "Fit a model without interactions when needed") {
        
        d.tmp %>%
          mutate(Cob_weight = Cob_weight + v.error)
        
      } else {
        
        d.tmp2 %>%
          mutate(blood.pressure = blood.pressure + v.error)
      }
      
      
    } #else if (scenario == 2){}
    
    
  }) ## End eventReactive
  
  
  ## Fit the model -------------------------------------------
  
  ## Linear model
  InputModel.errors <- eventReactive(n.simulate.errors(), {
    
    if (c.type.error() != "Fit a model without interactions when needed") {
      
      lm(Cob_weight ~ Fertilizer + Site,
         data = dataset.errors())
      
    } else {
      
      lm(blood.pressure ~ Group + Medication,
         data = dataset.errors())
    }
  })
  
  
  ## Summary ---------------------------------------------
  
  ## Title for summary
  output$summary.title.errors <- renderText({
    req(n.simulate.errors())
    
    HTML(paste0("<h4>","Summary of the model","</h4>"))
  })
  
  
  ## Render summary of the linear model
  output$model.errors <- renderPrint({
    req(n.simulate.errors())
    
    summary(InputModel.errors())
  })
  
  
  
  ## Plot -------------------------------------------------
  
  
  ## Title for plot
  output$plot.title.errors <- renderText({
    req(n.simulate.errors())
    
    HTML(paste0("<h4>","Fitted values of the generalised linear model","</h4>"))
  })
  
  ## Plot the dataset and the linear model
  InputPlotViolatinAssumptions <- eventReactive(n.simulate.errors(), {
    
    
    coef.lm.errors <- coef(InputModel.errors())
    
    if (c.type.error() != "Fit a model without interactions when needed") {
      
      ggplot(dataset.errors(),
             mapping = aes(x = Fertilizer, y = Cob_weight)) +
        geom_point(alpha = 0.3, mapping = aes(color = Site)) +
        
        ## Linear model for Site Lausanne
        geom_abline(intercept = coef.lm.errors["(Intercept)"],
                    slope = coef.lm.errors["Fertilizer"],
                    alpha = 0.5,
                    color = "red") +
        
        ## Linear model for Site Locarno
        geom_abline(intercept = coef.lm.errors["(Intercept)"] +
                      coef.lm.errors["SiteLocarno"],
                    slope = coef.lm.errors["Fertilizer"],
                    alpha = 0.5,
                    color = "green4") +
        
        ## Linear model for Site Zurich
        geom_abline(intercept = coef.lm.errors["(Intercept)"] +
                      coef.lm.errors["SiteZurich"],
                    slope = coef.lm.errors["Fertilizer"],
                    alpha = 0.5,
                    color = "blue")
      
    } else {
      
      ggplot(dataset.errors(),
             mapping = aes(x = Medication, y = blood.pressure)) +
        geom_point(alpha = 0.3, mapping = aes(color = Group)) +
        
        ## Linear model for Site Lausanne
        geom_abline(intercept = coef.lm.errors["(Intercept)"],
                    slope = coef.lm.errors["Medication"],
                    alpha = 0.5,
                    color = "red") +
        
        ## Linear model for Site Locarno
        geom_abline(intercept = coef.lm.errors["(Intercept)"] +
                      coef.lm.errors["GroupYoung"],
                    slope = coef.lm.errors["Medication"],
                    alpha = 0.5,
                    color = "green4") 
      
      
      
    }
    
    
  })
  
  
  output$plot.errors <- renderPlot({
    
    InputPlotViolatinAssumptions()
    
  })
  
  
  
  
  ## Residuals plots ------------------------------------------
  
  
  
  ## Title for residuals plot
  output$residuals.title.errors <- renderText({
    req(n.simulate.errors())
    
    HTML(paste0("<h4>","Plot of residuals against fitted values","</h4>"))
  })
  
  
  InputModel.fit.errors <- reactive({
    dataset.errors() %>%
      mutate(fit = fitted(InputModel.errors())) %>%
      mutate(res = residuals(InputModel.errors()))
  })
  
  output$something.amiss <- renderText({
    req(n.simulate.errors())
    
    HTML("It is already evident from the above plot that 
         something is amiss, but the diagnostic plots below 
         provide an even clearer representation of the issue.")
    
  })
  
  
  
  ## plot the diagnostic plots
  output$plot.diagnostics.errors <- renderPlot({
    par(mfrow = c(2, 2)#, mar = c(0.2, 1, 2, 1)
        )
    plot(InputModel.errors(), which = c(1, 2, 3, 5))
    par(mfrow = c(1, 1))
  })
  
  ## plot the residuals
  # output$plot.residuals.errors <- renderPlot({
  #   
  #   InputModel.fit.errors() %>%
  #     ggplot(mapping = aes(x = fit, y = res)) +
  #     geom_hline(yintercept = 0, colour = "violet") +
  #     geom_point(alpha = 0.3) +
  #     geom_smooth() +
  #     labs(x = "Fitted values", y = "Residuals")
  # 
  #   
  # })
  # 
  # 
  # 
  # 
  # output$qqplot.errors <- renderPlot({
  #   
  #   InputModel.fit.errors() %>%
  #     ggplot(aes(sample = res)) +
  #     geom_qq(na.rm = TRUE) +
  #     geom_qq_line(color = "red", na.rm = TRUE) +
  #     theme_bw() +
  #     labs(y = "Residuals quantiles", x = "Theoretical quantiles")
  #   
  #   
  # })
  
  
  observeEvent(input$openPlotButton_panel11, {
    output$modalUI_panel11 <- renderUI({
      modalDialog(
        id = "plotModal_panel11",
        title = "Zoomed Plot",
        plotOutput("plot.diagnostic.lm11_zoomed"),
        easyClose = TRUE,
      )
      
    })
  })
  
  output$plot.diagnostic.lm11_zoomed <- renderPlot({
    par(mfrow = c(2, 2)#, mar = c(0.2, 1, 2, 1)
        )
    plot(InputModel.errors(), which = c(1, 2, 3, 5))
    par(mfrow = c(1, 1))
  })
  
  
  output$explanation.diagnostic.plots <- renderText({
    req(n.simulate.errors())
    if (c.type.error() == "Log-normal distribution of the error"){
      HTML("The diagnostic plots reveal that the residuals do not follow 
             a normal distribution. Specifically, the TA-plot indicates that 
             they are not randomly and evenly distributed around zero. 
             Additionally, the QQ-plot displays a noticeable deviation from normality.")
      
    } else if (c.type.error() == "Fit a model without interactions when needed") {
      
    } else if (c.type.error() == "Non-constant variance") {
      
    } else if (c.type.error() == "Fit a model without quadratic effects when needed") {
      
      
    }
    
    
  })
  
  
  #################### Violating the assumptions of a generalised linear model - Binomial #######################
  
  
  dataset.errors.glm.bin <- eventReactive(n.simulate.errors.glm.bin(), {
    
    ## Sample a scenario
    scenario <- sample(nb.scenarios,
                       size = 1)
    
    if (scenario == 1) {
      
      ## Sample fertilizer
      Fertilizer <- runif(dim.dataset, min = 0, max = 100)
      
      Fertilizer.rescaled <- (Fertilizer - 50) / sd(Fertilizer)
      
      d.tmp <- data.frame(Site = rep(c("Lausanne", "Locarno", "Zurich"), 
                                     each = dim.dataset/3),
                          Fertilizer = Fertilizer)
      
      ## Structure response variable
      if ( input$type.error.glm.bin == "Linear instead of quadratic effects") {
        
        lin.pred <- intercept.scenario1.glm.quad.eff[d.tmp$Site] + 
          slope.x.scenario1.glm.quad.eff[d.tmp$Site] * Fertilizer.rescaled +
          slope.x.sq.scenario1.glm.quad.eff[d.tmp$Site] * Fertilizer.rescaled^2
      } ##else if {}
      
      
      ## Simulate the response variable
      Cob_nb <- rbinom(dim.dataset, size = 1, prob = plogis(lin.pred))
      
      ## Create a dataset
      d.tmp %>%
        mutate(Cob_nb = Cob_nb)
      
    } #else if (scenario == 2){}
    
    
  }) ## End eventReactive
  
  
  ## Linear model
  InputModel.errors.glm.bin <- reactive({
    req(n.simulate.errors.glm.bin())
    
    glm(Cob_nb ~ Fertilizer + Site,
        family = binomial,
        data = dataset.errors.glm.bin())
  })
  
  
  ## Title for summary
  output$summary.title.errors.glm.bin <- renderText({
    req(n.simulate.errors.glm.bin())
    
    HTML(paste0("<h4>","Summary of the model","</h4>"))
  })
  
  
  ## Render summary of the linear model
  output$model.errors.glm.bin <- renderPrint({
    req(n.simulate.errors.glm.bin())
    
    summary(InputModel.errors.glm.bin())
  })
  
  ## Title for plot
  output$plot.title.errors.glm.bin <- renderText({
    req(n.simulate.errors.glm.bin())
    
    HTML(paste0("<h4>","Fitted values of the linear model","</h4>"))
  })
  
  ## Plot the dataset and the linear model
  output$plot.errors.glm.bin <- renderPlot({
    req(n.simulate.errors.glm.bin())
    
    coef.glm.errors.bin <- coef(InputModel.errors.glm.bin())
    
    ggplot(dataset.errors.glm.bin(),
           mapping = aes(x = Fertilizer, y = Cob_nb)) +
      geom_point(alpha = 0.3, mapping = aes(color = Site)) +
      
      
      ## Generalised linear model for Lausanne
      stat_function(fun = function(x){
        ilogit(coef.glm.errors.bin["(Intercept)"] + 
                 coef.glm.errors.bin["Fertilizer"] * x)},
        color = "red") +
      
      ## Generalised linear model for Locarno
      stat_function(fun = function(x){
        ilogit(coef.glm.errors.bin["(Intercept)"] + 
                 coef.glm.errors.bin["SiteLocarno"] + 
                 coef.glm.errors.bin["Fertilizer"] * x)},
        color = "green4") +
      
      ## Linear model with interactions for Zurich
      stat_function(fun = function(x){
        ilogit(coef.glm.errors.bin["(Intercept)"] + 
                 coef.glm.errors.bin["SiteZurich"] + 
                 coef.glm.errors.bin["Fertilizer"] * x)},
        color = "blue")
  })
  
  ## Title for plot
  output$plot.title.errors.glm.bin <- renderText({
    req(n.simulate.errors.glm.bin())
    
    HTML(paste0("<h4>","Plot of residuals against fitted values","</h4>"))
  })
  
  ## Plot the residuals
  output$plot.residuals.errors.glm.bin <- renderPlot({
    TA.plot(InputModel.errors.glm.bin(),
            main = "TA-plot for GLM with binary response",
            show.call = FALSE)
  })
  
  
  
  
  #################### Violating the assumptions of a Generalised linear model - Poisson #######################
  
  
  dataset.errors.glm.pois <- eventReactive(n.simulate.errors.glm.pois(), {
    
    ## Sample a scenario
    scenario <- sample(nb.scenarios,
                       size = 1)
    
    if (scenario == 1) {
      
      ## Sample fertilizer
      Fertilizer <- runif(dim.dataset, min = 0, max = 100)
      
      
      
      d.tmp <- data.frame(Site = rep(c("Lausanne", "Locarno", "Zurich"), 
                                     each = dim.dataset/3),
                          Fertilizer = Fertilizer)
      
      Fertilizer.rescaled <- (Fertilizer - 50) / sd(Fertilizer)
      
      ## Structure response variable
      if ( input$type.error.glm.bin == "Linear instead of quadratic effects") {
        
        lin.pred <- intercept.scenario1.glm.pois.quad[d.tmp$Site] + 
          slope.x.scenario1.glm.pois.quad[d.tmp$Site] * Fertilizer.rescaled +
          slope.x.quad.scenario1.glm.pois.quad[d.tmp$Site] * Fertilizer.rescaled^2
      } ##else if {}
      
      
      ## Simulate the response variable
      Cob_nb <- rpois(dim.dataset, lambda = exp(lin.pred))
      
      ## Create a dataset
      d.tmp %>%
        mutate(Cob_nb = Cob_nb)
      
      
    } #else if (scenario == 2){}
    
    
  }) ## End eventReactive
  
  
  ## Linear model
  InputModel.errors.glm.pois <- reactive({
    req(n.simulate.errors.glm.pois())
    
    glm(Cob_nb ~ Fertilizer + Site,
        family = poisson,
        data = dataset.errors.glm.pois())
  })
  
  
  ## Title for summary
  output$summary.title.errors.glm.pois <- renderText({
    req(n.simulate.errors.glm.pois())
    
    HTML(paste0("<h4>","Summary of the model","</h4>"))
  })
  
  
  ## Render summary of the linear model
  output$model.errors.glm.pois <- renderPrint({
    req(n.simulate.errors.glm.pois())
    
    summary(InputModel.errors.glm.pois())
  })
  
  ## Title for plot
  output$plot.title.errors.glm.pois <- renderText({
    req(n.simulate.errors.glm.pois())
    
    HTML(paste0("<h4>","Plot of the data and the model","</h4>"))
  })
  
  ## Plot the dataset and the linear model
  output$plot.errors.glm.pois <- renderPlot({
    req(n.simulate.errors.glm.pois())
    
    coef.glm.errors.pois <- coef(InputModel.errors.glm.pois())
    
    ggplot(dataset.errors.glm.pois(),
           mapping = aes(x = Fertilizer, y = Cob_nb)) +
      geom_point(alpha = 0.3, mapping = aes(color = Site)) +
      
      
      ## Generalised linear model for Lausanne
      geom_line(mapping = aes(x = sort(Fertilizer), 
                              y = exp(predict(InputModel.errors.glm.pois(), 
                                              newdata = data.frame(Fertilizer = sort(Fertilizer), 
                                                                   Site = "Lausanne")))),
                lwd = 1, 
                col = "red") +
      
      ## Generalised linear model for Locarno
      geom_line(mapping = aes(x = sort(Fertilizer), 
                              y = exp(predict(InputModel.errors.glm.pois(), 
                                              newdata = data.frame(Fertilizer = sort(Fertilizer), 
                                                                   Site = "Locarno")))),
                lwd = 1, 
                col = "green4") +
      
      ## Linear model with interactions for Zurich
      geom_line(mapping = aes(x = sort(Fertilizer), 
                              y = exp(predict(InputModel.errors.glm.pois(), 
                                              newdata = data.frame(Fertilizer = sort(Fertilizer), 
                                                                   Site = "Zurich")))),
                lwd = 1, 
                col = "blue") 
  })
  
  ## Title for plot
  output$plot.title.errors.glm.pois <- renderText({
    req(n.simulate.errors.glm.pois())
    
    HTML(paste0("<h4>","Plot of residuals against fitted values","</h4>"))
  })
  
  ## Plot the residuals
  output$plot.residuals.errors.glm.pois <- renderPlot({
    TA.plot(InputModel.errors.glm.pois(),
            main = "TA-plot for GLM with binary response",
            show.call = FALSE)
  })
  
  
  
  #################### Violating the assumptions - Over dispersion #######################
  
  output$description <- renderText({
    
    HTML("<p>In almost all practical situations, overdispersion is the rule rather than the exception.</p>
    
    <p>Indeed, the Poisson distribution assumes that the mean and variance of the response variable are equal,
    which does not hold true in almost every real-life case due to the excess variability in the data.</p>
    
    <p>Two common approaches to address the issue of overdispersion are
    negative binomial models and quasi-Poisson models.</p>
    
    <p> The two methods are closely related, and yield very similar model fits.</p>
         <br>
         <p> The data we are going to analyse come from the {lme4} package and represent
         the number of ticks on the heads of red grouse chicks sampled in the field. </p>
         
         <p> <em>TICKS</em> represents the number of ticks sampled, 
         <em>HEIGHT</em> represents the height above sea level, 
         and <em>YEAR</em> represents the year (1900 + <em>YEAR</em>). </p>")
    
  })
  
  ######### Poisson ######### 
  
  # output$summary.title.errors.over.disp <- reactive({
  #   req(Input.model.errors.over.disp())
  #   
  #   HTML(paste0("<h4>","Summary of a model based on the poisson family","</h4>"))
  #   
  #   
  # })
  # Input.model.errors.over.disp <- eventReactive(n.simulate.errors.over.disp(), {
  #   
  #   glm(TICKS ~ HEIGHT * YEAR, 
  #       family = poisson,
  #       data = grouseticks)
  #   
  # })
  # 
  # output$model.errors.over.disp <- renderPrint({
  #   req(n.simulate.errors.over.disp())
  #   summary(Input.model.errors.over.disp())
  # })
  # 
  # output$plot.title.errors.over.disp <- renderText({
  #   req(Input.model.errors.over.disp())
  #   
  #   HTML(paste0("<h4>","Plot of the model based on the poisson family","</h4>"))
  #   
  # })
  # 
  # output$plot.errors.over.disp <- renderPlot({
  #   req(n.simulate.errors.over.disp())
  #   
  #   ggplot(grouseticks,
  #          mapping = aes(x = HEIGHT, y = TICKS)) +
  #     geom_point(alpha = 0.3, mapping = aes(color = YEAR)) +
  #     
  #     
  #     ## Model for age 95
  #     geom_line(mapping = aes(x = sort(HEIGHT), 
  #                             y = exp(predict(Input.model.errors.over.disp(), 
  #                                             newdata = data.frame(HEIGHT = sort(HEIGHT), 
  #                                                                  YEAR = "95")))),
  #               lwd = 1, 
  #               col = "red") +
  #     
  #     ## Model for age 96
  #     geom_line(mapping = aes(x = sort(HEIGHT), 
  #                             y = exp(predict(Input.model.errors.over.disp(), 
  #                                             newdata = data.frame(HEIGHT = sort(HEIGHT), 
  #                                                                  YEAR = "96")))),
  #               lwd = 1, 
  #               col = "green4") +
  #     
  #     ## Model for age 97
  #     geom_line(mapping = aes(x = sort(HEIGHT), 
  #                             y = exp(predict(Input.model.errors.over.disp(), 
  #                                             newdata = data.frame(HEIGHT = sort(HEIGHT), 
  #                                                                  YEAR = "97")))),
  #               lwd = 1, 
  #               col = "blue") 
  #   
  # })
  
  ######### Marginal plots ######### 
  
  
  output$marginal.plot.overdispersion <- renderPlot({
    req(Input.model.errors.over.disp.quasi())
    
    gg.height <- ggplot(grouseticks, mapping = aes(x = HEIGHT, 
                                                   y = TICKS)) +
      geom_point(alpha = 0.3) +
      geom_smooth(se = TRUE)
    
    gg.year <- ggplot(grouseticks, mapping = aes(x = YEAR, 
                                                 y = TICKS, 
                                                 colour = YEAR)) +
      geom_boxplot() +
      geom_point(alpha = 0.2)
    
    ggarrange(gg.height, gg.year, ncol = 1)
  })
  
  
  ######### quasi-poisson ######### 
  
  output$summary.title.errors.over.disp.quasi <- reactive({
    req(Input.model.errors.over.disp.quasi())
    
    HTML(paste0("<h4>","Summary of a model based on the quasi-poisson family","</h4>"))
    
    
  })
  Input.model.errors.over.disp.quasi <- eventReactive(n.simulate.errors.over.disp(), {
    
    glm(TICKS ~ HEIGHT * YEAR, 
        family = quasipoisson,
        data = grouseticks)
    
  })
  
  output$model.errors.over.disp.quasi <- renderPrint({
    req(n.simulate.errors.over.disp())
    summary(Input.model.errors.over.disp.quasi())
  })
  
  output$plot.title.errors.over.disp.quasi <- renderText({
    req(Input.model.errors.over.disp.quasi())
    
    HTML(paste0("<h4>","Plot of the model based on the quasi-poisson family","</h4>"))
    
  })
  
  output$plot.errors.over.disp.quasi <- renderPlot({
    req(n.simulate.errors.over.disp())
    
    ggplot(grouseticks,
           mapping = aes(x = HEIGHT, y = TICKS)) +
      geom_point(alpha = 0.3, mapping = aes(color = YEAR)) +
      
      
      ## Model for age 95
      geom_line(mapping = aes(x = sort(HEIGHT), 
                              y = exp(predict(Input.model.errors.over.disp.quasi(), 
                                              newdata = data.frame(HEIGHT = sort(HEIGHT), 
                                                                   YEAR = "95")))),
                lwd = 1, 
                col = "red") +
      
      ## Model for age 96
      geom_line(mapping = aes(x = sort(HEIGHT), 
                              y = exp(predict(Input.model.errors.over.disp.quasi(), 
                                              newdata = data.frame(HEIGHT = sort(HEIGHT), 
                                                                   YEAR = "96")))),
                lwd = 1, 
                col = "green4") +
      
      ## Model for age 97
      geom_line(mapping = aes(x = sort(HEIGHT), 
                              y = exp(predict(Input.model.errors.over.disp.quasi(), 
                                              newdata = data.frame(HEIGHT = sort(HEIGHT), 
                                                                   YEAR = "97")))),
                lwd = 1, 
                col = "blue") 
    
  })
  
  
  ######### negative binomial ######### 
  
  output$summary.title.errors.over.disp.nb <- reactive({
    req(Input.model.errors.over.disp.nb())
    
    HTML(paste0("<h4>","Summary of a model based on the negative-binomial family","</h4>"))
    
    
  })
  Input.model.errors.over.disp.nb <- eventReactive(n.simulate.errors.over.disp(), {
    
    MASS::glm.nb(TICKS ~ HEIGHT * YEAR,
                 data = grouseticks)
    
  })
  
  output$model.errors.over.disp.nb <- renderPrint({
    req(n.simulate.errors.over.disp())
    summary(Input.model.errors.over.disp.nb())
  })
  
  output$plot.title.errors.over.disp.nb <- renderText({
    req(Input.model.errors.over.disp.nb())
    
    HTML(paste0("<h4>","Plot of the model based on the negative-binomial family","</h4>"))
    
  })
  
  output$plot.errors.over.disp.nb <- renderPlot({
    req(n.simulate.errors.over.disp())
    
    ggplot(grouseticks,
           mapping = aes(x = HEIGHT, y = TICKS)) +
      geom_point(alpha = 0.3, mapping = aes(color = YEAR)) +
      
      
      ## Model for age 95
      geom_line(mapping = aes(x = sort(HEIGHT), 
                              y = exp(predict(Input.model.errors.over.disp.nb(), 
                                              newdata = data.frame(HEIGHT = sort(HEIGHT), 
                                                                   YEAR = "95")))),
                lwd = 1, 
                col = "red") +
      
      ## Model for age 96
      geom_line(mapping = aes(x = sort(HEIGHT), 
                              y = exp(predict(Input.model.errors.over.disp.nb(), 
                                              newdata = data.frame(HEIGHT = sort(HEIGHT), 
                                                                   YEAR = "96")))),
                lwd = 1, 
                col = "green4") +
      
      ## Model for age 97
      geom_line(mapping = aes(x = sort(HEIGHT), 
                              y = exp(predict(Input.model.errors.over.disp.nb(), 
                                              newdata = data.frame(HEIGHT = sort(HEIGHT), 
                                                                   YEAR = "97")))),
                lwd = 1, 
                col = "blue") 
    
  })
  
} ## End server

