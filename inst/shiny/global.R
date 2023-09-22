## Author: Nisia Trisconi
## Reviewers: Dr. Matteo Tanadini and Dr. Claude Renaux
## test


Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL", "en_GB.UTF-8")

## Maybe use checkpoint

## Load packages
library(shiny)
library(ggplot2)
library(dplyr)
library(lmtest) ## used for what? 
library(sfsmisc)
library(faraway) ## for ilogit function
#library(lme4) ## for the dataset grouseticks
library(shinyBS) ## for collapsible boxes
library(shinyjs) ## initially hide collapsible boxes
library(ggpubr) ## needed for ggarrange
library(shinyWidgets) ## dropMenu
library(rintrojs)
library(tidyr) ## function replace_na
library(binom) ## CI







############################################  Generalities ############################################ 

nb.scenarios <- 1
dim.dataset <- 306


############################################ Linear model ############################################


################## Scenario 1 ##################
## Set the intercept and the slope
intercept.scenario1.lm <- c("Lausanne" = 10,
                            "Locarno" = 20,
                            "Zurich" = 30)

slope.scenario1.lm <- c("Lausanne" = 1.5,
                        "Locarno" = 1.5,
                        "Zurich" = 1.5)

## If we are in the case of 2 factors and 2 continuous variables:

## The second variety of cob is smaller
intercept.scenario1.variety <- c("Lausanne" = -5,
                                 "Locarno" = -5,
                                 "Zurich" = -5) 

## The higher the cob is, the more they weight
slope.scenario1.lm.height <- c("Lausanne" = 1.5,
                               "Locarno" = 1.5,
                               "Zurich" = 1.5) 


################## Scenario 2 ##################
## Set the intercept and the slope



####################################### Linear model with interactions #######################################

################## Scenario 1 ##################
## Set the intercept and the slope
intercept.scenario1.lm.inter <- c("Lausanne" = 10,
                                  "Locarno" = 20,
                                  "Zurich" = 30)

slope.scenario1.lm.inter <- c("Lausanne" = 1.5,
                              "Locarno" = 1.5,
                              "Zurich" = 1.5)

############## violating the assumptions - 
## Suppose we are studying the effect of a new medication (Variable *Medication*)
## on blood pressure (Variable *blood.pressure*),
## and we are interested in examining how the effect differs based on the age group 
## of the patients (Variable *Age* with two levels: "Young" and "Old"). 
## For the "Young" age group, the relationship between *medication* and 
## *blood.pressure* might be negative. 
## This suggests that the new medication leads to a decrease in 
## blood pressure among younger individuals.
## However, for the "Old" age group. the relationship between medication 
## and blood pressure might be positive. 


intercept.inter <- c("Young" = 75,
                     "Old" = 85)

slope.lm.inter.violating.assumptions <- c("Young" = - 0.005,
                                          "Old" = 0.005)


####################################### Linear model with quadratic effects #######################################

################## Scenario 1 ##################
## Set the intercept and the slope
intercept.scenario1.lm.quad <- c("Lausanne" = 10,
                                 "Locarno" = 20,
                                 "Zurich" = 30)

slope.x.scenario1.lm.quad <- c("Lausanne" = 5,
                               "Locarno" = 5,
                               "Zurich" = 5)

slope.x.sq.scenario1.lm.quad <- c("Lausanne" = -0.07,
                                  "Locarno" = -0.07,
                                  "Zurich" = -0.07)




################## Scenario 2 ##################
## Set the intercept and the slope

####################################### Generalized Linear model - binomial #######################################

################## Scenario 1 ##################
## Set the intercept and the slope
intercept.scenario1.glm.bin <- c("Lausanne" = 0.01,
                                 "Locarno" = 0,
                                 "Zurich" = 0.5)

slope.scenario1.glm.bin <- c("Lausanne" = 1.8,
                             "Locarno" = 1.8,
                             "Zurich" = 1.8)

####################################### Generalized Linear model with interactions - binomial #######################################

################## Scenario 1 ##################
## Set the intercept and the slope
intercept.scenario1.glm.bin.inter <- c("Lausanne" = 0.01,
                                       "Locarno" = 0.2,
                                       "Zurich" = 0.5)

slope.scenario1.glm.bin.inter <- c("Lausanne" = 1.6,
                                   "Locarno" = 1.8,
                                   "Zurich" = 2.1)


####################################### Generalized Linear model - Quadratic effects Binomial #######################################

################## Scenario 1 ##################
## Set the intercept and the slope
intercept.scenario1.glm.quad.eff <- c("Lausanne" = 0.1,
                                      "Locarno" = 0.2,
                                      "Zurich" = 0.4)

slope.x.scenario1.glm.quad.eff <- c("Lausanne" = 5,
                                    "Locarno" = 5,
                                    "Zurich" = 5)

slope.x.sq.scenario1.glm.quad.eff <- c("Lausanne" = -0.02,
                                       "Locarno" = -0.02,
                                       "Zurich" = -0.02)


####################################### Generalized Linear model - poisson #######################################

################## Scenario 1 ##################
## Set the intercept and the slope
intercept.scenario1.glm.pois <- c("Lausanne" = 2,
                                  "Locarno" = 5,
                                  "Zurich" = 6)

slope.scenario1.glm.pois <- c("Lausanne" = 1.01,
                              "Locarno" = 1.01,
                              "Zurich" = 1.01)

####################################### Generalized Linear model with interactions - Poisson #######################################


################## Scenario 1 ##################
## Set the intercept and the slope
intercept.scenario1.glm.pois.inter <- c("Lausanne" = 2,
                                        "Locarno" = 5,
                                        "Zurich" = 6)

slope.scenario1.glm.pois.inter <- c("Lausanne" = 1.01,
                                    "Locarno" = 1.03,
                                    "Zurich" = 1.05)


####################################### Generalized Linear model - quadratic effects Poisson #######################################


################## Scenario 1 ##################
## Set the intercept and the slope
intercept.scenario1.glm.pois.quad <- c("Lausanne" = 2,
                                       "Locarno" = 5,
                                       "Zurich" = 6)

slope.x.scenario1.glm.pois.quad <- c("Lausanne" = 5,
                                     "Locarno" = 5,
                                     "Zurich" = 5)

slope.x.quad.scenario1.glm.pois.quad <-  c("Lausanne" = -0.02,
                                           "Locarno" = -0.02,
                                           "Zurich" = -0.02)




####################################### Generalized Linear model - Poisson overdispersion #######################################
data(grouseticks, package = "lme4")

