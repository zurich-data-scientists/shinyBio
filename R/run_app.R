#' Shiny App Bio stats
#'
#' Run this function to start the shiny app in the browser.
#'
#' @export
#'
#' @import shiny
#' @import ggplot2
#' @import dplyr
#' @import lmtest
#' @import sfsmisc
#' @import faraway
#' @import lme4
#' @import shinyBS
#' @import shinyjs
#' @import ggpubr
#' @import shinyWidgets 
#' @import rintrojs
#' @importFrom magrittr %>%



run_app <- function() {
  shiny::runApp(appDir = system.file("shiny", package = "shinyBio"),
                launch.browser = TRUE)
}
