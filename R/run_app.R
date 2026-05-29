#' Shiny App Bio stats
#'
#' Run this function to start the shiny app in the browser.
#'
#' @param ... Arguments passed to [shiny::runApp()], such as `port`, `host`,
#'   and `launch.browser`.
#'
#' @export

run_app <- function(...) {
  shiny::runApp(appDir = system.file("shiny", package = "shinyBio"),
                ...)
}
