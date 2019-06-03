#' Runs the application.
#' @export
run_app <- function() {
  shiny::runApp(system.file('spatialrisk', package='spatialrisk'))
}
