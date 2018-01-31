#' @export
#'
chainR <- function(...) {
  shiny::runApp(appDir = system.file("app", package = "chainR"), ...)
}