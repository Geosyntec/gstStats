#' quiet function to suppresses pesky MannKendall warnings
#'
#' @description
#' via Hadley Wickham This function silences MannKendall function warnings
#'
#' @param x MannKendall stat run?
#' @return
#' * A quiet run of the MannKendall function
#' @export
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}
