#' Summary of the Treatment Effect Estimators
#'
#' The \code{summary} prints the summary from obejects of class "didnp"
#'
#' @rdname summary
#' @export
summary.didnp <- function( obj, ... ) {
  class( obj ) <- "summary.didnp"
  return( obj )
}
