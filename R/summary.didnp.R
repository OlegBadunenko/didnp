#' Summary of objects of class "didnp"
#'
#' The \code{summary} prints the summary from objects of class "didnp"
#'
#' @rdname summary
#' @export
summary.didnp <- function( obj, ... ) {
  class( obj ) <- "summary.didnp"
  return( obj )
}
