#' @rdname summary
#' @export
summary.didnp <- function( obj, ... ) {
  class( obj ) <- "summary.didnp"
  return( obj )
}
