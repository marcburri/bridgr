#' Summary method for bridge_model
#'
#' Implement the `forecast` method for `bridge`
#' @param bridge_model A `bridge` object.
#'
#' @export
summary <- S7::new_generic(
  "summary",
  dispatch_args ="bridge_model",
  function(bridge_model, ...) {S7::S7_dispatch()}
)


S7::method(summary, bridge_model) <- function(bridge_model, ...) {

  summary <- ""
  return(summary)
}
