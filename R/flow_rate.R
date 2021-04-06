#' Flow rate
#'
#' \code{flow_rate} calculate the flow rate for a given head loss
#'
#' @param  hf Total head loss in meters
#' @inheritParams head_loss
#'
#' @return Q Flow rate in cubic meters per second
#' @export
#'
#' @examples
#' flow_rate(hf = 0.97, L = 10, D = 0.008, v = 3e-6)
flow_rate <- function(hf, D, L, v = 1.01e-6, g = 9.81) {

  # assuming laminar flow
  Q <- (2 * g * pi * hf * D^4) / (256 * v * L)
  Re <- (4 * Q) / (pi * D * v)

  # laminar flow
  if (Re < 2000) {
    return(Q)
  }
}
