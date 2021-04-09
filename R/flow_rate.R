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
#' flow_rate(hf = 9.7, L = 10, D = 0.050, RC = 0.0001)
flow_rate <- function(hf, D, L, RC, v = 1.01e-6, g = 9.81) {

  # assuming laminar flow
  Q <- (2 * g * pi * hf * D^4) / (256 * v * L)
  Re <- re(D, Q, v)

  # laminar flow
  if (Re < 2000) {
    # is laminar flow: do nothing
  } else {
    Re_sqf <- (D / v) * (sqrt(2 * g * D * hf / L))
    f <- (1 / (-2 * log10(RC / (3.7 * D) + (2.51 / Re_sqf))))^2
    V <- sqrt((hf / L * D * 2 * g) / (f))
    Q <- (pi * (D^2)) / 4 * sqrt((hf / L * D * 2 * g) / (f))
  }

  return(Q)
}
