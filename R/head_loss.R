#' Head loss over the length of pipe
#'
#' \code{headLoss} calculate the head loss by friction in pipes by different
#' methods
#'
#' @param D  Diameter in meters
#' @param Q  Flow rate in cubic meters per second
#' @param L  Length of pipe in meters
#' @param v  Kinematic viscosity of fluid in square meters per second. By
#'           default use the value for water at 20 Celsius degree
#'           (\code{v = 1.01e-6 m^2/s}). Unnecessary for empirical
#'           equations.
#' @param g  Gravitational acceleration. By default use the value
#'           \code{g = 9.81 m/s^2}
#'
#' @return hf Head loss in meters
#' @export
#'
#' @examples
#' head_loss(D = 0.025, Q = 0.000000001, L = 200)
head_loss <- function(D, Q, L, v = 1.01e-6, g = 9.81) {

  # Reynolds number
  Re <- (4 * Q) / (pi * D * v)

  ## laminar flow
  if (Re < 2000) {
    f <- 64 / Re
    hf <- (16 * f * Q^2 * L) / (2 * g * pi^2 * D^5)
  }

  return(hf)
}
