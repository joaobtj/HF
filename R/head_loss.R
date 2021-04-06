#' Head loss over the length of pipe
#'
#' \code{headLoss} calculate the head loss by friction in pipes by different
#' methods
#'
#' @param D  Diameter in meters
#' @param Q  Flow rate in cubic meters per second
#' @param L  Length of pipe in meters
#' @param RC Roughness coefficient. Absolute roughness (in meters) for
#'           Universal equation.
#' @param v  Kinematic viscosity of fluid in square meters per second. By
#'           default use the value for water at 20 Celsius degree
#'           (\code{v = 1.01e-6 m^2/s}). Unnecessary for empirical
#'           equations.
#' @param g  Gravitational acceleration. By default use the value
#'           \code{g = 9.81 m/s^2}
#' @param x1  Initial parameter of f for Newton-Raphson.
#'            By default \code{x1=0.06}.
#'            Unnecessary for empirical or explicit equations.
#'
#' @return hf Head loss in meters
#' @export
#'
#' @examples
#' head_loss(D = 0.025, Q = 0.000000001, L = 200)
#' head_loss(D = 0.050, Q = 0.0006, L = 200, RC = 0.0001)
head_loss <- function(D, Q, L, RC, v = 1.01e-6, g = 9.81, x1 = 0.06) {

  # Reynolds number
  Re <- (4 * Q) / (pi * D * v)

  ## laminar flow
  if (Re < 2000) {
    f <- 64 / Re
    hf <- (16 * f * Q^2 * L) / (2 * g * pi^2 * D^5)
  } else {

    # Colebrook-White
    x1 <- (1 / x1)^0.5
    w <- (RC / (3.7 * D)) + ((2.51 * x1) / Re)
    h <- (2.18 / (((RC * Re) / (3.7 * D)) + (2.51 * x1)))

    repeat {
      x2 <- x1 - (((x1 + (2 * log10(w))) / (1 + h)))
      x <- abs(x2 - x1)
      x1 <- x2
      if (x < 0.00001) break
    }

    f <- 1 / x1^2
    hf <- (16 * f * Q^2 * L) / (2 * g * pi^2 * D^5)
  }

  return(hf)
}
