#' Diameter of pipes
#'
#' \code{diameter} calculates the diameter of pipes for a given head loss
#'
#' @param  hf Total head loss in meters
#' @inheritParams head_loss
#'
#' @return D Diameter of pipe in meters
#' @export
#'
#' @examples
#' diameter(hf = 19.7, L = 100, Q = 0.0000005)
#' diameter(hf = 9.7, L = 10, Q = 0.005, RC = 0.0001)
diameter <- function(hf, Q, L, RC, v = 1.01e-6, g = 9.81, x1 = 0.06) {

  # assuming laminar flow
  D <- ((v * L * Q * 128) / (g * pi * hf))^(1 / 4)
  Re <- (4 * Q) / (pi * D * v)

  if (Re < 2000) {
    # is laminar flow: do nothing
  } else {

    # Colebrook-White - turbulent flow
    p <- 2 * sqrt(12.1 * hf / L) / Q
    q <- RC / 3.7
    r <- (2.51 * v) / sqrt(2 * g * hf / L)
    repeat {
      x2 <- x1 - ((x1^5 + p * log10(q * x1^2 + r * x1^3))) / (5 * x1^4 + p * log10(exp(1) * (2 * q + 3 * r * x1^3) / (q * x1 + 2 * r * x1^2)))
      x <- abs(x2 - x1)
      x1 <- x2
      if (x < 0.00001) break
    }
    D <- 1 / (x1)^2
  }

  return(D)
}
