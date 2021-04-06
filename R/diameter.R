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
diameter <- function(hf, Q, L, v = 1.01e-6, g = 9.81) {

  # assuming laminar flow
  D <- ((v * L * Q * 128) / (g * pi * hf))^(1 / 4)
  Re <- (4 * Q) / (pi * D * v)

  # laminar flow
  if (Re < 2000) {
    return(D)
  }
}
