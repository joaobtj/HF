#' Reynolds Number
#'
#' \code{re} calculates the Reynolds number
#'
#' @inheritParams head_loss
#' @param v Kinematic viscosity of fluid in square meters per second.
#'
#' @return re Reynolds number
#' @export
#'
#' @examples
#' re(D = 0.025, Q = 0.000000001, v = 1.01e-6)
#' re(D = 0.050, Q = 0.0006, v = 1.01e-6)
re <- function(D, Q, v) {
  Re <- (4 * Q) / (pi * D * v)
  return(Re)
}
