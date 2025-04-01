#' Apply Absolute Gravity-Subtracted Acceleration (AGSA)
#'
#' @param x Calibrated acceleration data frame.
#' @return Measure column appended to end of calibrated data frame.
#' @export
#' @examples
#' x <- c(0.14268, 0.21757, -0.529, -0.36383)
#' y <- c(0.26385, 0.27295, 0.29220, 0.79510)
#' z <- c(0.27722, 0.20296, 0.35092, 0.27459)
#' calibrated <- data.frame(x, y, z)
#' calibrated <- apply_AGSA(calibrated)
apply_AGSA <- function(x) {
  AGSA <- abs(((rowSums(x[, c("x", "y", "z")]**2))**(1 / 2)) - 1)
  return(cbind(x, AGSA))
}

#' Apply Euclidean Norm Minus One (ENMO)
#'
#' @param x Calibrated acceleration data frame.
#' @return Measure column appended to end of calibrated data frame.
#' @export
#'
#' @examples
#' x <- c(0.14268, 0.21757, -0.529, -0.36383)
#' y <- c(0.26385, 0.27295, 0.29220, 0.79510)
#' z <- c(0.27722, 0.20296, 0.35092, 0.27459)
#' calibrated <- data.frame(x, y, z)
#' calibrated <- apply_ENMO(calibrated)
apply_ENMO <- function(x) {
  ENMO <- (((rowSums(x[, c("x", "y", "z")]**2))**(1 / 2)) - 1)
  ENMO[ENMO < 0] <- 0
  return(cbind(x, ENMO))
}

#' Apply Elevation (updown)
#'
#' @param x Calibrated acceleration data frame.
#'
#' @return Measure column appended to end of calibrated data frame.
#' @export
#' @examples
#' x <- c(0.14268, 0.21757, -0.529, -0.36383)
#' y <- c(0.26385, 0.27295, 0.29220, 0.79510)
#' z <- c(0.27722, 0.20296, 0.35092, 0.27459)
#' calibrated <- data.frame(x, y, z)
#' calibrated <- apply_updown(calibrated)
apply_updown <- function(x) {
  numerator <- x[, "y"]
  denominator <- sqrt(rowSums(x[, c("x", "y", "z")]^2))
  updown <- (-acos(numerator / denominator) * 180 / pi + 90)
  return(cbind(x, updown))
}


#' Apply Rotation (degrees)
#'
#' @param x Calibrated acceleration data frame.
#' @return Measure column appended to end of calibrated data frame.
#' @export
#' @examples
#' x <- c(0.14268, 0.21757, -0.529, -0.36383)
#' y <- c(0.26385, 0.27295, 0.29220, 0.79510)
#' z <- c(0.27722, 0.20296, 0.35092, 0.27459)
#' calibrated <- data.frame(x, y, z)
#' calibrated <- apply_degrees(calibrated)
apply_degrees <- function(x) {
  magnitude <- sqrt(rowSums(x[, c("x", "z")]^2))
  degrees <- sign(-x[, "x"]) * 180 * acos(-x[, "z"] / magnitude) / pi + 180
  degrees <- 361 * degrees / 360
  degrees <- floor(degrees) - 45
  degrees <- ifelse(degrees < 0, degrees + 360, degrees)
  return(cbind(x, degrees))
}


#' Apply Rotation (radians)
#'
#' @param x Calibrated acceleration data frame.
#' @return Measure column appended to end of calibrated data frame.
#' @export
#' @examples
#' x <- c(0.14268, 0.21757, -0.529, -0.36383)
#' y <- c(0.26385, 0.27295, 0.29220, 0.79510)
#' z <- c(0.27722, 0.20296, 0.35092, 0.27459)
#' calibrated <- data.frame(x, y, z)
#' calibrated <- apply_radians(calibrated)
apply_radians <- function(x) {
  magnitude <- sqrt(rowSums(x[, c("x", "z")]^2))
  radians <- sign(-x[, "x"]) * acos(-x[, "z"] / magnitude) + pi
  return(cbind(x, radians))
}
