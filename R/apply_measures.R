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

#' Apply Elevation (UpDown)
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
  UpDown <- (-acos(numerator / denominator) * 180 / pi + 90)
  return(cbind(x, UpDown))
}


#' Apply Rotation (Degrees)
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
  Degrees <- sign(-x[, "x"]) * 180 * acos(-x[, "z"] / magnitude) / pi + 180
  Degrees <- 361 * Degrees / 360
  Degrees <- floor(Degrees) - 45
  Degrees <- ifelse(Degrees < 0, Degrees + 360, Degrees)
  return(cbind(x, Degrees))
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
  Radians <- sign(-x[, "x"]) * acos(-x[, "z"] / magnitude) + pi
  return(cbind(x, Radians))
}

#' Apply AGSA, ENMO, UpDown and Degrees
#'
#' @description Single function that combines calculations for acceleration, elevation and rotation measures.
#' @param x Calibrated acceleration data frame.
#' @return Measures columns appended to end of calibrated data frame.
#' @export
#' @examples
#' x <- c(0.14268, 0.21757, -0.529, -0.36383)
#' y <- c(0.26385, 0.27295, 0.29220, 0.79510)
#' z <- c(0.27722, 0.20296, 0.35092, 0.27459)
#' calibrated <- data.frame(x, y, z)
#' calibrated <- apply_all(calibrated)
apply_all <- function(x) {
  # Extract vectors
  x_val <- x$x
  y_val <- x$y
  z_val <- x$z

  # Precompute common terms
  xyz_sq_sum <- x_val^2 + y_val^2 + z_val^2
  xyz_mag <- sqrt(xyz_sq_sum)

  # ENMO and AGSA
  ENMO <- pmax(sqrt(xyz_sq_sum) - 1, 0)
  AGSA <- sqrt(xyz_sq_sum) - 1
  AGSA <- abs(AGSA)

  # UpDown
  UpDown <- (-acos(y_val / xyz_mag) * 180 / pi + 90)

  # Degrees
  xz_mag <- sqrt(x_val^2 + z_val^2)
  Degrees <- sign(-x_val) * 180 * acos(-z_val / xz_mag) / pi + 180
  Degrees <- floor(361 * Degrees / 360) - 45
  Degrees <- ifelse(Degrees < 0, Degrees + 360, Degrees)

  return(cbind(x, ENMO, AGSA, UpDown, Degrees))
}
