#' Weighted sum with logical index
#'
#' @description Computes the weighted sum of `x` using weights `w`, where `idx` is TRUE.
#'
#' @param x Numeric vector of values.
#' @param w Numeric vector of weights, same length as `x`.
#' @param idx Logical vector indicating which elements to include.
#' @keywords internal
#' @export
#' @return A single numeric value. Returns 0 if no TRUE values in `idx`.
#' @examples
#' idx_active <- c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE)
#' AGSA <- c(0.1, 0.2, 0.03, 0.04, 0.05, 0.6)
#' Duration <- c(100, 90, 80, 70, 60, 50)
#' ActiveVolume <- weighted_sum(AGSA, Duration, idx_active)
weighted_sum <- function(x, w, idx) {
  sum(x[idx] * w[idx], na.rm = TRUE)
}

#' Weighted mean with logical index
#'
#' @description Computes the weighted mean of `x` using weights `w`, where `idx` is TRUE.
#' @param x Numeric vector of values.
#' @param w Numeric vector of weights, same length as `x`.
#' @param idx Logical vector indicating which elements to include.
#' @keywords internal
#' @export
#' @return A single numeric value. Returns NA if the sum of weights is 0.
#' @examples
#' idx_active <- c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE)
#' AGSA <- c(0.1, 0.2, 0.03, 0.04, 0.05, 0.6)
#' Duration <- c(100, 90, 80, 70, 60, 50)
#' ActiveIntensity <- weighted_mean(AGSA, Duration, idx_active)
weighted_mean <- function(x, w, idx) {
  denominator <- sum(w[idx], na.rm = TRUE)
  if (is.na(denominator) || denominator == 0) {
    return(NA_real_)
  }
  weighted_sum(x, w, idx) / denominator
}


#' Compute Mx and Lx metrics
#' @details Calculates intensity for each x duration window.
#'
#' @param bouts A data frame sorted by time containing at minimum the columns TimeUTC, Duration and AGSAMean.
#' @param window Window length in seconds.
#' @param low Set to TRUE for both the maximum (Mx) and minimum (Lx) intensities
#' are returned. If FALSE, only the maximum (Mx) is returned.
#' @returns A list containing the maximum and minimum mean intensity and their
#' corresponding start times for the supplied bouts data at a given window.
#' @keywords internal
#' @export
mx_lx_window <- function(bouts, window, low = TRUE) {
  # Sort bouts by time
  bouts <- bouts[order(bouts$TimeUTC), ]

  if (nrow(bouts) == 0) {
    return(NULL)
  }

  timeutc <- bouts$TimeUTC
  agsa <- bouts$AGSAMean
  duration <- bouts$Duration

  intensity <- c()

  for (k in 1:nrow(bouts)) {
    tmp <- bouts[timeutc >= timeutc[k] &
      timeutc < (timeutc[k] + window), ]

    intensity[k] <- weighted_mean(tmp$AGSAMean, tmp$Duration, rep(TRUE, nrow(tmp)))
  }

  i_max <- which.max(intensity)
  out <- list(
    M_intensity = intensity[i_max],
    M_time      = timeutc[i_max]
  )

  if (low) {
    i_min <- which.min(intensity)
    out$L_intensity <- intensity[i_min]
    out$L_time <- timeutc[i_min]
  }

  return(out)
}


#' Compute Cx percentile
#' @details Calculates the xth percentile of cadence for a given percentile.
#'
#' @param bouts A data frame sorted by time containing at minimum a `value_col` and `duration_col`.
#' @param percentile The percentile to compute. Single integer between 0 and 100. Default is 95.
#' @param value_col The column name of the variable for which the percentile will be calculated (e.g., `"StepMean`).
#' @param duration_col The column name of the duration or weight column (e.g., `"Duration"`).
#' @returns A single numeric value or `NA` if percentile cannot be calculated
#' @keywords internal
#' @export
cx_percentile <- function(bouts,
                          percentile = 95,
                          value_col = "StepMean",
                          duration_col = "Duration") {
  percentile <- as.integer(percentile)
  percentile <- max(0, min(100, percentile))

  if (!value_col %in% names(bouts)) stop("value_col not found in bouts: ", value_col)
  if (!duration_col %in% names(bouts)) stop("duration_col not found in bouts: ", duration_col)

  bouts <- na.omit(bouts)

  if (nrow(bouts) > 0) {
    bouts <-  bouts[order(bouts[[value_col]]), ]
    v <- bouts[[value_col]]
    d <- bouts[[duration_col]]
    cutoff <- (percentile / 100) * sum(d)
    i_cutoff <- which(cumsum(d) >= cutoff)[1]
    return(v[i_cutoff])
  } else {
    return(NA)
  }
}

#' Compute Fx metrics
#' @details Calculates frequency of bouts for each x duration window.
#'
#' @param bouts A bouts data frame sorted by time containing at minimum the TimeUTC and AGSAMean columns.
#' @param AGSA_threshold Threshold for active classification in g. Default is 0.0625 (62.5mg).
#' @param duration Duration of active time in seconds. Default is 3600 seconds (60 minutes).
#' @returns A list containing the maximum and minimum bout counts and their
#' corresponding start times for the supplied bouts data at a given window.
#' @keywords internal
#' @export
fx_count <- function(bouts, AGSA_threshold = 0.0625, duration = 3600) {
  bouts <- bouts[order(bouts$TimeUTC), ]
  bouts$active <- ifelse(bouts$AGSAMean > AGSA_threshold, TRUE, FALSE)

  bouts$activecumsum <- cumsum(ifelse(bouts$active, bouts$Duration, 0))
  timeutc <- bouts$TimeUTC

  bout_count <- c()

  if (nrow(bouts) == 0) {
    return(NULL)
  }

  for (k in 1:nrow(bouts)) {
    start_i <- k
    idx <- which(bouts$activecumsum >= (bouts$activecumsum[k] + duration))
    if (length(idx) == 0) {
      end_i <- NA
    } else {
      end_i <- min(idx)
    }
    bout_count[k] <- end_i - start_i
  }

  c_max <- which.max(bout_count)
  c_min <- which.min(bout_count)

  out <- list(
    M_count = bout_count[c_max],
    M_time = timeutc[c_max],
    L_count = bout_count[c_min],
    L_time = timeutc[c_min]
  )

  return(out)
}
