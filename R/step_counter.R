#' Function to calculate the number and variance of the steps in the data.
#'
#' @title Step Counter
#' @param step_data The data to use for calculating the steps. This should be a vector of acceleration values.
#' @param sample_frequency The sampling frequency of the data, in hertz,
#' when calculating the step number (default 100).
#' @param filter_order single integer, order of the Chebyshev bandpass filter,
#' passed to argument n of \code{\link[signal]{cheby1}}.
#' @param boundaries length 2 numeric vector specifying lower and upper bounds
#' of Chebychev filter (default \code{c(0.5, 5)} Hz),
#' passed to argument W of \code{\link[signal]{butter}} or \code{\link[signal]{cheby1}}.
#' @param Rp the decibel level that the cheby filter takes, see \code{\link[signal]{cheby1}}.
#' @param hysteresis The hysteresis applied after zero crossing. (default 100mg)
#' @param fun character vector naming functions by which to summarize steps.
#' "count" is an internally implemented summarizing function that returns step count.
#' @return Returns a vector with length fun.
#' @export
#' @importFrom signal cheby1
#' @importFrom methods is
#' @importFrom stats median
#' @examples
#' d1 <- sin(seq(0.1, 100, 0.1)) / 2 + rnorm(1000) / 10 + 1
#' Steps4 <- step_counter(d1)
#' length(Steps4)
#' mean(Steps4)
#' sd(Steps4)
#' plot(Steps4)
step_counter <- function(step_data,
                         sample_frequency = 100,
                         filter_order = 2,
                         boundaries = c(0.5, 5),
                         Rp = 3,
                         hysteresis = 0.05,
                         fun = c("GENEAcount", "mean", "sd", "stepdiff")) {
  if (missing(step_data)) {
    stop("data is missing")
  }
  if (!is.character(fun)) {
    stop("fun must be character vector of function names")
  }
  if (length(fun) < 1L) {
    stop("fun must name at least one function")
  }

  # Going to remove the amplitude variable from this step counter. Can come back to this later.
  res <- numeric(length(fun))
  names(res) <- fun

  #### Check whether an AccData object or a vector ####
  step_data <- na.omit(step_data)

  Filter <- signal::cheby1(
    n = filter_order, # order of filter
    Rp = Rp, # ripple of band pass
    W = boundaries / sample_frequency, # lower then upper frequencies of band pass
    type = "pass",
    plane = "z"
  )

  #### Apply the band pass filter ####
  filtered_data <- signal::filter(Filter, step_data)

  ## Remove NAs?
  filtered_data <- na.omit(filtered_data)

  state <- -1 # initialise step state
  interval <- 0 # initialise the interval counter
  cadence <- numeric(0) # initialise first element of array for intervals
  samples <- length(filtered_data) # loop through all samples

  if (samples > 0) {
    for (a in 1:samples) {
      if ((filtered_data[a] > hysteresis) && (state < 0)) { # new step started
        state <- 1 # set the state
        cadence[length(cadence)] <- interval + 1 # write the step interval
        cadence[length(cadence) + 1] <- 0 # initialise to record the next step
        interval <- 0 # reset the step counter
      } else if ((-1 * filtered_data[a] > hysteresis) && (state > 0)) { # hysteresis reset condition met
        state <- -1 # reset the state
        interval <- interval + 1 # increment the interval
      } else {
        interval <- interval + 1 # increment the interval
      }
      cadence[length(cadence)] <- interval # capture last part step
    }
  }

  cadence <- cadence / sample_frequency # divide by the sample frequency to get seconds

  if ("GENEAcount" %in% fun) {
    res["GENEAcount"] <- length(cadence)
    fun <- fun[fun != "GENEAcount"]
  }

  if ("mean" %in% fun) {
    if (length(cadence) < 2) {
      res["mean"] <- 0
      fun <- fun[fun != "mean"]
    } else {
      res["mean"] <- 60 / mean(cadence, na.rm = T)
      fun <- fun[fun != "mean"]
    }
  }

  if ("sd" %in% fun) {
    if (length(cadence) < 2) {
      res["sd"] <- 0
      fun <- fun[fun != "sd"]
    } else {
      res["sd"] <- 60 / sd(cadence, na.rm = T)
      fun <- fun[fun != "sd"]
    }
  }

  if ("stepdiff" %in% fun) {
    if (length(cadence) < 2) {
      res["stepdiff"] <- 0
      fun <- fun[fun != "stepdiff"]
    } else {
      res["stepdiff"] <- mean(abs(diff(cadence)))
      fun <- fun[fun != "stepdiff"]
    }
  }

  for (i in fun) {
    val <- try(get(x = i, mode = "function")(diff(cadence)))
    if (is(val, class2 = "try-error")) {
      val <- 0
    }
    if (is.na(val)) {
      val <- 0
    }
    res[i] <- val
  }

  return(res)
}


## Deprecated functions

#' @rdname step_counter
#' @param ... Additional arguments passed to internal aggregation functions.
#' @export
stepCounter <- function(...) {
  .Deprecated("step_counter")
  step_counter(...)
}

## Write MPI file history for step counter function
# Called in geneacore() after all steps (for epochs or events) have been calculated
step_counter_history <- function(MPI_filepath,
                                 sample_frequency = 100,
                                 filter_order = 2,
                                 boundaries = c(0.5, 5),
                                 Rp = 3,
                                 hysteresis = 0.05) {
  if (file.exists(MPI_filepath)) {
    MPI <- readRDS(MPI_filepath)

    MPI$file_history <- rbind(
      MPI$file_history,
      paste0(
        substr(Sys.time(), 0, 23),
        " steps calculated ",
        "(parameters: ",
        "sample_frequency = ", sample_frequency, ", ",
        "filter_order = ", filter_order, ", ",
        "boundaries = ", paste(boundaries, collapse = "; "), ", ",
        "Rp = ", Rp, ", ",
        "hysteresis = ", hysteresis, ")"
      )
    )
    saveRDS(MPI, MPI_filepath)
  }
  return(MPI)
}
