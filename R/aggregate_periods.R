#' Aggregate Epochs
#'
#' @details Wrapper function that calls \code{aggregatePeriods} for epochs (duration of fixed length).
#' @param time_series Data frame to be aggregated.
#' @param measure Name of the measure columns to be included.
#' @param time Name of the time column.
#' @param sample_frequency Measurement frequency of data.
#' @param duration Time duration to aggregate in each epoch.
#' @param first_epoch_timestamp Time to start the first epoch, defaults to first record.
#' @param fun Function to apply on aggregation, defaults to mean.
#' @return Data frame of aggregated epochs.
#' @export
#' @importFrom stats sd
#' @examples
#' timestamp <- c(
#'   1619424004, 1619424005, 1619424006, 1619424007,
#'   1619424008, 1619424009, 1619424010, 1619424011,
#'   1619424012, 1619424013, 1619424014, 1619424015
#' )
#' value <- c(
#'   0.729614366, 1.729115871, 0.804973546, 2.510181118,
#'   2.23764038, 0.613203747, 0.681953275, 0.089566943,
#'   0.021042388, 2.4780338, 2.437488989, 2.632635727
#' )
#' data <- data.frame(timestamp, value)
#' aggregated <- aggregateEpochs(data,
#'   duration = 5,
#'   measure = "value",
#'   sample_frequency = 1,
#'   first_epoch_timestamp = 1619424005,
#'   time = "timestamp"
#' )
aggregateEpochs <- function(time_series,
                            measure = "AGSA",
                            time = "timestamp",
                            sample_frequency,
                            duration = NA,
                            first_epoch_timestamp = NA,
                            fun = mean) {
  return(aggregatePeriods(time_series,
    measure = measure,
    time = time,
    sample_frequency = sample_frequency,
    duration = duration,
    first_epoch_timestamp = first_epoch_timestamp,
    fun = fun
  ))
}

#' Aggregate Events
#'
#' @details Wrapper function that calls \code{aggregatePeriods} for events (duration of variable length).
#' @param time_series Data frame to be aggregated.
#' @param measure Name of the measure columns to be included.
#' @param time Name of the time column.
#' @param sample_frequency Measurement frequency of data.
#' @param events Data frame containing the start and end index of each event.
#' @param start_time Name of the column in events containing the start index of the events.
#' @param end_time Name of the column in events containing the end index of the events.
#' @param fun Function to apply on aggregation, defaults to mean.
#' @return Data frame of aggregated events.
#' @export
#' @examples
#' timestamp <- c(
#'   1619424004, 1619424005, 1619424006, 1619424007,
#'   1619424008, 1619424009, 1619424010, 1619424011,
#'   1619424012, 1619424013, 1619424014, 1619424015
#' )
#' value <- c(
#'   0.729614366, 1.729115871, 0.804973546, 2.510181118,
#'   2.23764038, 0.613203747, 0.681953275, 0.089566943,
#'   0.021042388, 2.4780338, 2.437488989, 2.632635727
#' )
#' data <- data.frame(timestamp, value)
#' event_start <- c(1, 5, 10)
#' event_end <- c(4, 9, 12)
#' aggregated_events <- aggregateEvents(data,
#'   events = data.frame(start = event_start, end = event_end),
#'   measure = "value",
#'   time = "timestamp",
#'   start_time = "start",
#'   end_time = "end",
#'   sample_frequency = 1,
#'   fun = sum
#' )
aggregateEvents <- function(time_series,
                            measure = "AGSA",
                            time = "timestamp",
                            sample_frequency,
                            events = NA,
                            start_time = "start",
                            end_time = "end",
                            fun = mean) {
  return(aggregatePeriods(time_series,
    measure = measure,
    time = time,
    sample_frequency = sample_frequency,
    events = events,
    start_time = start_time,
    end_time = end_time,
    fun = fun
  ))
}

#' Aggregate Periods
#'
#' @description Generalised aggregation function generates distinct epochs or events outputs based on the initial parameters provided.
#' @param time_series Data frame to be aggregated.
#' @param measure Name of the measure columns to be included.
#' @param time Name of the time column.
#' @param sample_frequency Frequency of data.
#' @param duration Time duration to aggregate in each epoch.
#' @param first_epoch_timestamp Time to start the first epoch, defaults to first record.
#' @param events Data frame containing the start and end index of each event.
#' @param start_time Name of the column in events containing the start index of the events.
#' @param end_time Name of the column in events containing the end index of the events.
#' @param fun Function to apply on aggregation, defaults to mean.
#' @return Data frame of aggregated epochs or events.
#' @export
#' @importFrom stats aggregate
aggregatePeriods <- function(time_series,
                             measure = "AGSA",
                             time = "timestamp",
                             sample_frequency,
                             duration = NA,
                             first_epoch_timestamp = NA,
                             events = NA,
                             start_time = "start",
                             end_time = "end",
                             fun = mean) {
  # Determine whether we are processing epochs or events
  EPOCH <- "EPOCH"
  EVENT <- "EVENT"
  mode <- ifelse(is.na(duration), EVENT, EPOCH)
  if (sample_frequency == 0) stop("aggregateEpochs: Sample frequency not defined")

  if (mode == EPOCH) {
    # Epoch specific processing
    if (is.na(duration)) stop("Duration must be defined for epoch aggregation")
    if (is.na(first_epoch_timestamp)) first_epoch_timestamp <- time_series[1, time] # Use first data point as start time if not defined

    measurements_per_epoch <- duration * sample_frequency
    # epochs_times <- seq(from = first_epoch_timestamp, by = measurements_per_epoch, to = max(time_series[, time]))
    start_index <- match(first_epoch_timestamp, time_series[, time], nomatch = NA)
    if (is.na(start_index)) stop("aggregateEpochs: Start time not found")

    max_epoch_number <- (nrow(time_series) - start_index + 1) / measurements_per_epoch
    period_number <- c(
      rep(0, max(0, start_index - 1)),
      floor((1 / measurements_per_epoch) * c(0:(nrow(time_series) - start_index))) + 1
    )
    epoch_durations <- rle(period_number)[["lengths"]] / sample_frequency
  } else {
    # Event specific processing
    if (exists("events") && is.data.frame(get("events"))) {
      period_number <- createEventMapping(events, start_time, end_time, nrow(time_series))
      if (is.na(match(0, period_number))) {
        epoch_durations <- c((events[, end_time] - events[, start_time] + 1) / sample_frequency)
      } else {
        epoch_durations <- c(0, (events[, end_time] - events[, start_time] + 1) / sample_frequency) #  Include 0 for period 0
      }
    } else {
      stop("Events must be defined for event aggregation")
    }
  }

  result <- aggregate(time_series[, measure], by = list(period_number), FUN = fun)

  # Measure can be multiple columns
  if (is.matrix(result[, 2])) {
    # Multiple functions were applied on multiple columns
    function_output_names <- colnames(result[, 2])
    output_names <- apply(expand.grid(function_output_names, measure), 1, function(x) paste0(x[2], ".", x[1]))
    # Expand each of the "matrix""array" into a single data frame
    temp <- data.frame(result[, 1])
    for (i in 1:length(measure)) {
      temp <- data.frame(temp, as.data.frame(result[, i + 1]))
    }
    colnames(temp) <- c("period_number", output_names)
    result <- temp
    rm(temp)
  } else {
    # Single function applied to aggregation
    colnames(result) <- c("period_number", measure)
  }

  # Identify the start of each epoch to later get the timestamp for the epoch
  epoch_start_index <- match(sort(unique(period_number)), period_number)

  df <- data.frame(
    time = time_series[epoch_start_index, time],
    result,
    duration = round(epoch_durations, digits = 1)
  )

  # Remove columns that contain 'max' except 'light.max' and remove 'light.sd'
  df <- df[, !(grepl("max", names(df)) & names(df) != "light.max") &
    !names(df) %in% "light.sd"]

  df <- df[df$period != 0, ] # drop event 0 as it represents "inter-event" times

  colnames(df)[1:2] <- c(time, ifelse(mode == EPOCH, "epoch_number", "event_number"))

  return(df)
}


#' Create Event Mapping
#'
#' @details Enumerate a vector to identify which event each measurement belongs to.
#' @param events Data frame containing the start and end index of each event.
#' @param start_time Name of the column in events containing the start index of the events.
#' @param end_time Name of the column in events containing the end index of the events.
#' @param max_row_number Number of rows in the source vector the events describe
#' @return List of mapped events.
#' @export
#' @examples
#' events <- data.frame(
#'   "start" = c(1, 5, 10, 15),
#'   "end" = c(4, 9, 14, 19)
#' )
#' time_series <- rnorm(25)
#' period_number <- createEventMapping(events, "start", "end", length(time_series))
createEventMapping <- function(events, start_time, end_time, max_row_number) {
  if (nrow(events) > 1) {
    events <- cbind(
      event_number = cbind(seq_len(nrow(events))),
      events,
      previous_end = c(0, events[1:(nrow(events) - 1), "end"])
    )
    event_mapping <- apply(events, 1,
      FUN = function(x) {
        c(
          rep(0, (x[start_time] - x["previous_end"] - 1)),
          rep(x["event_number"], (x[end_time] - x[start_time] + 1))
        )
      }
    )
  } else {
    events <- cbind(event_number = 1, events, previous_end = 0)
    event_mapping <- c(
      rep(0, events[start_time] - 1),
      rep(1, (events[end_time] - events[start_time] + 1))
    )
  }
  event_mapping <- unlist(event_mapping)
  # Returned mapping will have names, so remove them to enable testing
  event_mapping <- unname(event_mapping)
  # Map any occurrences beyond last event to 0
  if (length(event_mapping) < max_row_number) {
    event_mapping <- c(event_mapping, rep(0, max_row_number - length(event_mapping)))
  }
  return(event_mapping)
}
