#' Reorder Data Frame
#'
#' @details Internal function to remove the epoch_number or event_number
#' columns and move duration column forward.
#' @param df Epochs or events data frame.
#' @keywords internal
#' @return Reordered data frame.
#   epochs_df <- data.frame(
#   time = c(1731421525, 1731421526, 1731421527),
#   epoch_number = c(1, 2, 3),
#   x = c(0.001, 0.001, 0.001),
#   y = c(0.125, 0.123, 0.121),
#   z = c(0.008, 0.009, 0.008),
#   duration = c(1, 1, 1)
# )
# epochs_df <- reorder_df(epochs_df)
reorder_df <- function(df) {
  df <- df[, !names(df) %in% c("EpochNumber", "EventNumber")]
  df$DateTimeUTC <- format(as.POSIXct(df$TimeUTC, tz = "GMT"), "%Y-%m-%d %H:%M:%S")
  cols <- names(df)
  datetime_index <- which(cols == "DateTimeUTC")
  duration_index <- which(cols == "Duration")
  new_order <- c(1, datetime_index, duration_index, setdiff(2:length(cols), c(datetime_index, duration_index)))
  df <- df[, new_order]
  return(df)
}


#' Check Time Format
#'
#' @details Internal function to parse and check validity of time string
#' passed as parameter.
#' @param time_str Time string.
#' @returns Valid time string or error.
#' @keywords internal
# cut_time_24hr <- "15:00"
# cut_time <- check_time_format(cut_time_24hr)
check_time_format <- function(time_str) {
  # Check if the input matches the "HH:MM" format
  if (grepl("^([01]?[0-9]|2[0-3]):[0-5][0-9]$", time_str)) {
    # Convert the time
    # time_str <- strptime(time_str, format = "%H:%M")$hour
    time_str <- time_str
  } else {
    warning("Invalid time format. Please use 'HH:MM'. cut_time_24hr defaulted to '15:00'.")
    time_str <- "15:00"
  }
  return(time_str)
}

#' Round Columns
#'
#' @details Internal function to round columns based on column names or
#' variability of data in column.
#' @param df Aggregated data frame.
#' @returns Epochs or events data frame with rounded columns.
#' @keywords internal
# epochs_df <- data.frame(
#   "x.mean" = c(0.1111, 0.1222, 0.1333, 0.1444),
#   "y.mean" = c(0.2111, 0.2222, 0.2333, 0.2444),
#   "light.mean" = c(1.25, 1.73, 1.99, 2.02)
# )
# epochs_df <- round_columns(epochs_df)
round_columns <- function(df) {
  for (col in 1:ncol(df)) {
    if (is.numeric(df[[col]])) {
      dp <- get_decimal_places(df[col])
      df[col] <- round(df[col], dp)
    }
  }
  return(df)
}

#' Get Decimal Places
#'
#' @details Internal function to determine the number of decimal places based on column name.
#' @param column Aggregated data frame column.
#' @returns Decimal place integer.
#' @keywords internal
# epochs_df <- data.frame(
#   "xMean" = c(0.1111, 0.1222, 0.1333, 0.1444),
#   "yMean" = c(0.2111, 0.2222, 0.2333, 0.2444),
#   "LightMean" = c(1.25, 1.73, 1.99, 2.02)
# )
# dp <- get_decimal_places(epochs_df[1])
get_decimal_places <- function(column) {
  if (grepl("Light", colnames(column))) {
    return(0)
  } else if (grepl("DegreesMean|TempMean|UpDownMean", colnames(column))) {
    return(1)
  } else {
    # Determine decimal places based on variability
    variability <- sd(as.numeric(unlist(column)), na.rm = TRUE)
    if (is.na(variability)) {
      return(5)
    } else if (variability < 0.001) {
      return(5)
    } else if (variability < 0.1) {
      return(4)
    } else {
      return(3)
    }
  }
}

#' Get Cut Times
#'
#' @details Internal function to determine the daily cut times based on 24Hr Cut Time specified.
#' @param cut_time_24hr Time in 24h to split days up by.
#' @param MPI Measurement Period Information.
#' @keywords internal
#' @export
#' @returns List of cut times in Unix UTC.
get_cut_times <- function(cut_time_24hr, MPI) {
  cut_time_24hr <- check_time_format(cut_time_24hr)
  cut_time_24hr_offset <- 60 * (60 * as.numeric(unlist(strsplit(cut_time_24hr, ":"))[1]) + as.numeric(unlist(strsplit(cut_time_24hr, ":"))[2]))
  if ((MPI$file_data[["MeasurementStartTimeUTC"]] - MPI$file_data[["FirstLocalMidnightTimeUTC"]]) > cut_time_24hr_offset) {
    local_first_cut_time <- MPI$file_data[["FirstLocalMidnightTimeUTC"]] + cut_time_24hr_offset
  } else {
    local_first_cut_time <- MPI$file_data[["FirstLocalMidnightTimeUTC"]] - (24 * 60 * 60 - cut_time_24hr_offset)
  }

  if (local_first_cut_time < MPI$file_data[["MeasurementEndTimeUTC"]]) {
    cut_times <- seq(local_first_cut_time, MPI$file_data[["MeasurementEndTimeUTC"]], 24 * 60 * 60)
    cut_times <- c(MPI$file_data[["MeasurementStartTimeUTC"]], cut_times, MPI$file_data[["MeasurementEndTimeUTC"]])
  } else {
    cut_times <- c(MPI$file_data[["MeasurementStartTimeUTC"]], MPI$file_data[["MeasurementEndTimeUTC"]])
  }
  cut_times <- sort(unique(cut_times))
  cut_times <- cut_times[cut_times >= MPI$file_data[["MeasurementStartTimeUTC"]]]
  cut_times <- cut_times[c(TRUE, diff(cut_times) > 5)]

  return(cut_times)
}


#' Write MPI to JSON
#'
#' @details Restructures the MPI and writes it out to a JSON for ActivInsights internal use.
#' @param MPI Measurement Period Information.
#' @param mpi_path File path to MPI RDS.
#' @keywords internal
#' @returns MPI JSON file written to the same directory as the MPI RDS.
MPI_toJSON <- function(MPI, mpi_path) {
  MPI[["non_movement"]][["sphere_points"]] <- NULL # remove sphere points
  if (length(MPI$errors) > 0) MPI$errors <- unlist(MPI$errors)
  MPI$file_history <- unlist(MPI$file_history)
  MPI$file_info <- as.list(MPI$file_info)
  MPI$file_data <- as.list(MPI$file_data)
  if (length(MPI$button) > 0) MPI$button <- as.array(MPI$button)

  MPI$file_data$MeasurementDeviceCalibrationDate <- format(as.Date(MPI$file_data$MeasurementDeviceCalibrationDate, format = "%d-%b-%Y", "%Y-%m-%d"))
  MPI$file_data$MeasurementStartDate <- format(as.Date(MPI$file_data$MeasurementStartDate, format = "%d-%b-%Y", "%Y-%m-%d"))
  MPI$file_data$VoltsEnd <- as.numeric(MPI$file_data$VoltsEnd)
  MPI$file_data$VoltsStart <- as.numeric(MPI$file_data$VoltsStart)
  if (is.na(MPI$file_data$ClockDrift)) MPI$file_data$ClockDrift <- 0
  if (is.na(MPI$file_data$ClockDriftDay)) MPI$file_data$ClockDriftDay <- 0

  mpi_json <- jsonlite::toJSON(MPI, auto_unbox = TRUE, pretty = TRUE)
  writeLines(mpi_json, sub(".rds", ".json", mpi_path))
}

#' Calculate Expected Last Timestamp
#'
#' @details Calculates the expected last timestamp of a file based on measurement frequency,
#' page count and first timestamp of file.
#' @param measurement_frequency Measurement frequency of recorded data in the bin file.
#' @param page_count Total number of pages in the bin file.
#' @param first_timestamp Unix timestamp of the first page of recorded data in the bin file.
#' @keywords internal
#' @export
#' @returns Expected last timestamp
expected_last_timestamp <- function(measurement_frequency, page_count, first_timestamp) {
  page_duration <- 300 / measurement_frequency
  last_page_number <- page_count - 1
  expected_duration <- last_page_number * page_duration
  expected_lastpagetime <- first_timestamp + expected_duration

  return(expected_lastpagetime)
}


#' Determine Valid Days
#'
#' @details Determine which days of data are valid days to process based on non-wear
#' calculated in MPI and a minimum valid day duration.
#' @param MPI_nonwear MPI non-wear data frame with start times and durations of non-wear events.
#' @param date_range Data frame of day border timestamps.
#' @param required_processing_hours Hours of wear time in a 24-hour day to be considered a valid day to be processed.
#' @param cut_time_24hr Time in 24h to split days up by.
#' @keywords internal
#' @export
#' @returns List of 2 data objects:
#' \itemize{
#'  \item valid_day provides information on the wear and non-wear duration for each day and its Boolean validity based on `minimum_valid_day`.
#'  \item nonwear_day provides the start and end time of each non-wear period and the day in which it occurs.
#' }
#'
valid_day_nonwear <- function(MPI_nonwear,
                              date_range,
                              required_processing_hours = 0,
                              cut_time_24hr = "15:00") {
  cut_time_24hr <- check_time_format(cut_time_24hr)
  cut_time_24hr_offset <- 60 * (60 * as.numeric(unlist(strsplit(cut_time_24hr, ":"))[1]) + as.numeric(unlist(strsplit(cut_time_24hr, ":"))[2]))

  MPI_nonwear$end_time <- MPI_nonwear$start_time + MPI_nonwear$duration

  # shift start of day
  day_zero <- date_range$start[1] + cut_time_24hr_offset
  day_length <- date_range$length[1]

  # compute which day each interval touches
  MPI_nonwear$day_start_idx <- floor((MPI_nonwear$start_time - day_zero) / day_length) + 1
  MPI_nonwear$day_end_idx <- floor((MPI_nonwear$end_time - day_zero) / day_length) + 1

  # clamp to valid days
  MPI_nonwear$day_start_idx <- pmax(MPI_nonwear$day_start_idx, 1)
  MPI_nonwear$day_end_idx <- pmin(MPI_nonwear$day_end_idx, nrow(date_range))

  # expand intervals across days
  idx <- Map(seq, MPI_nonwear$day_start_idx, MPI_nonwear$day_end_idx)

  all_days <- seq_len(nrow(date_range))
  all_intervals <- seq_len(nrow(MPI_nonwear))

  expanded <- expand.grid(
    interval = all_intervals,
    day = all_days
  )

  i <- expanded$interval
  d <- expanded$day

  # calculate overlap per day
  expanded$nonwear_start <- pmax(
    MPI_nonwear$start_time[i],
    date_range$start[d]
  )

  expanded$nonwear_end <- pmin(
    MPI_nonwear$end_time[i],
    date_range$end[d]
  )

  expanded$MPI_nonwear_duration <- pmax(
    0,
    expanded$nonwear_end - expanded$nonwear_start
  )

  if (nrow(MPI_nonwear) > 0) {
    # sum durations per day
    MPI_nonwear_by_day <- aggregate(
      MPI_nonwear_duration ~ day,
      data = expanded,
      sum
    )

    MPI_nonwear_by_day <- merge(
      data.frame(day = all_days),
      MPI_nonwear_by_day,
      by = "day",
      all.x = TRUE
    )

    MPI_nonwear_by_day$MPI_nonwear_duration[is.na(MPI_nonwear_by_day$MPI_nonwear_duration)] <- 0
  } else {
    MPI_nonwear_by_day <- data.frame(
      day = all_days,
      MPI_nonwear_duration = rep(0, length(all_days))
    )
  }

  # calculate wear duration and valid day in both cases
  MPI_nonwear_by_day$wear_duration <- date_range$length - MPI_nonwear_by_day$MPI_nonwear_duration
  MPI_nonwear_by_day$valid_day <- MPI_nonwear_by_day$wear_duration >= required_processing_hours * 3600

  return(list(
    valid_day = MPI_nonwear_by_day,
    nonwear_day = expanded
  ))
}

#' @importFrom utils modifyList
merge_control <- function(defaults, control) {
  if (is.null(control)) control <- list()
  if (!is.list(control)) stop("control must be a list")
  control <- control[names(control) %in% names(defaults)]
  modifyList(defaults, control)
}

#' Get events from transitions for aggregation and step counter
#' @param time_series Data frame that transitions are for
#' @param transitions Returned output of detect_transitions().
#' @param sample_frequency Measurement frequency of data.
#' @export
#' @keywords internal
get_events <- function(time_series, transitions, sample_frequency) {
  if (!all(is.na(transitions))) {
    rle_cal_lengths <- rle(time_series$TimeUTC)$lengths
    cum_lengths <- cumsum(rle_cal_lengths)
    events <- data.frame(
      "start" = transitions$index[-nrow(transitions)],
      "end" = cum_lengths[transitions$index - 1]
    )
    if (nrow(transitions) == 1) {
      events <- data.frame("start" = transitions, "end" = nrow(time_series))
    }
    if (nrow(events) > 1) {
      events$start[2:nrow(events)] <- events$end[-nrow(events)] + 1
    }
    if ((nrow(time_series) - events$end[nrow(events)]) > sample_frequency &
      (nrow(time_series) - events$end[nrow(events)]) / sample_frequency >= 5) {
      events <- rbind(events, c(events$end[nrow(events)] + 1, nrow(time_series)))
    } else {
      events$end[nrow(events)] <- nrow(time_series)
    }

    return(events)
  } else {
    warning(
      "No transitions to calculate events from."
    )
    return(invisible(NULL))
  }
}

#' Map Study Management Hub Identifiers
#' @param x Data frame with ID columns to map.
#' @param cols_to_map Vector of column names to map ID to identifier.
#' @param identifier_mapping_record File path of the exported identifier_mapping_record CSV file.
#' @export
#' @keywords internal
identifier_mapping <- function(x, cols_to_map, identifier_mapping_record) {

  expected_object_types <- c(
    SiteID = "Site",
    StudyID = "Study",
    PeriodInfo = "Visit",
    ParticipantID = "Participant"
  )

  # Keep only supported mapping columns
  unsupported_cols <- setdiff(cols_to_map, names(expected_object_types))
  if (length(unsupported_cols) > 0) {
    warning(
      sprintf(
        "Ignoring unsupported names in cols_to_map: %s",
        paste(unsupported_cols, collapse = ", ")
      )
    )
  }

  valid_cols <- intersect(cols_to_map, names(expected_object_types))

  if (length(valid_cols) == 0) {
    warning("No supported columns in cols_to_map")
    return(x)
  }

  # Keep only columns that actually exist in x
  missing_in_x <- setdiff(valid_cols, names(x))
  if (length(missing_in_x) > 0) {
    warning(
      sprintf(
        "Columns are not present in x and will be skipped: %s",
        paste(missing_in_x, collapse = ", ")
      )
    )
  }

  valid_cols <- intersect(valid_cols, names(x))

  if (length(valid_cols) == 0) {
    warning("No valid mapping columns exist in x")
    return(x)
  }

  # Find header row in first few lines
  first_lines <- readLines(identifier_mapping_record, n = 10, warn = FALSE)

  header_row <- which(
    grepl("Object Type", first_lines, fixed = TRUE) &
      grepl("Object Id", first_lines, fixed = TRUE) &
      grepl("Object Identifier", first_lines, fixed = TRUE)
  )[1]

  if (is.na(header_row)) {
    stop(
      sprintf(
        "Could not find header row containing 'Object Type', 'Object Id', and 'Object Identifier' within the first %d lines",
        10
      )
    )
  }

  identifier_map <- read.csv(
    identifier_mapping_record,
    skip = header_row - 1,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  names(identifier_map) <- gsub("\\s+", "", names(identifier_map))
  required_cols <- c("ObjectType", "ObjectId", "ObjectIdentifier")
  missing_map_cols <- setdiff(required_cols, names(identifier_map))

  if (length(missing_map_cols) > 0) {
    stop(
      sprintf(
        "Mapping file is missing required columns: %s",
        paste(missing_map_cols, collapse = ", ")
      )
    )
  }

  for (col_name in valid_cols) {
    object_type_needed <- expected_object_types[[col_name]]

    this_map <- identifier_map[
      identifier_map[["ObjectType"]] == object_type_needed,
      ,
      drop = FALSE
    ]

    if (nrow(this_map) == 0) {
      warning(
        sprintf(
          "No mapping rows found for column '%s' with Object Type '%s'",
          col_name,
          object_type_needed
        )
      )
      next
    }

    idx <- match(x[[col_name]], this_map[["ObjectId"]])
    replacement_values <- this_map[["ObjectIdentifier"]][idx]

    x[[col_name]] <- ifelse(!is.na(idx), replacement_values, x[[col_name]])
  }

  return(x)
}
