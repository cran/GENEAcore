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
#   "x.mean" = c(0.1111, 0.1222, 0.1333, 0.1444),
#   "y.mean" = c(0.2111, 0.2222, 0.2333, 0.2444),
#   "light.mean" = c(1.25, 1.73, 1.99, 2.02)
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
#' @param cut_time_24hr Time in 24h to split the days up by.
#' @param MPI Measurement Period Information.
#' @keywords internal
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
