#' Reorder Data Frame
#'
#' @details Internal function to remove the epoch_number or event_number
#' columns and move duration column forward.
#' @param df Epochs or events data frame.
#' @export
#' @keywords internal
#' @return Reordered data frame.
# epochs_df <- data.frame(
#   time = c(1731421525, 1731421526, 1731421527),
#   epoch_number = c(1, 2, 3),
#   x = c(0.001, 0.001, 0.001),
#   y = c(0.125, 0.123, 0.121),
#   z = c(0.008, 0.009, 0.008),
#   duration = c(1, 1, 1)
# )
# epochs_df <- reorder_df(epochs_df)
reorder_df <- function(df) {
  df <- df[, !names(df) %in% c("epoch_number", "event_number")]
  cols <- names(df)
  duration_index <- which(cols == "duration")
  new_order <- c(1, duration_index, setdiff(2:length(cols), duration_index))
  df <- df[, new_order]
  return(df)
}


#' Check Time Format
#'
#' @details Internal function to parse and check validity of time string
#' passed as parameter.
#' @param time_str Time string.
#' @returns Valid time string or error.
#' @export
#' @keywords internal
#' @examples
#' CutTime24Hr <- "15:00"
#' cut_time <- check_time_format(CutTime24Hr)
check_time_format <- function(time_str) {
  # Check if the input matches the "HH:MM" format
  if (grepl("^([01]?[0-9]|2[0-3]):[0-5][0-9]$", time_str)) {
    # Convert the time
    # time_str <- strptime(time_str, format = "%H:%M")$hour
    time_str <- time_str
  } else {
    warning("Invalid time format. Please use 'HH:MM'. CutTime24Hr defaulted to '15:00'.")
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
#' @export
#' @keywords internal
#' @examples
#' epochs_df <- data.frame(
#'   "x.mean" = c(0.1111, 0.1222, 0.1333, 0.1444),
#'   "y.mean" = c(0.2111, 0.2222, 0.2333, 0.2444),
#'   "light.mean" = c(1.25, 1.73, 1.99, 2.02)
#' )
#' epochs_df <- round_columns(epochs_df)
round_columns <- function(df) {
  for (col in 1:ncol(df)) {
    dp <- get_decimal_places(df[col])
    df[col] <- round(df[col], dp)
  }
  return(df)
}

#' Get Decimal Places
#'
#' @details Function to determine the number of decimal places based on column name.
#' @param column Aggregated data frame column.
#' @returns Decimal place integer.
#' @export
#' @examples
#' epochs_df <- data.frame(
#'   "x.mean" = c(0.1111, 0.1222, 0.1333, 0.1444),
#'   "y.mean" = c(0.2111, 0.2222, 0.2333, 0.2444),
#'   "light.mean" = c(1.25, 1.73, 1.99, 2.02)
#' )
#' dp <- get_decimal_places(epochs_df[1])
get_decimal_places <- function(column) {
  if (grepl("light", colnames(column))) {
    return(0)
  } else if (grepl("degrees.mean|temp.mean|updown.mean", colnames(column))) {
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
