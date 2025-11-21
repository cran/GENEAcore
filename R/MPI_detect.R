#' Detect Non-movement
#'
#' @details Function to detect non-movement bouts, non-wear events and points in a 1Hz
#' downsampled bin file.
#' @param binfile Text lines read from an open connection to a bin file.
#' @param binfile_path Path to the bin file to be processed.
#' @param output_folder Path to the folder containing GENEAcore run outputs and Measurement Period Information (MPI) files.
#' @param still_seconds The number of seconds included in the rolling standard
#' deviation calculation for stillness to determine the shortest detection duration.
#' @param sd_threshold The threshold applied to the rolling standard deviation
#' of combined acceleration to determine stillness.
#' @param temp_seconds The number of seconds included in the rolling temperature
#' difference calculation or non-wear which also determines the shortest detection duration.
#' @param border_seconds The minimum number of seconds of a still event to be classed as a new bout.
#' @param long_still_seconds The minimum number of seconds of a still bout that is classed as non-wear.
#' @param delta_temp_threshold The threshold applied to the rolling temperature difference to determine non-wear.
#' @param posture_changes_max The maximum number of adjoining events that make up a single bout.
#' @param non_move_duration_max The maximum number of seconds of a still bout to be classed as non-movement.
#' Still bouts with a duration longer than this number is automatically classed as non-wear.
#' @return List of sphere points, non-movement bouts and non-wear events.
#' @export
#' @importFrom stats na.omit
#' @examples
#' binfile_path <- system.file("extdata/20Hz_file.bin", package = "GENEAcore")
#' con <- file(binfile_path, "r")
#' binfile <- readLines(con, skipNul = TRUE)
#' close(con)
#' output_folder <- tempdir()
#' MPI <- create_MPI(binfile, binfile_path, output_folder)
#' MPI <- detect_nonmovement(binfile, binfile_path, output_folder)
detect_nonmovement <- function(binfile,
                               binfile_path,
                               output_folder,
                               still_seconds = 120,
                               sd_threshold = 0.013,
                               temp_seconds = 240,
                               border_seconds = 300,
                               long_still_seconds = 120 * 60,
                               delta_temp_threshold = -0.7,
                               posture_changes_max = 2,
                               non_move_duration_max = 12 * 60 * 60) {
  # Get UniqueBinFileIdentifier
  UniqueBinFileIdentifier <- get_UniqueBinFileIdentifier(binfile)

  if (!is.na(UniqueBinFileIdentifier)) {
    # Check if downsampled data file already exists and create if not
    file_pattern <- paste0(UniqueBinFileIdentifier, "_downsample.rds")
    files <- list.files(path = output_folder, pattern = file_pattern, recursive = TRUE)
    if (length(files) != 0) {
      measurements <- readRDS(file.path(output_folder, files[1]))
    } else {
      measurements <- sample_binfile(binfile, binfile_path, output_folder)
    }
    MPI_filepath <- file.path(output_folder, paste0(UniqueBinFileIdentifier, "_MPI.rds"))
    MPI <- readRDS(MPI_filepath)

    # Condition check for non-wear
    temp_seconds <- floor(temp_seconds / 2) * 2 ## need to make sure this is an even number
    c_diff_temp <- c(0, cumsum(diff(measurements$Temp)))

    if ((nrow(measurements) < (still_seconds + 1)) || (length(c_diff_temp) < temp_seconds)) {
      warning(paste(basename(binfile_path), ": Insufficient data to detect non-movement,", nrow(measurements), "data points"))
      # set empty lists, update MPI and add note to file history
      MPI$file_history <- rbind(MPI$file_history, paste0(substr(Sys.time(), 0, 23), " non-movement attempted and failed"))
      MPI$non_movement$sphere_points <- data.frame()
      MPI$non_movement$still_bouts <- data.frame()
      MPI$non_movement$non_wear <- data.frame()

      saveRDS(MPI, MPI_filepath)
      return(MPI)
    }

    # Divide still_seconds by 2 as it is applied twice and make sure it is even number
    still_seconds <- floor(still_seconds / 4) * 2

    # Rolling mean of x, y & z
    xyz <- subset(measurements, select = c(x, y, z))
    c_xyz <- cumsum(xyz)
    mean_xyz <- (c_xyz[(still_seconds + 1):nrow(c_xyz), ] - c_xyz[1:(nrow(c_xyz) - still_seconds), ]) / still_seconds

    # Rolling sd of x, y & z
    res_xyz <- (xyz[(still_seconds / 2 + 1):(nrow(c_xyz) - still_seconds / 2), ] - mean_xyz)^2
    c_res_xyz <- cumsum(res_xyz)
    sd_xyz <- ((c_res_xyz[(still_seconds + 1):nrow(c_res_xyz), ] - c_res_xyz[1:(nrow(c_res_xyz) - still_seconds), ]) / still_seconds)^0.5

    # Store mean sd in measurements
    measurements$sd_acc <- NA
    measurements$sd_acc[(still_seconds + 1):(nrow(measurements) - still_seconds)] <- rowMeans(sd_xyz)

    # still points on sphere for calibration
    sphere_points <- na.omit(measurements[measurements$sd_acc < sd_threshold, ])
    sphere_points <- subset(sphere_points, select = c(x, y, z, Temp))
    # remove 0 rows to handle division by zero error in calc_autocalparams
    sphere_points <- subset(sphere_points, !(x == 0 & y == 0 & z == 0))
    colnames(sphere_points) <- c("x", "y", "z", "temp")

    mean_diff_temp <- (c_diff_temp[(temp_seconds + 1):length(c_diff_temp)] - c_diff_temp[1:(length(c_diff_temp) - temp_seconds)])
    measurements$delta_temp <- NA
    measurements$delta_temp[(temp_seconds / 2 + 1):(nrow(measurements) - temp_seconds / 2)] <- mean_diff_temp

    # Set up still events for non-wear detection
    still_events <- na.omit(data.frame(unclass(rle(ifelse(measurements$sd_acc > sd_threshold, 1, 0)))))

    if (nrow(still_events) < 2) {
      still_bouts <- data.frame()
      non_wear <- data.frame()
    } else {
      names(still_events) <- c("duration", "state")

      # set start and end times
      still_events$end_time <- cumsum(still_events$duration) + na.omit(measurements)[1, 1]
      still_events$start_time <- still_events$end_time - still_events$duration

      # record the duration of the previous state
      still_events$prev_duration <- c(0, still_events$duration[1:(nrow(still_events) - 1)])

      # extract just the still events (no information lost)
      still_events <- still_events[still_events$state == 0, ]
      still_events <- subset(still_events, select = -c(state))

      ### loop to merge adjoining bouts

      bout_index <- 1

      # first bout
      start_time <- still_events$start_time[bout_index]
      end_time <- still_events$end_time[bout_index]
      duration <- still_events$duration[bout_index]
      number_events <- 1
      category <- NA

      if (nrow(still_events) > 2) {
        for (i in 2:(nrow(still_events))) {
          # adjoining event
          if (still_events$prev_duration[i] < border_seconds) {
            end_time[bout_index] <- still_events$end_time[i]
            duration[bout_index] <- duration[bout_index] + still_events$duration[i] + still_events$prev_duration[i]
            if (still_events$duration[i] > long_still_seconds) {
              category[bout_index] <- "non-wear"
            } else {
              category[bout_index] <- NA
            }
            number_events[bout_index] <- number_events[bout_index] + 1

            # new bout
          } else {
            bout_index <- bout_index + 1
            start_time[bout_index] <- still_events$start_time[i]
            end_time[bout_index] <- still_events$end_time[i]
            duration[bout_index] <- still_events$duration[i]
            if (still_events$duration[i] > long_still_seconds) {
              category[bout_index] <- "non-wear"
            } else {
              category[bout_index] <- NA
            }
            number_events[bout_index] <- 1
          }
        }
      }

      still_bouts <- data.frame(
        start_time,
        duration,
        number_events,
        category
      )

      # add back bout time lost to rolling filter
      still_bouts$start_time <- still_bouts$start_time - 2 * still_seconds
      still_bouts$duration <- still_bouts$duration + 2 * still_seconds

      # remove short bouts
      still_bouts <- still_bouts[still_bouts$duration > border_seconds, ]

      # add in temperature information
      still_bouts$delta_temp <- measurements$delta_temp[match(still_bouts$start_time + temp_seconds, measurements$time)]

      # apply non-wear rules
      still_bouts$category <- ifelse(((still_bouts$delta_temp <= delta_temp_threshold) & (still_bouts$number_events <= posture_changes_max)) |
        (still_bouts$duration > non_move_duration_max),
      "non-wear", still_bouts$category
      )

      # create output objects
      non_wear <- subset(still_bouts, still_bouts$category == "non-wear")
      non_wear <- subset(non_wear, select = -c(delta_temp, category, number_events))
      still_bouts <- subset(still_bouts, is.na(still_bouts$category))
      still_bouts <- subset(still_bouts, select = -c(delta_temp, category))
    }
    # update MPI and add note to file history
    MPI$file_history <- rbind(
      MPI$file_history,
      paste0(
        substr(Sys.time(), 0, 23),
        " non-movement detected and non-wear events added ",
        "(parameters: ",
        "still_seconds = ", still_seconds, ", ",
        "sd_threshold = ", sd_threshold, ", ",
        "temp_seconds = ", temp_seconds, ", ",
        "border_seconds = ", border_seconds, ", ",
        "long_still_seconds = ", long_still_seconds, ", ",
        "delta_temp_threshold = ", delta_temp_threshold, ", ",
        "posture_changes_max = ", posture_changes_max, ", ",
        "non_move_duration_max = ", non_move_duration_max, ")"
      )
    )
    MPI$non_movement$sphere_points <- sphere_points
    MPI$non_movement$still_bouts <- still_bouts
    MPI$non_movement$non_wear <- non_wear

    saveRDS(MPI, MPI_filepath)
    return(MPI)
  } else {
    warning(paste0(basename(binfile_path), ": Unable to generate UniqueBinFileIdentifier, no movement detection performed"))
    return(MPI)
  }
}


#' Detect Transitions
#'
#' @details Function to detect mean and variance changepoints in 1Hz acceleration data from a bin file.
#' @param binfile Text lines read from an open connection to a bin file.
#' @param binfile_path Path to the bin file to be processed.
#' @param output_folder Path to the folder containing GENEAcore run outputs and Measurement Period Information (MPI) files.
#' @param minimum_event_duration The minimum interval between changepoint transitions.
#' @param x_cpt_penalty The manual penalty value applied in the PELT changepoint algorithm for the x axis, see \code{\link[changepoint]{cpt.var}}.
#' @param y_cpt_penalty The manual penalty value applied in the PELT changepoint algorithm for the y axis, see \code{\link[changepoint]{cpt.var}}.
#' @param z_cpt_penalty The manual penalty value applied in the PELT changepoint algorithm for the z axis, see \code{\link[changepoint]{cpt.var}}.
#' @param cut_time_24hr Time in 24h to split the days up by.
#' @return List of time, index and day number of each transition within the measurement period information.
#' @importFrom utils timestamp
#' @export
#' @examples
#' binfile_path <- system.file("extdata/20Hz_file.bin", package = "GENEAcore")
#' con <- file(binfile_path, "r")
#' binfile <- readLines(con, skipNul = TRUE)
#' close(con)
#' output_folder <- tempdir()
#' MPI <- create_MPI(binfile, binfile_path, output_folder)
#' MPI <- detect_transitions(binfile, binfile_path, output_folder)
detect_transitions <- function(binfile,
                               binfile_path,
                               output_folder,
                               minimum_event_duration = 5,
                               x_cpt_penalty = 18,
                               y_cpt_penalty = 25,
                               z_cpt_penalty = 16,
                               cut_time_24hr = "15:00") {
  # Get UniqueBinFileIdentifier
  UniqueBinFileIdentifier <- get_UniqueBinFileIdentifier(binfile)

  cut_time_24hr <- check_time_format(cut_time_24hr)

  if (!is.na(UniqueBinFileIdentifier)) {
    # Check if downsampled data file already exists and create if not
    downsampled_filepath <- file.path(output_folder, paste0(UniqueBinFileIdentifier, "_downsample.rds"))
    if (file.exists(downsampled_filepath)) {
      measurements <- readRDS(downsampled_filepath)
    } else {
      measurements <- sample_binfile(binfile, binfile_path, output_folder)
    }
    MPI_filepath <- file.path(output_folder, paste0(UniqueBinFileIdentifier, "_MPI.rds"))
    MPI <- readRDS(MPI_filepath)

    if (("CutTime24Hr" %in% names(MPI$file_data)) && (MPI$file_data[["CutTime24Hr"]] == cut_time_24hr)) {
      return(MPI)
    } else {
      MPI$file_data[["CutTime24Hr"]] <- cut_time_24hr
      cut_times <- get_cut_times(cut_time_24hr, MPI)
      MPI$file_data[["NumberDays"]] <- length(cut_times) - 1

      cpts_var_all <- c()
      cpts_index <- c()
      days_all <- c()
      for (i in 1:MPI$file_data[["NumberDays"]]) {
        # print(paste("Day", i))
        day_measurements <- subset(measurements, TimeUTC >= cut_times[i] & TimeUTC < cut_times[i + 1])
        day_measurements <- na.omit(day_measurements)

        cpt_var_x <- changepoint::cpt.var(
          data = day_measurements$x,
          penalty = "Manual",
          pen.value = x_cpt_penalty,
          minseglen = minimum_event_duration,
          method = "PELT"
        )

        cpt_var_y <- changepoint::cpt.var(
          data = day_measurements$y,
          penalty = "Manual",
          pen.value = y_cpt_penalty,
          minseglen = minimum_event_duration,
          method = "PELT"
        )

        cpt_var_z <- changepoint::cpt.var(
          data = day_measurements$z,
          penalty = "Manual",
          pen.value = z_cpt_penalty,
          minseglen = minimum_event_duration,
          method = "PELT"
        )

        # Combine axes & days
        cpts_var_day <- c(
          cpt_var_x@cpts,
          cpt_var_y@cpts,
          cpt_var_z@cpts
        )

        # Sort in time order
        cpts_var_day <- sort(cpts_var_day)

        # Remove direct duplicates
        cpts_var_day <- unique(cpts_var_day)

        # Remove timestamps closer than minimum_event_duration
        diffs <- c(0, diff(c(0, cpts_var_day)))
        cpts_var_day <- data.frame(values = diffs)
        cpts_var_day$time <- cumsum(cpts_var_day$values)
        cpts_var_day <- cpts_var_day[cpts_var_day$values >= minimum_event_duration, ]
        cpts_var_day <- cpts_var_day$time

        # Finalise output
        cpts_var_all <- c(cpts_var_all, day_measurements$TimeUTC[cpts_var_day])
        cpts_index <- c(cpts_index, cpts_var_day)
        days_all <- c(days_all, rep(i, length(cpts_var_day)))
      }

      cut_times_df <- data.frame(
        time = cut_times,
        index = c(rep(1, MPI$file_data[["NumberDays"]]), nrow(day_measurements) + 1),
        day = c(1:MPI$file_data[["NumberDays"]], MPI$file_data[["NumberDays"]])
      )

      cpts_var_df <- data.frame(time = cpts_var_all, index = cpts_index, day = days_all)
      cpts_var_all_df <- remove_short_transitions(cpts_var_df, cut_times_df, minimum_event_duration)
      colnames(cpts_var_all_df) <- c("time_UTC", "index", "day_number")

      # Update MPI and add note to file history
      MPI$file_history <- rbind(
        MPI$file_history,
        paste0(
          substr(Sys.time(), 0, 23),
          " event transitions added ",
          "(parameters: ",
          "minimum_event_duration = ", minimum_event_duration, ", ",
          "x_cpt_penalty = ", x_cpt_penalty, ", ",
          "y_cpt_penalty = ", y_cpt_penalty, ", ",
          "z_cpt_penalty = ", z_cpt_penalty, ", ",
          "cut_time_24hr = ", cut_time_24hr, ")"
        )
      )
      MPI$transitions <- cpts_var_all_df
      saveRDS(MPI, MPI_filepath)

      return(MPI)
    }
  } else {
    warning(paste(basename(binfile_path), ": UniqueBinFileIdentifier cannot be created, no transitions calculated"))
    MPI <- NA
    return(MPI)
  }
}


#' Remove Short Transitions
#'
#' @details Identify and remove transitions shorter than the minimum event duration.
#' @param cpts_var_df Calculated changepoints data frame.
#' @param cut_times Start of day transitions data frame.
#' @param minimum_event_duration The minimum interval between changepoint transitions.
#' @return Data frame of all transitions including start of day with short transitions removed.
#' @export
#' @examples
#' changepoints <- data.frame(
#'   time = c(1677855218, 1677855598, 1677855661, 1677855679),
#'   index = c(86019, 86399, 62, 80),
#'   day = c(1, 1, 2, 2)
#' )
#' cut_times <- data.frame(time = 1677855600, index = 1, day = 2)
#' transitions <- remove_short_transitions(changepoints, cut_times, 5)
remove_short_transitions <- function(cpts_var_df, cut_times, minimum_event_duration) {
  distance <- abs(outer(cpts_var_df$time, cut_times$time, FUN = "-"))
  distance_min <- apply(distance, 1, FUN = min)
  cpts_var_df <- cpts_var_df[!(distance_min >= 1 & distance_min <= (minimum_event_duration - 1)), ]

  cpts_var_all_df <- rbind(cpts_var_df, cut_times)
  cpts_var_all_df <- cpts_var_all_df[order(cpts_var_all_df$time), ]
  cpts_var_all_df <- cpts_var_all_df[!duplicated(cpts_var_all_df[, c("index", "day")]), ]
  rownames(cpts_var_all_df) <- NULL
  return(cpts_var_all_df)
}
