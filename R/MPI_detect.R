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
#' binfile_path <- system.file("inst/extdata/20Hz_file.bin", package = "GENEAcore")
#' con <- file(binfile_path, "r")
#' binfile <- readLines(con, skipNul = TRUE)
#' close(con)
#' output_folder <- "."
#' MPI <- create_MPI(binfile, binfile_path, output_folder)
#' MPI <- detect_nonmovement(binfile, binfile_path, output_folder)
detect_nonmovement <- function(binfile,
                               binfile_path,
                               output_folder,
                               still_seconds = 120,
                               sd_threshold = 0.011,
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
    c_diff_temp <- c(0, cumsum(diff(measurements$temp)))

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
    sphere_points <- subset(sphere_points, select = c(x, y, z, temp))
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
      num_events <- 1
      category <- NA

      if (nrow(still_events) > 2) {
        for (i in 2:(nrow(still_events))) {
          # adjoining event
          if (still_events$prev_duration[i] < border_seconds) {
            end_time[bout_index] <- still_events$end_time[i]
            duration[bout_index] <- duration[bout_index] + still_events$duration[i]
            if (still_events$duration[i] > long_still_seconds) {
              category[bout_index] <- "non-wear"
            } else {
              category[bout_index] <- NA
            }
            num_events[bout_index] <- num_events[bout_index] + 1

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
            num_events[bout_index] <- 1
          }
        }
      } else {
        warning(paste(basename(binfile_path), ": Only 1 still bout detected"))
      }

      still_bouts <- data.frame(
        start_time,
        end_time,
        duration,
        num_events,
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
      still_bouts$category <- ifelse(((still_bouts$delta_temp <= delta_temp_threshold) & (still_bouts$num_events <= posture_changes_max)) |
                                       (still_bouts$duration > non_move_duration_max),
                                     "non-wear", still_bouts$category
      )

      # create output objects
      non_wear <- subset(still_bouts, still_bouts$category == "non-wear")
      non_wear <- subset(non_wear, select = -c(delta_temp, category, num_events))
      if (nrow(non_wear) > 0) {
        non_wear$certainty <- 0.9
      }
      still_bouts <- subset(still_bouts, is.na(still_bouts$category))
      still_bouts <- subset(still_bouts, select = -c(delta_temp, category))
    }
    # update MPI and add note to file history
    MPI$file_history <- rbind(MPI$file_history, paste0(substr(Sys.time(), 0, 23), " non-movement detected"))
    MPI$file_history <- rbind(MPI$file_history, paste0(substr(Sys.time(), 0, 23), " non-wear events added"))
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
#' @param CutTime24Hr Time in 24h to split the days up by.
#' @return List of time, index and day number of each transition within the measurement period information.
#' @importFrom utils timestamp
#' @export
#' @examples
#' binfile_path <- system.file("inst/extdata/20Hz_file.bin", package = "GENEAcore")
#' con <- file(binfile_path, "r")
#' binfile <- readLines(con, skipNul = TRUE)
#' close(con)
#' output_folder <- "."
#' MPI <- create_MPI(binfile, binfile_path, output_folder)
#' MPI <- detect_transitions(binfile, binfile_path, output_folder)
detect_transitions <- function(binfile,
                               binfile_path,
                               output_folder,
                               minimum_event_duration = 3,
                               x_cpt_penalty = 20,
                               y_cpt_penalty = 30,
                               z_cpt_penalty = 20,
                               CutTime24Hr = "15:00") {
  # :DEV: add checks for CutTime24Hr and other input formats
  CutTime24Hr <- check_time_format(CutTime24Hr)

  # Get UniqueBinFileIdentifier
  UniqueBinFileIdentifier <- get_UniqueBinFileIdentifier(binfile)

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

    MPI$file_data[["CutTime24Hr"]] <- CutTime24Hr

    CutTime24HrOffset <- 60 * (60 * as.numeric(unlist(strsplit(CutTime24Hr, ":"))[1]) + as.numeric(unlist(strsplit(CutTime24Hr, ":"))[2]))
    if ((MPI$file_data[["MeasurementStartTime"]] - MPI$file_data[["FirstLocalMidnightTime"]]) > CutTime24HrOffset) {
      LocalFirstCutTime <- MPI$file_data[["FirstLocalMidnightTime"]] + CutTime24HrOffset
    } else {
      LocalFirstCutTime <- MPI$file_data[["FirstLocalMidnightTime"]] - (24 * 60 * 60 - CutTime24HrOffset)
    }

    if (LocalFirstCutTime < MPI$file_data[["MeasurementEndTime"]]) {
      CutTimes <- seq(LocalFirstCutTime, MPI$file_data[["MeasurementEndTime"]], 24 * 60 * 60)
      CutTimes <- c(MPI$file_data[["MeasurementStartTime"]], CutTimes, MPI$file_data[["MeasurementEndTime"]])
    } else {
      CutTimes <- c(MPI$file_data[["MeasurementStartTime"]], MPI$file_data[["MeasurementEndTime"]])
    }
    CutTimes <- sort(unique(CutTimes))
    CutTimes <- CutTimes[CutTimes >= MPI$file_data[["MeasurementStartTime"]]]
    MPI$file_data[["NumberDays"]] <- length(CutTimes) - 1

    cpts_var_all <- c()
    cpts_index <- c()
    days_all <- c()
    for (i in 1:MPI$file_data[["NumberDays"]]) {
      # print(paste("Day", i))
      day_measurements <- subset(measurements, timestamp >= CutTimes[i] & timestamp < CutTimes[i + 1])
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
      cpts_var_day <- data.frame(unclass(rle(c(0, diff(c(0, cpts_var_day))))))
      cpts_var_day$time <- cumsum(cpts_var_day$values)
      cpts_var_day <- cpts_var_day[cpts_var_day$values >= minimum_event_duration, ]
      cpts_var_day <- cpts_var_day$time

      # Finalise output
      cpts_var_all <- c(cpts_var_all, day_measurements$time[cpts_var_day])
      cpts_index <- c(cpts_index, cpts_var_day)
      days_all <- c(days_all, rep(i, length(cpts_var_day)))
    }

    CutTimes_df <- data.frame(
      time = CutTimes,
      index = c(rep(1, MPI$file_data[["NumberDays"]]), nrow(day_measurements)),
      day = c(1:MPI$file_data[["NumberDays"]], MPI$file_data[["NumberDays"]])
    )
    if (MPI$file_data[["NumberDays"]] > 1) {
      CutTimes_df <- new_cut_times(CutTimes_df)
    }
    cpts_var_df <- data.frame(time = cpts_var_all, index = cpts_index, day = days_all)
    cpts_var_all_df <- rbind(cpts_var_df, CutTimes_df)
    cpts_var_all_df <- cpts_var_all_df[order(cpts_var_all_df$time), ]
    cpts_var_all_df <- unique(cpts_var_all_df)
    cpts_var_all_df <- cpts_var_all_df[!duplicated(cpts_var_all_df[, c("index", "day")]), ]

    # Update MPI and add note to file history
    MPI$file_history <- rbind(MPI$file_history, paste0(substr(Sys.time(), 0, 23), " event transitions added"))
    MPI$transitions <- cpts_var_all_df
    saveRDS(MPI, MPI_filepath)

    return(MPI)
  } else {
    warning(paste(basename(binfile_path), ": UniqueBinFileIdentifier cannot be created, no transitions calculated"))
    MPI <- NA
    return(MPI)
  }
}


#' New Cut Times
#'
#' @details Add the timestamps, indexes and day numbers of the cut times and their ends.
#' @param df Cut Times data frame.
#' @return Data frame with added cut times.
#' @export
#' @examples
#' CutTimes_df <- data.frame(
#'   time = c(1731421000, 1731421100, 1731421362, 1731421480, 1731421525),
#'   index = c(56, 1, 230, 1, 400), day = c(1, 2, 2, 3, 3)
#' )
#' CutTimes_df <- new_cut_times(CutTimes_df)
new_cut_times <- function(df) {
  new_times <- integer()
  new_indices <- integer()
  new_days <- integer()

  for (i in 2:(nrow(df) - 1)) {
    new_time <- df$time[i] - 1
    new_index <- df$time[i] - df$time[i - 1]
    new_day <- df$day[i - 1]

    new_times <- c(new_times, new_time)
    new_indices <- c(new_indices, new_index)
    new_days <- c(new_days, new_day)
  }

  new_df <- data.frame(time = new_times, index = new_indices, day = new_days)
  combined_df <- rbind(df, new_df)
  combined_df <- combined_df[order(combined_df$time), ]
  return(combined_df)
}
