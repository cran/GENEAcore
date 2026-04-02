#' GENEAcore Part 3
#'
#' @description GENEAcore function which performs the following tasks for each valid day:
#' \itemize{
#'  \item Applies calibration parameters to raw data
#'  \item Identifies transitions for events processing only
#'  \item Performs aggregation by event and/or epoch
#'  \item Calculate steps, if option selected
#'  \item Calculate non-wear coverage and rest coverage of events and/or epochs
#' }
#'
#' @param data_folder Folder that contains raw data bin files to process or path to single bin file.
#' @param control Named list of optional settings for geneacore functions.
#' @details
#' Control options (all optional):
#' \describe{
#'  \item{output_epochs}{Logical. Default `FALSE`. Create epoch outputs.}
#'  \item{epoch_duration}{Integer. Default 1. Specify duration of fixed epochs.}
#'  \item{output_events}{Logical. Default `TRUE`. Create event outputs.}
#'  \item{output_steps}{Logical. Default `FALSE`. Calculate step counts and stepping rate during epoch processing.
#'  Steps are always calculated during events processing.}
#'  \item{output_csv}{Logical. Default `FALSE`. Allows CSV output to be saved during epoch and event processing.}
#'  \item{timer}{Logical. Default `FALSE`. Print elapsed times of each processing step.}
#'  \item{multisession}{Integer vector of length 2. Default `c(1,1)`.
#'  Controls day split when running function across multiple R sessions.
#'  The first value is the current session number and the second is the total number of sessions.
#'  The default processes all days in a single session.}
#'  \item{required_processing_hours}{Integer. Default 0. Hours of wear time in a 24-hour day to be considered a valid day to be processed.}
#' }
#'
#' @examples
#' \dontrun{
#' controls <- list(output_csv = TRUE, required_processing_hours = 4, timer = TRUE)
#' geneacore_part3("path/to/folder", controls)
#' }
#'
#' @export
geneacore_part3 <- function(data_folder = data_folder, control = list()) {
  defaults <- list(
    output_epochs = FALSE,
    epoch_duration = 1,
    output_events = TRUE,
    output_steps = FALSE,
    output_csv = FALSE,
    timer = FALSE,
    multisession = c(1, 1),
    required_processing_hours = 0
  )
  ctrl <- merge_control(defaults, control)
  ## Add: check that MPI and Raw sample RDS files exist
  if (file.info(data_folder)$isdir) {
    data_files <- list.files(data_folder, pattern = "(?i)\\.bin$")
    if (length(data_files) == 0) {
      stop(sprintf("No .bin files found in directory: %s", data_folder))
    }
  } else {
    if (!grepl("(?i)\\.bin$", data_folder)) {
      stop(sprintf("The file '%s' does not have a .bin extension.", data_folder))
    }
    # Process a single file
    data_files <- basename(data_folder)
    data_folder <- dirname(data_folder)
  }
  mpi_summary <- MPI_summary(data_folder)
  mpi_summary <- mpi_summary[mpi_summary$MeasurementDurationActual >= 5,]
  total_files <- length(data_files)
  for (seq in 1:total_files) {
    project <- gsub("\\.bin", "", basename(data_files[seq]))
    output_folder <- file.path(data_folder, project)

    UniqueBinFileIdentifier <- mpi_summary$UniqueBinFileIdentifier[which(mpi_summary$BinfileName == data_files[seq])]

    if (is.character(UniqueBinFileIdentifier) && length(UniqueBinFileIdentifier) == 0 || all(is.na(UniqueBinFileIdentifier))) {
      next
    }

    mpi_filepath <- file.path(output_folder, paste0(UniqueBinFileIdentifier, "_MPI.rds"))
    MPI <- readRDS(mpi_filepath)

    cut_times <- get_cut_times(MPI$file_data[["CutTime24Hr"]], MPI)
    number_days <- MPI$file_data[["NumberDays"]]

    # Check day_range is valid
    multisession <- as.integer(ctrl$multisession)
    current_instance <- multisession[1]
    current_instance <- min(current_instance, number_days)
    total_instances <- min(multisession[2], number_days)
    if (current_instance > total_instances) current_instance <- total_instances
    days_per_instance <- ceiling(number_days / total_instances)
    start_day <- (current_instance - 1) * days_per_instance + 1
    end_day <- min(current_instance * days_per_instance, number_days)
    if (start_day > number_days) {
      day_range <- integer(0)
    } else {
      day_range <- seq(start_day, end_day, 1)
    }

    if (ctrl$output_events) {
      downsampled_measurements <- readRDS(file.path(output_folder, paste0(UniqueBinFileIdentifier, "_downsample.rds")))
    }

    ## Epochs and events creation

    message(paste("Running geneacore_part3 for file", seq, "of", length(data_files), ":", project))

    if (ctrl$output_epochs || ctrl$output_events) {
      date_range <- data.frame(start = cut_times[1:(length(cut_times) - 1)], end = cut_times[2:(length(cut_times))])
      date_range$day <- rownames(date_range)
      date_range$length <- date_range$end - date_range$start
      date_range <- date_range[date_range$length > 5, ]

      nonwear_by_day <- valid_day_nonwear(MPI[["non_movement"]][["non_wear"]], date_range, ctrl$required_processing_hours, MPI$file_data[["CutTime24Hr"]])

      sample_frequency <- MPI$file_data[["MeasurementFrequency"]]

      load_time <- system.time({
        for (day_number in day_range) {
          steps_epochs_df <- data.frame()
          steps_events_df <- data.frame()

          if (nonwear_by_day$valid_day$valid_day[nonwear_by_day$valid_day$day == day_number]) {
            message(paste("Processing Day", day_number, "of", nrow(date_range)))
            elapsed_res <- system.time({
              results <- readRDS(file.path(
                output_folder,
                paste0(UniqueBinFileIdentifier, "_", date_range$start[day_number], "_", date_range$end[day_number], "_rawdata.rds")
              ))
            })

            elapsed_cal <- system.time({
              # Apply auto calibration and other calculated measures
              if ("auto_calibration" %in% names(MPI)) {
                calibrated <- apply_calibration(results, MPI$auto_calibration, MPI$file_data[["MeasurementDevice"]])
              } else {
                calibrated <- apply_calibration(results, MPI$factory_calibration, MPI$file_data[["MeasurementDevice"]])
              }
            })
            elapsed_calc <- system.time({
              calibrated <- apply_all(calibrated)
            })

            if (ctrl$timer) {
              print(paste0("Reading ", nrow(results), " rows of raw data time: ", round(elapsed_res["user.self"] + elapsed_res["sys.self"], 2), " seconds"))
              print(paste0("Apply calibration time: ", round(elapsed_cal["user.self"] + elapsed_cal["sys.self"], 2), " seconds"))
              print(paste0("Apply calculations time: ", round(elapsed_calc["user.self"] + elapsed_calc["sys.self"], 2), " seconds"))
            }

            if (ctrl$output_epochs) {
              elapsed_ep <- system.time({
                epochs_df <- aggregate_epochs(calibrated,
                  duration = ctrl$epoch_duration,
                  measure = c("x", "y", "z", "Light", "Temp", "AGSA", "ENMO", "UpDown", "Degrees"),
                  time = "TimeUTC",
                  sample_frequency = sample_frequency,
                  fun = function(x) c(Mean = mean(x), Max = max(x), SD = sd(x))
                )
                epochs_df$DayNumber <- rep(day_number, nrow(epochs_df))
              })
              if (ctrl$timer) print(paste0("Epoch creation time: ", round(elapsed_ep["user.self"] + elapsed_ep["sys.self"], 2), " seconds"))

              # Step Counter
              if (ctrl$output_steps) {
                index_step <- ctrl$epoch_duration * sample_frequency # number of samples per epoch
                epochs <- as.integer(nrow(calibrated) / index_step) # total number of epochs
                if (epochs > 0) {
                  if (ctrl$epoch_duration >= 5) {
                    elapsed_steps <- system.time({
                      for (epochnumber in 1:epochs) { # step counter for each epoch
                        steps <- step_counter(calibrated[((1 + (epochnumber - 1) * index_step):(epochnumber * index_step)), "y"],
                          sample_frequency = sample_frequency
                        )
                        steps_epochs_df <- rbind(steps_epochs_df, steps)
                      }
                    })
                    if (ctrl$timer) print(paste0("Epoch steps calculation time: ", round(elapsed_steps["user.self"] + elapsed_steps["sys.self"], 2), " seconds"))
                  } else {
                    steps_epochs_df <- rbind(steps_epochs_df, as.data.frame(matrix(NA, nrow = epochs, ncol = 4)))
                    message("Epoch duration below minimum threshold (5 seconds): steps were not calculated and have been set to NA.")
                  }
                  colnames(steps_epochs_df) <- c("StepCount", "StepMean", "StepSD", "StepDiff")
                  epochs_df <- cbind(epochs_df, steps_epochs_df)
                }
              }
            }
            if (ctrl$output_events) {
              elapsed_ev <- system.time({
                day_measurements <- subset(
                  downsampled_measurements,
                  TimeUTC >= date_range$start[day_number] & TimeUTC < date_range$end[day_number]
                )
                day_transitions <- detect_transitions(day_measurements, cut_time_24hr = MPI$file_data[["CutTime24Hr"]])
                events <- get_events(calibrated, day_transitions, sample_frequency)
                if (is.null(events) || nrow(events) == 0) {
                  next
                }
                events_df <- aggregate_events(calibrated,
                  measure = c("x", "y", "z", "Light", "Temp", "AGSA", "ENMO", "UpDown", "Degrees"),
                  time = "TimeUTC",
                  sample_frequency = sample_frequency,
                  events = events,
                  fun = function(x) c(Mean = mean(x), Max = max(x), SD = sd(x))
                )
                events_df$DayNumber <- rep(day_number, nrow(events_df))
              })
              if (ctrl$timer) print(paste0("Event creation time: ", round(elapsed_ev["user.self"] + elapsed_ev["sys.self"], 2), " seconds"))

              # Step Counter
              elapsed_steps <- system.time({
                for (eventnumber in seq_len(nrow(events))) {
                  steps <- step_counter(calibrated[(events$start[eventnumber]:events$end[eventnumber]), "y"],
                    sample_frequency = sample_frequency
                  )
                  steps_events_df <- rbind(steps_events_df, steps)
                }
                colnames(steps_events_df) <- c("StepCount", "StepMean", "StepSD", "StepDiff")
                events_df <- cbind(events_df, steps_events_df)
              })

              if (ctrl$timer) print(paste0("Event steps calculation time: ", round(elapsed_steps["user.self"] + elapsed_steps["sys.self"], 2), " seconds"))
            }
            rm(list = grep("elapsed", ls(), value = TRUE))

            if (ctrl$output_epochs && !is.null(epochs_df)) {
              epochs_df <- reorder_df(epochs_df)
              epochs_df <- nonwear_rest_coverage(MPI, epochs_df, date_range$start[day_number], date_range$end[day_number], nonwear_by_day$nonwear_day)
              output_location <- file.path(output_folder, paste0(MPI$file_data[["UniqueBinFileIdentifier"]], "_day", day_number, "_epochs_", ctrl$epoch_duration, "s"))
              saveRDS(epochs_df, paste0(output_location, ".rds"))
              if (ctrl$output_csv) write.csv(round_columns(epochs_df), file = paste0(output_location, ".csv"), row.names = FALSE)
            }
            if (ctrl$output_events && !is.null(events_df)) {
              events_df <- reorder_df(events_df)
              events_df <- nonwear_rest_coverage(MPI, events_df, date_range$start[day_number], date_range$end[day_number], nonwear_by_day$nonwear_day)
              output_location <- file.path(output_folder, paste0(MPI$file_data[["UniqueBinFileIdentifier"]], "_day", day_number, "_events"))
              saveRDS(events_df, paste0(output_location, ".rds"))
              if (ctrl$output_csv) write.csv(round_columns(events_df), file = paste0(output_location, ".csv"), row.names = FALSE)
            }
          } else {
            message(paste("Day", day_number, "of", nrow(date_range), " is not a valid day and will not be processed"))
          }
        }
      })

      # message("Total wall-clock time: ", round(load_time["elapsed"], 2), " seconds")
      if (ctrl$timer) print(paste0("Total epoch/event creation time: ", round(load_time["user.self"] + load_time["sys.self"], 2), " seconds"))
    }
  }
}
