#' Main GENEAcore Function
#' @description Main GENEAcore Function which performs the following tasks:
#' \itemize{
#'   \item Checks for and reads or creates Measurement Processing Information (MPI)
#'   \item Downsamples the file to 1Hz and detects periods of non movement
#'   \item Calculates auto calibration settings for the device
#'   \item Identifies transitions (if processing events)
#'   \item Applies auto calibration scaling
#'   \item Counts steps
#'   \item Performs aggregations by event or epoch
#'   \item Writes out data
#' }
#'
#' @param data_folder Folder that contains raw data bin files to process or path to single bin file.
#' @param cut_time_24hr Time in 24h to split the days up by.
#' @param output_epochs Create epoch outputs.
#' @param epoch_duration Specify duration of fixed epochs.
#' @param output_events Create event outputs.
#' @param output_steps Include step counts and stepping rate outputs.
#' @param output_csv Allows CSV output to be saved during epoch and event processing.
#' @param timer Print elapsed times of each process.
#' @param summary Create bin file summary for all files in data folder.
#' @return RDS and CSV of Measurement Period Information, Epoch measures and Event measures.
#' @export
#' @importFrom utils write.csv write.table
#' @importFrom stats time
geneacore <- function(
    data_folder = data_folder,
    cut_time_24hr = "15:00",
    output_epochs = TRUE,
    epoch_duration = 1,
    output_events = TRUE,
    output_steps = FALSE,
    output_csv = FALSE,
    timer = FALSE,
    summary = FALSE) {
  if (file.info(data_folder)$isdir) {
    data_files <- list.files(data_folder, pattern = "(?i)\\.bin$")
  } else {
    # Process a single file
    data_files <- basename(data_folder)
    data_folder <- dirname(data_folder)
  }
  if (summary) {
    summary <- binfile_summary(data_folder)
    write.csv(summary, file.path(data_folder, "binfile_summary.csv"), row.names = FALSE)
  }
  total_files <- length(data_files)
  for (seq in 1:total_files) {
    tryCatch(
      {
        binfile_path <- file.path(data_folder, data_files[seq])
        project <- gsub("\\.bin", "", basename(binfile_path))
        output_folder <- file.path(data_folder, project)
        if (!file.exists(output_folder)) {
          dir.create(output_folder)
        }

        message(paste0("Processing file ", seq, "/", total_files, " : ", data_files[seq]))

        ## Pre-processing
        # Open file connection and read file
        elapsed_p1 <- system.time({
          con <- file(binfile_path, "r")
          binfile <- readLines(con, skipNul = TRUE)
          close(con)
        })
        if (timer) print(paste0("File read time: ", round(elapsed_p1["user.self"] + elapsed_p1["sys.self"], 2), " seconds"))

        # Create MPI
        elapsed_p2 <- system.time({
          UniqueBinFileIdentifier <- get_UniqueBinFileIdentifier(binfile)
          MPI <- create_MPI(binfile, binfile_path, output_folder)
          MPI_filepath <- file.path(output_folder, paste0(UniqueBinFileIdentifier, "_MPI.rds"))
        })
        if (timer) print(paste0("MPI creation time: ", round(elapsed_p2["user.self"] + elapsed_p2["sys.self"], 2), " seconds"))

        # Downsample file and detect non-movement
        elapsed_p3 <- system.time({
          MPI <- detect_nonmovement(binfile, binfile_path, output_folder)
        })
        if (timer) print(paste0("Non-movement detection time: ", round(elapsed_p3["user.self"] + elapsed_p3["sys.self"], 2), " seconds"))

        # Calculate auto-calibration parameters
        elapsed_p4 <- system.time({
          MPI <- calc_autocalparams(
            binfile, binfile_path, output_folder,
            MPI$non_movement$sphere_points
          )
        })
        if (timer) print(paste0("Auto-calibration calculation time: ", round(elapsed_p4["user.self"] + elapsed_p4["sys.self"], 2), " seconds"))

        cut_time_24hr <- check_time_format(cut_time_24hr)

        if (output_events) {
          # Detect transitions
          elapsed_p5 <- system.time({
            MPI <- detect_transitions(binfile, binfile_path, output_folder, cut_time_24hr = cut_time_24hr)
            transitions <- MPI$transitions
            rownames(transitions) <- NULL
          })
          if (timer) print(paste0("Transitions detection time: ", round(elapsed_p5["user.self"] + elapsed_p5["sys.self"], 2), " seconds"))
        }

        rm(list = grep("elapsed", ls(), value = TRUE))

        ## Epochs and events creation
        if (output_epochs || output_events) {
          cut_time <- strptime(cut_time_24hr, format = "%H:%M")$hour
          cut_time_shift <- (cut_time * 60 * 60) - MPI$file_data[["TimeOffset"]]
          first_day <- as.Date(as.POSIXct(MPI$file_info$first_time_UTC - cut_time_shift, origin = "1970-01-01"))
          last_day <- as.Date(as.POSIXct(MPI$file_info$last_time_UTC - cut_time_shift, origin = "1970-01-01"))
          # Generate start and end time for each day we need to process
          days_to_process <- seq(first_day, last_day, by = 1)
          date_range <- lapply(days_to_process, FUN = function(x) {
            c(
              "start" = max(MPI$file_info$first_time_UTC, as.numeric(as.POSIXlt(x)) + cut_time_shift),
              "end" = min(MPI$file_info$last_time_UTC, as.numeric(as.POSIXlt(x + 1)) + cut_time_shift)
            )
          })
          # Convert to data frame
          date_range <- data.frame(t(sapply(date_range, c)))
          date_range$day <- rownames(date_range)
          date_range$length <- date_range$end - date_range$start
          date_range <- date_range[date_range$length > 5, ]

          sample_frequency <- MPI$file_data[["MeasurementFrequency"]]
          epochs_list <- list()
          events_list <- list()
          steps_epochs_df <- data.frame()
          steps_events_df <- data.frame()

          load_time <- system.time({
            for (day_number in date_range$day[1]:date_range$day[nrow(date_range)]) {
              message(paste("Processing Day", day_number, "of", nrow(date_range)))
              elapsed_res <- system.time({
                results <- sample_binfile(binfile, binfile_path, output_folder,
                  start_time = date_range[day_number, 1],
                  end_time = date_range[day_number, 2],
                  downsample = FALSE, save_raw = FALSE
                )
              })

              # Add button press timestamps to MPI
              MPI$button <- sort(unique(c(MPI$button, results$timestamp[results$button > 0])))

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

              if (timer) {
                print(paste0("Reading ", nrow(results), " rows of raw data time: ", round(elapsed_res["user.self"] + elapsed_res["sys.self"], 2), " seconds"))
                print(paste0("Apply calibration time: ", round(elapsed_cal["user.self"] + elapsed_cal["sys.self"], 2), " seconds"))
                print(paste0("Apply calculations time: ", round(elapsed_calc["user.self"] + elapsed_calc["sys.self"], 2), " seconds"))
              }

              if (output_epochs) {
                elapsed_ep <- system.time({
                  epochs_agg <- aggregate_epochs(calibrated,
                    duration = epoch_duration,
                    measure = c("x", "y", "z", "Light", "Temp", "AGSA", "ENMO", "UpDown", "Degrees"),
                    time = "TimeUTC",
                    sample_frequency = sample_frequency,
                    fun = function(x) c(Mean = mean(x), Max = max(x), SD = sd(x))
                  )
                  epochs_agg$DayNumber <- rep(day_number, nrow(epochs_agg))
                  epochs_list[[day_number]] <- epochs_agg
                })
                if (timer) print(paste0("Epoch creation time: ", round(elapsed_ep["user.self"] + elapsed_ep["sys.self"], 2), " seconds"))

                # Step Counter
                if (output_steps) {
                  index_step <- epoch_duration * sample_frequency # number of samples per epoch
                  epochs <- as.integer(nrow(calibrated) / index_step) # total number of epochs
                  if (epoch_duration >= 5) {
                    elapsed_steps <- system.time({
                      for (epochnumber in 1:epochs) { # step counter for each epoch
                        steps <- step_counter(calibrated[((1 + (epochnumber - 1) * index_step):(epochnumber * index_step)), "y"],
                          sample_frequency = sample_frequency
                        )
                        steps_epochs_df <- rbind(steps_epochs_df, steps)
                      }
                    })
                    if (timer) print(paste0("Epoch steps calculation time: ", round(elapsed_steps["user.self"] + elapsed_steps["sys.self"], 2), " seconds"))
                  } else {
                    steps_epochs_df <- rbind(steps_epochs_df, as.data.frame(matrix(NA, nrow = epochs, ncol = 4)))
                    message("Epoch duration below minimum threshold (5 seconds): steps were not calculated and have been set to NA.")
                  }
                }
              }
              if (output_events) {
                elapsed_ev <- system.time({
                  day_transitions <- transitions[transitions$day == day_number, "index"]
                  rle_cal_lengths <- rle(calibrated$TimeUTC)$lengths
                  cum_lengths <- cumsum(rle_cal_lengths)
                  events <- data.frame(
                    "start" = day_transitions[-length(day_transitions)],
                    "end" = cum_lengths[day_transitions - 1]
                  )
                  if (length(day_transitions) == 1) {
                    events <- data.frame("start" = day_transitions, "end" = nrow(calibrated))
                  }
                  if (nrow(events) > 1) {
                    events$start[2:nrow(events)] <- events$end[-nrow(events)] + 1
                  }
                  if ((nrow(calibrated) - events$end[length(events$end)]) > sample_frequency) {
                    events <- rbind(events, c(events$end[length(events$end)] + 1, nrow(calibrated)))
                  } else {
                    events$end[length(events$end)] <- nrow(calibrated)
                  }

                  events_agg <- aggregate_events(calibrated,
                    measure = c("x", "y", "z", "Light", "Temp", "AGSA", "ENMO", "UpDown", "Degrees"),
                    time = "TimeUTC",
                    sample_frequency = sample_frequency,
                    events = events,
                    fun = function(x) c(Mean = mean(x), Max = max(x), SD = sd(x))
                  )
                  events_agg$DayNumber <- rep(day_number, nrow(events_agg))
                  events_list[[day_number]] <- events_agg
                })
                if (timer) print(paste0("Event creation time: ", round(elapsed_ev["user.self"] + elapsed_ev["sys.self"], 2), " seconds"))

                # Step Counter
                if (output_steps) {
                  elapsed_steps <- system.time({
                    for (eventnumber in seq_len(nrow(events))) {
                      steps <- step_counter(calibrated[(events$start[eventnumber]:events$end[eventnumber]), "y"],
                        sample_frequency = sample_frequency
                      )
                      steps_events_df <- rbind(steps_events_df, steps)
                    }
                  })

                  if (timer) print(paste0("Event steps calculation time: ", round(elapsed_steps["user.self"] + elapsed_steps["sys.self"], 2), " seconds"))
                }
              }
            }
            rm(list = grep("elapsed", ls(), value = TRUE))
            epochs_df <- do.call(rbind, epochs_list)
            events_df <- do.call(rbind, events_list)

            if (output_steps && output_epochs) {
              MPI <- step_counter_history(MPI_filepath, sample_frequency = sample_frequency)
              colnames(steps_epochs_df) <- c("StepCount", "StepMean", "StepSD", "StepDiff")
              epochs_df <- cbind(epochs_df, steps_epochs_df)
            }
            if (output_steps && output_events) {
              MPI <- step_counter_history(MPI_filepath, sample_frequency = sample_frequency)
              colnames(steps_events_df) <- c("StepCount", "StepMean", "StepSD", "StepDiff")
              events_df <- cbind(events_df, steps_events_df)
            }
          })
          if (timer) print(paste0("Total epoch/event creation time: ", round(load_time["user.self"] + load_time["sys.self"], 2), " seconds"))

          if (output_epochs && !is.null(epochs_df)) {
            epochs_df <- reorder_df(epochs_df)
            output_location <- file.path(output_folder, paste0(MPI$file_data[["UniqueBinFileIdentifier"]], "_epochs_", epoch_duration))
            saveRDS(epochs_df, paste0(output_location, "s.rds"))
            if (output_csv) write.csv(round_columns(epochs_df), file = paste0(output_location, "s.csv"), row.names = FALSE)
          }
          if (output_events && !is.null(events_df)) {
            events_df <- reorder_df(events_df)
            output_location <- file.path(output_folder, paste0(MPI$file_data[["UniqueBinFileIdentifier"]], "_events"))
            saveRDS(events_df, paste0(output_location, ".rds"))
            if (output_csv) write.csv(round_columns(events_df), file = paste0(output_location, ".csv"), row.names = FALSE)
          }
        }
      },
      error = function(e) {
        warning(paste("Error processing", binfile_path, ",\n", e))
      }
    )
  }
}
