#' Main GENEAcore Function
#'
#' @param data_folder Folder that contains raw data bin files to process.
#' @param CutTime24Hr Time in 24h to split the days up by.
#' @param output_epochs Create epoch outputs.
#' @param epoch_duration Specify duration of fixed epochs.
#' @param output_events Create event outputs.
#' @param output_steps Include step counts and stepping rate outputs.
#' @param output_csv Allows CSV output to be saved during epoch and event processing.
#' @param timer Print elapsed times of each process.
#' @return RDS and CSV of Measurement Period Information, Epoch measures and Event measures.
#' @export
#' @importFrom utils write.csv write.table
#' @importFrom stats time
geneacore <- function(
    data_folder = data_folder,
    CutTime24Hr = "15:00",
    output_epochs = TRUE,
    epoch_duration = 1,
    output_events = TRUE,
    output_steps = FALSE,
    output_csv = FALSE,
    timer = FALSE) {
  data_files <- (list.files(data_folder, pattern = "(?i)\\.bin$"))
  total_files <- length(data_files)
  # batch_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
  # summary_output_filepath <- file.path(getwd(), paste0("binfile_summary_", batch_time, ".csv"))
  for (seq in 1:total_files) {
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
      MPI <- create_MPI(binfile, binfile_path, output_folder)
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

    if (output_events) {
      # Detect transitions
      elapsed_p5 <- system.time({
        MPI <- detect_transitions(binfile, binfile_path, output_folder, CutTime24Hr = CutTime24Hr)
        transitions <- MPI$transitions
        rownames(transitions) <- NULL
      })
      if (timer) print(paste0("Transitions detection time: ", round(elapsed_p5["user.self"] + elapsed_p5["sys.self"], 2), " seconds"))
    }

    rm(list = grep("elapsed", ls(), value = TRUE))

    # Create bin file summary
    # MPI_summary(MPI, summary_output_filepath, quiet = TRUE)

    ## Epochs and events creation
    if (output_epochs || output_events) {
      cut_time <- check_time_format(CutTime24Hr)
      cut_time <- strptime(cut_time, format = "%H:%M")$hour
      cut_time_shift <- (cut_time * 60 * 60) - MPI$file_data[["TimeOffset"]]
      first_day <- as.Date(as.POSIXct(MPI$file_info$firsttimestamp - cut_time_shift, origin = "1970-01-01"))
      last_day <- as.Date(as.POSIXct(MPI$file_info$lasttimestamp - cut_time_shift, origin = "1970-01-01"))
      # Generate start and end time for each day we need to process
      days_to_process <- seq(first_day, last_day, by = 1)
      date_range <- lapply(days_to_process, FUN = function(x) {
        c(
          "start" = max(MPI$file_info$firsttimestamp, as.numeric(as.POSIXlt(x)) + cut_time_shift),
          "end" = min(MPI$file_info$lasttimestamp, as.numeric(as.POSIXlt(x + 1)) + cut_time_shift)
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
              downsample = FALSE
            )
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
            calibrated <- apply_degrees(
              apply_updown(
                apply_AGSA(
                  apply_ENMO(calibrated)
                )
              )
            )
          })

          if (timer) {
            print(paste0("Reading ", nrow(results), " rows of raw data time: ", round(elapsed_res["user.self"] + elapsed_res["sys.self"], 2), " seconds"))
            print(paste0("Apply calibration time: ", round(elapsed_cal["user.self"] + elapsed_cal["sys.self"], 2), " seconds"))
            print(paste0("Apply calculations time: ", round(elapsed_calc["user.self"] + elapsed_calc["sys.self"], 2), " seconds"))
          }
          # print(paste0("Read and calibrate time: ", round(elapsed_res["user.self"] + elapsed_res["sys.self"], 2) + round(elapsed_cal["user.self"] + elapsed_cal["sys.self"], 2), " seconds"))

          if (output_epochs) {
            # print("Aggregating epochs")
            elapsed_ep <- system.time({
              epochs_agg <- aggregateEpochs(calibrated,
                duration = epoch_duration,
                measure = c("x", "y", "z", "light", "temp", "AGSA", "ENMO", "updown", "degrees"),
                time = "timestamp",
                sample_frequency = sample_frequency,
                fun = function(x) c(mean = mean(x), max = max(x), sd = sd(x))
              )
              epochs_list[[day_number]] <- epochs_agg
            })
            if (timer) print(paste0("Epoch creation time: ", round(elapsed_ep["user.self"] + elapsed_ep["sys.self"], 2), " seconds"))

            # Step Counter
            if (output_steps) {
              elapsed_steps <- system.time({
                index_step <- epoch_duration * sample_frequency # number of samples per epoch
                epochs <- as.integer(nrow(calibrated) / index_step) # total number of epochs
                for (epochnumber in 1:epochs) { # step counter for each epoch
                  steps <- stepCounter(calibrated[((1 + (epochnumber - 1) * index_step):(epochnumber * index_step)), "y"], samplefreq = sample_frequency)
                  steps_epochs_df <- rbind(steps_epochs_df, steps)
                }
                if (nrow(calibrated) %% index_step > 0) { # step counter for remaining steps that don't fit in a full epoch
                  steps <- stepCounter(calibrated[((1 + epochs * index_step):nrow(calibrated)), "y"], samplefreq = sample_frequency)
                  steps_epochs_df <- rbind(steps_epochs_df, steps)
                }
              })
              if (timer) print(paste0("Epoch steps calculation time: ", round(elapsed_steps["user.self"] + elapsed_steps["sys.self"], 2), " seconds"))
            }
          }
          if (output_events) {
            # print("Aggregating events")
            elapsed_ev <- system.time({
              day_transitions <- transitions[transitions$day == day_number, "index"]
              events <- data.frame(
                "start" = day_transitions[-length(day_transitions)],
                "end" = floor(sample_frequency * (day_transitions[-1]))
              )
              if (nrow(events) > 1) {
                events$start[2:nrow(events)] <- events$end[-nrow(events)] + 1
              }
              events_agg <- aggregateEvents(calibrated,
                measure = c("x", "y", "z", "light", "temp", "AGSA", "ENMO", "updown", "degrees"),
                time = "timestamp",
                sample_frequency = sample_frequency,
                events = events,
                fun = function(x) c(mean = mean(x), max = max(x), sd = sd(x))
              )
              events_list[[day_number]] <- events_agg
            })
            if (timer) print(paste0("Event creation time: ", round(elapsed_ev["user.self"] + elapsed_ev["sys.self"], 2), " seconds"))

            # Step Counter
            if (output_steps) {
              elapsed_steps <- system.time({
                for (eventnumber in 1:nrow(events)) {
                  steps <- stepCounter(calibrated[(events$start[eventnumber]:events$end[eventnumber]), "y"], samplefreq = sample_frequency)
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
          colnames(steps_epochs_df) <- c("step.count", "step.mean", "step.sd")
          epochs_df <- cbind(epochs_df, steps_epochs_df)
        }
        if (output_steps && output_events) {
          colnames(steps_events_df) <- c("step.count", "step.mean", "step.sd")
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
      # else {
      #   warning(paste(basename(binfile_path), ": There are no epochs to output."))
      # }
      if (output_events && !is.null(events_df)) {
        events_df <- reorder_df(events_df)
        output_location <- file.path(output_folder, paste0(MPI$file_data[["UniqueBinFileIdentifier"]], "_events"))
        saveRDS(events_df, paste0(output_location, ".rds"))
        if (output_csv) write.csv(round_columns(events_df), file = paste0(output_location, ".csv"), row.names = FALSE)
      }
      # else {
      #   warning(paste(basename(binfile_path), ": There are no events to output."))
      # }
    }
  }
}
