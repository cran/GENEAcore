## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval = FALSE-------------------------------------------------------------
#  vignette("breaking-changes", package = "GENEAcore")

## ----eval = FALSE-------------------------------------------------------------
#  browseURL(system.file("extdata", "GENEAcore_Data_Dictionary_21Nov2025.pdf", package = "GENEAcore"))

## ----Installing from CRAN, eval = FALSE---------------------------------------
#  install.packages("GENEAcore", dependencies = TRUE)

## ----Installing from source, eval = FALSE-------------------------------------
#  # Note that R only uses / not \ when referring to a file/directory location
#  install.packages("changepoint")
#  install.packages("signal")
#  install.packages("C:/path/to/GENEAcore_1.1.1.tar.gz", repos = NULL, type = "source")

## ----Loading in the GENEAcore library, eval = FALSE---------------------------
#  library(GENEAcore)
#  library(changepoint)
#  library(signal)

## ----Running geneacore, eval = FALSE------------------------------------------
#  library(GENEAcore)
#  geneacore(data_folder = "C:/path/to/datafolder")

## ----Running geneacore with parameters, eval = FALSE--------------------------
#  library(GENEAcore)
#  geneacore(
#    data_folder = "C:/path/to/datafolder",
#    cut_time_24hr = "15:00",
#    output_epochs = TRUE,
#    epoch_duration = 600, # 10 minutes
#    output_events = FALSE,
#    output_steps = FALSE,
#    output_csv = TRUE,
#    timer = FALSE
#  )

## ----Bin file summary, eval = FALSE-------------------------------------------
#  # Run summary for a single bin file
#  binfile_summary <- binfile_summary("C:/path/to/binfile.bin")

## ----Bin file folder summary, eval = FALSE------------------------------------
#  # Run summary for all bin files in bin files folder only
#  binfile_folder_summary <- binfile_summary("C:/path/to/binfilesfolder", recursive = FALSE)

## ----MPI summary, eval = FALSE------------------------------------------------
#  # Run summary for single MPI file
#  mpi_summary <- MPI_summary("C:/path/to/MPI.rds")

## ----MPI folder summary, eval = FALSE-----------------------------------------
#  # Run summary for all MPI files in a folder
#  mpi_folder_summary <- MPI_summary("C:/path/to/MPIfolder")

## ----File preparation before individual runs, eval = FALSE--------------------
#  binfile_path <- "C:/path/to/binfile"
#  output_folder <- "C:/path/to/outputfolder"
#  
#  con <- file(binfile_path, "r")
#  binfile <- readLines(con, skipNul = TRUE)
#  close(con)

## ----Create MPI, eval = FALSE-------------------------------------------------
#  MPI <- create_MPI(binfile, binfile_path, output_folder)

## ----Downsampling a file default, eval = FALSE--------------------------------
#  # Simple run using default parameter values
#  downsampled_measurements <- sample_binfile(binfile, binfile_path, output_folder)

## ----Downsampling a file with parameters, eval = FALSE------------------------
#  # Exposed parameters can be changed
#  downsampled_measurements <- sample_binfile(binfile, binfile_path, output_folder,
#    start_time = NULL,
#    end_time = NULL,
#    output_csv = FALSE
#  )

## ----Raw sampling a file default, eval = FALSE--------------------------------
#  # Simple run using default parameter values
#  raw_measurements <- sample_binfile(binfile, binfile_path, output_folder, downsample = FALSE)

## ----Raw sampling a file with parameters, eval = FALSE------------------------
#  # Exposed parameters can be changed
#  raw_measurements <- sample_binfile(binfile, binfile_path, output_folder,
#    start_time = NULL,
#    end_time = NULL,
#    downsample = FALSE,
#    output_csv = FALSE,
#    save_raw = FALSE
#  )

## ----Auto calibration default, eval = FALSE-----------------------------------
#  ## Two steps in obtaining auto calibration parameters:
#  
#  # 1. Identify non-movement periods
#  MPI <- detect_nonmovement(binfile, binfile_path, output_folder)
#  # 2. Calculate auto-calibration parameters, temperature compensation TRUE by default
#  MPI <- calc_autocalparams(binfile, binfile_path, output_folder, MPI$non_movement$sphere_points)

## ----Calibrating your data parameters, eval = FALSE---------------------------
#  # Detect non-movement
#  MPI <- detect_nonmovement(binfile, binfile_path, output_folder,
#    still_seconds = 120,
#    sd_threshold = 0.013,
#    temp_seconds = 240,
#    border_seconds = 300,
#    long_still_seconds = 120 * 60,
#    delta_temp_threshold = -0.7,
#    posture_changes_max = 2,
#    non_move_duration_max = 12 * 60 * 60
#  )
#  
#  # Calculate auto-calibration parameters
#  MPI <- calc_autocalparams(binfile, binfile_path, output_folder,
#    MPI$non_movement$sphere_points,
#    use_temp = TRUE,
#    spherecrit = 0.3,
#    maxiter = 500,
#    tol = 1e-13
#  )

## ----Apply calibration, eval = FALSE------------------------------------------
#  # Sample data
#  raw_measurements <- sample_binfile(binfile, binfile_path, output_folder, downsample = FALSE)
#  
#  # Apply factory calibration
#  calibrated_factory <- apply_calibration(raw_measurements, MPI$factory_calibration, MPI$file_data[["MeasurementDevice"]])
#  
#  # Apply auto calibration
#  calibrated_auto <- apply_calibration(raw_measurements, MPI$auto_calibration, MPI$file_data[["MeasurementDevice"]])

## ----Detect transitions for event aggregation default, eval = FALSE-----------
#  MPI <- detect_transitions(binfile, binfile_path, output_folder)

## ----Detect transitions for event aggregation parameters, eval = FALSE--------
#  MPI <- detect_transitions(binfile, binfile_path, output_folder,
#    minimum_event_duration = 5,
#    x_cpt_penalty = 18,
#    y_cpt_penalty = 25,
#    z_cpt_penalty = 16,
#    cut_time_24hr = "15:00"
#  )

## ----Applying calculations on calibrated data, eval = FALSE-------------------
#  # To apply one measure calculations
#  calibrated_measure <- apply_AGSA(calibrated)
#  
#  # To apply multiple on the same data set
#  calibrated_measures <- apply_degrees(
#    apply_updown(
#      apply_AGSA(
#        apply_ENMO(calibrated)
#      )
#    )
#  )

## ----Aggregating events, eval = FALSE-----------------------------------------
#  events_agg <- aggregate_events(calibrated,
#    measure = c("x", "y", "z", "AGSA"),
#    time = "TimeUTC",
#    sample_frequency = sample_frequency,
#    events = events,
#    fun = function(x) c(Mean = mean(x), SD = sd(x))
#  )

## ----Aggregating epochs, eval = FALSE-----------------------------------------
#  epochs_agg <- aggregate_epochs(calibrated,
#    duration = 1,
#    measure = c("x", "y", "z", "AGSA", "ENMO"),
#    time = "TimeUTC",
#    sample_frequency = MPI$file_data[["MeasurementFrequency"]],
#    fun = function(x) c(Mean = mean(x), SD = sd(x))
#  )

## ----GENEAcore flowchart, echo=FALSE------------------------------------------
knitr::include_graphics("../inst/extdata/geneacore_functions.png")

## ----Loop functions for folder, eval=FALSE------------------------------------
#  data_folder <- "C:/path/to/folder"
#  data_files <- (list.files(data_folder, pattern = "(?i)\\.bin$"))
#  
#  for (seq in 1:length(data_files)) {
#    binfile_path <- file.path(data_folder, data_files[seq])
#    project <- gsub("\\.bin", "", basename(binfile_path))
#    output_folder <- file.path(data_folder, project)
#    if (!file.exists(output_folder)) {
#      dir.create(output_folder)
#    }
#    # Open file connection and read file
#    con <- file(binfile_path, "r")
#    binfile <- readLines(con, skipNul = TRUE)
#    close(con)
#    # Create MPI
#    MPI <- create_MPI(binfile, binfile_path, output_folder)
#    # Downsample file and detect non-movement
#    MPI <- detect_nonmovement(binfile, binfile_path, output_folder)
#    # Calculate auto-calibration parameters
#    MPI <- calc_autocalparams(
#      binfile, binfile_path, output_folder,
#      MPI$non_movement$sphere_points
#    )
#  }

## ----Aggregating events day by day, eval = FALSE------------------------------
#  # Prepare time borders of each day
#  cut_time <- strptime(cut_time_24hr, format = "%H:%M")$hour
#  cut_time_shift <- (cut_time * 60 * 60) - MPI$file_data[["TimeOffset"]]
#  first_day <- as.Date(as.POSIXct(MPI$file_info$first_time_UTC - cut_time_shift, origin = "1970-01-01"))
#  last_day <- as.Date(as.POSIXct(MPI$file_info$last_time_UTC - cut_time_shift, origin = "1970-01-01"))
#  
#  # Generate start and end time for each day we need to process
#  days_to_process <- seq(first_day, last_day, by = 1)
#  date_range <- lapply(days_to_process, FUN = function(x) {
#    c(
#      "start" = max(MPI$file_info$first_time_UTC, as.numeric(as.POSIXlt(x)) + cut_time_shift),
#      "end" = min(MPI$file_info$last_time_UTC, as.numeric(as.POSIXlt(x + 1)) + cut_time_shift)
#    )
#  })
#  date_range <- data.frame(t(sapply(date_range, c)))
#  date_range$day <- rownames(date_range)
#  date_range$length <- date_range$end - date_range$start
#  # A valid day is at least 5 seconds long
#  date_range <- date_range[date_range$length > 5, ]
#  
#  sample_frequency <- MPI$file_data[["MeasurementFrequency"]]
#  events_list <- list()
#  # Sample, calibrate and aggregate the data day-by-day
#  for (day_number in date_range$day[1]:date_range$day[nrow(date_range)]) {
#    results <- sample_binfile(binfile, binfile_path, output_folder,
#      start_time = date_range[day_number, 1],
#      end_time = date_range[day_number, 2],
#      downsample = FALSE
#    )
#  
#    calibrated <- apply_calibration(results, MPI$auto_calibration, MPI$file_data[["MeasurementDevice"]])
#  
#    calibrated <- apply_AGSA(calibrated)
#  
#    day_transitions <- MPI$transitions[MPI$transitions$day == day_number, "index"]
#    events <- data.frame(
#      "start" = day_transitions[-length(day_transitions)],
#      "end" = floor(sample_frequency * (day_transitions[-1] - 1))
#    )
#    if (length(day_transitions) == 1) {
#      events <- data.frame("start" = day_transitions, "end" = nrow(calibrated))
#    }
#    if (nrow(events) > 1) {
#      events$start[2:nrow(events)] <- events$end[-nrow(events)] + 1
#    }
#    if ((nrow(calibrated) - events$end[length(events$end)]) > sample_frequency) {
#      events <- rbind(events, c(events$end[length(events$end)] + 1, nrow(calibrated)))
#    } else {
#      events$end[length(events$end)] <- nrow(calibrated)
#    }
#    events_agg <- aggregate_events(calibrated,
#      measure = c("x", "y", "z", "AGSA"),
#      time = "TimeUTC",
#      sample_frequency = sample_frequency,
#      events = events,
#      fun = function(x) c(Mean = mean(x), SD = sd(x))
#    )
#    events_list[[day_number]] <- events_agg
#  }
#  
#  # Combine daily aggregated events into a single output
#  events_df <- do.call(rbind, events_list)

