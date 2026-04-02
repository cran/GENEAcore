## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval = FALSE-------------------------------------------------------------
# vignette("breaking-changes", package = "GENEAcore")

## ----eval = FALSE-------------------------------------------------------------
# browseURL(system.file("extdata", "GENEAcore_Data_Dictionary_2Apr26.pdf", package = "GENEAcore"))

## ----Installing from CRAN, eval = FALSE---------------------------------------
# install.packages("GENEAcore", dependencies = TRUE)

## ----Installing from source, eval = FALSE-------------------------------------
# # Note that R only uses forward slashes / as a file path separator
# install.packages("changepoint")
# install.packages("signal")
# install.packages("jsonlite")
# install.packages("C:/path/to/GENEAcore_1.1.1.tar.gz", repos = NULL, type = "source")

## ----Loading in the GENEAcore library, eval = FALSE---------------------------
# library(GENEAcore)
# library(changepoint)
# library(signal)
# library(jsonlite)

## ----Running geneacore, eval = FALSE------------------------------------------
# library(GENEAcore)
# geneacore(data_folder = "C:/path/to/datafolder")

## ----Running geneacore with parameters, eval = FALSE--------------------------
# library(GENEAcore)
# 
# controls_list <- list(
#   output_epochs = TRUE,
#   epoch_duration = 600, # 10 minutes
#   output_events = FALSE,
#   output_csv = TRUE,
#   required_processing_hours = 4)
# 
# geneacore(
#   data_folder = "C:/path/to/datafolder",
#   control = controls_list
# )

## ----Bin file summary, eval = FALSE-------------------------------------------
# # Run summary for a single bin file
# binfile_summary <- binfile_summary("C:/path/to/binfile.bin")

## ----Bin file folder summary, eval = FALSE------------------------------------
# # Run summary for all bin files in bin files folder only
# binfile_folder_summary <- binfile_summary("C:/path/to/binfilesfolder", recursive = FALSE)

## ----MPI summary, eval = FALSE------------------------------------------------
# # Run summary for single MPI file
# mpi_summary <- MPI_summary("C:/path/to/MPI.rds")

## ----MPI folder summary, eval = FALSE-----------------------------------------
# # Run summary for all MPI files in a folder
# mpi_folder_summary <- MPI_summary("C:/path/to/MPIfolder")

## ----File preparation before individual runs, eval = FALSE--------------------
# binfile_path <- "C:/path/to/binfile"
# output_folder <- "C:/path/to/outputfolder"
# 
# con <- file(binfile_path, "r")
# binfile <- readLines(con, skipNul = TRUE)
# close(con)

## ----Create MPI, eval = FALSE-------------------------------------------------
# MPI <- create_MPI(binfile, binfile_path, output_folder)

## ----Downsampling a file default, eval = FALSE--------------------------------
# # Simple run using default parameter values
# downsampled_measurements <- sample_binfile(binfile,
#                                            binfile_path,
#                                            output_folder)

## ----Downsampling a file with parameters, eval = FALSE------------------------
# # Exposed parameters can be changed
# downsampled_measurements <- sample_binfile(
#   binfile,
#   binfile_path,
#   output_folder,
#   start_time = NULL,
#   end_time = NULL,
#   output_csv = FALSE
# )

## ----Raw sampling a file default, eval = FALSE--------------------------------
# # Simple run using default parameter values
# raw_measurements <- sample_binfile(binfile,
#                                    binfile_path,
#                                    output_folder,
#                                    downsample = FALSE)

## ----Raw sampling a file with parameters, eval = FALSE------------------------
# # Exposed parameters can be changed
# raw_measurements <- sample_binfile(
#   binfile,
#   binfile_path,
#   output_folder,
#   start_time = NULL,
#   end_time = NULL,
#   downsample = FALSE,
#   output_csv = FALSE,
#   save_raw = FALSE
# )

## ----Auto calibration default, eval = FALSE-----------------------------------
# ## Two steps in obtaining auto calibration parameters:
# 
# # 1. Identify non-movement periods
# MPI <- detect_nonmovement(binfile, binfile_path, output_folder)
# 
# # 2. Calculate auto-calibration parameters, temperature compensation TRUE by default
# MPI <- calc_autocalparams(binfile, binfile_path, output_folder, MPI$non_movement$sphere_points)

## ----Calibrating your data parameters, eval = FALSE---------------------------
# # Detect non-movement
# MPI <- detect_nonmovement(
#   binfile,
#   binfile_path,
#   output_folder,
#   still_seconds = 120,
#   sd_threshold = 0.013,
#   temp_seconds = 240,
#   border_seconds = 300,
#   long_still_seconds = 120 * 60,
#   delta_temp_threshold = -0.7,
#   posture_changes_max = 2,
#   non_move_duration_max = 12 * 60 * 60
# )
# 
# # Calculate auto-calibration parameters
# MPI <- calc_autocalparams(
#   binfile,
#   binfile_path,
#   output_folder,
#   MPI$non_movement$sphere_points,
#   use_temp = TRUE,
#   spherecrit = 0.3,
#   maxiter = 500,
#   tol = 1e-13
# )

## ----Apply calibration, eval = FALSE------------------------------------------
# # Sample data
# raw_measurements <- sample_binfile(binfile, binfile_path, output_folder, downsample = FALSE)
# 
# # Apply factory calibration
# calibrated_factory <- apply_calibration(raw_measurements, MPI$factory_calibration, MPI$file_data[["MeasurementDevice"]])
# 
# # Apply auto calibration
# calibrated_auto <- apply_calibration(raw_measurements, MPI$auto_calibration, MPI$file_data[["MeasurementDevice"]])

## ----Detect transitions for event aggregation default, eval = FALSE-----------
# transitions <- detect_transitions(measurements)

## ----Detect transitions for event aggregation parameters, eval = FALSE--------
# MPI <- detect_transitions(
#   measurements,
#   minimum_event_duration = 5,
#   x_cpt_penalty = 18,
#   y_cpt_penalty = 25,
#   z_cpt_penalty = 16,
#   cut_time_24hr = "15:00"
# )

## ----Calculate transitions from bouts output, eval = FALSE--------------------
# bouts <- readRDS("path/to/dayX_bouts.rds")
# transitions <- data.frame(time_UTC = c(bouts$TimeUTC,
#                                        bouts$TimeUTC[nrow(bouts)] + bouts$Duration[nrow(bouts)]),
#                           index = c(1, cumsum(bouts$Duration) + 1))

## ----Applying calculations on calibrated data, eval = FALSE-------------------
# # To apply one measure calculations
# calibrated_measure <- apply_AGSA(calibrated)
# 
# # To apply multiple on the same data set
# calibrated_measures <- apply_degrees(
#   apply_updown(
#     apply_AGSA(
#       apply_ENMO(calibrated)
#     )
#   )
# )

## ----Get events, eval = FALSE-------------------------------------------------
# events <- get_events(calibrated, transitions, sample_frequency)

## ----Aggregating events, eval = FALSE-----------------------------------------
# events_agg <- aggregate_events(
#   calibrated,
#   measure = c("x", "y", "z", "AGSA"),
#   time = "TimeUTC",
#   sample_frequency = sample_frequency,
#   events = events,
#   fun = function(x) c(Mean = mean(x), SD = sd(x))
# )

## ----Aggregating epochs, eval = FALSE-----------------------------------------
# epochs_agg <- aggregate_epochs(calibrated,
#   duration = 1,
#   measure = c("x", "y", "z", "AGSA", "ENMO"),
#   time = "TimeUTC",
#   sample_frequency = MPI$file_data[["MeasurementFrequency"]],
#   fun = function(x) c(Mean = mean(x), SD = sd(x))
# )

## ----Running geneabout, eval = FALSE------------------------------------------
# library(GENEAcore)
# geneabout(data_folder = "/path/to/datafolder")

## ----Running geneabout with existing parameters, eval = FALSE-----------------
# library(GENEAcore)
# 
# controls_list <- list(
#   output_epochs = TRUE,
#   epoch_duration = 600,
#   output_events = FALSE,
#   output_csv = TRUE,
#   required_processing_hours = 4)
# 
# geneabout(
#   data_folder = "C:/path/to/datafolder",
#   control = controls_list
# )

## ----Running geneabout with new parameters, eval = FALSE----------------------
# library(GENEAcore)
# 
# controls_list <- list(
#   output_epochs = TRUE,
#   epoch_duration = 600,
#   output_events = FALSE,
#   output_csv = TRUE,
#   required_processing_hours = 4,
#   save_daily_bouts = TRUE,
#   identifier_mapping_record = "C:/path/to/identifier-mapping-record.csv"
#   )
# 
# geneabout(
#   data_folder = "C:/path/to/datafolder",
#   control = controls_list
# )

## ----Find rest intervals, eval = FALSE----------------------------------------
# MPI <- find_rest_intervals(start_expansion_percent = 26,
#                            end_expansion_percent = 13,
#                            cut_time_24hr = "15:00",
#                            MPI)

## ----Non wear rest coverage, eval = FALSE-------------------------------------
# # Date range must define the day boundaries using the chosen 24-hour cut time.
# date_range <- data.frame(
#   start = c(1761829400, 1761832800, 1761919200),
#   end = c(1761832800, 1761919200, 1761938195),
#   length = c(83000, 86400, 18995),
#   day = c(1, 2, 3)
# )
# 
# nonwear_by_day <- valid_day_nonwear(
#   MPI_nonwear = MPI[["non_movement"]][["non_wear"]],
#   date_range = date_range,
#   required_processing_hours = 0,
#   cut_time_24hr = "15:00"
# )
# 
# events_df <- nonwear_rest_coverage(
#   MPI = MPI,
#   aggregated_data = events_df,
#   start_time = NULL, # If NULL, the first timestamp in the aggregated data is used
#   end_time = NULL, # If NULL, the timestamp at the end of the final event or epoch is used
#   nonwear_day = nonwear_by_day[["nonwear_day"]]
# )

## ----Bouts decision tree, eval = FALSE----------------------------------------
# bouts <- bouts_decision_tree(events_df,
#                              SDduration_threshold = 5.7e-5,
#                              AGSA_threshold = 0.0625,
#                              running_threshold = 0.407)

## ----geneacore flowchart, echo=FALSE, out.extra='style="border:1px solid #ccc; padding:4px;"'----
knitr::include_graphics("../inst/extdata/geneacore_functions.png")

## ----geneabout flowchart, echo=FALSE, out.width="70%", out.extra='style="border:1px solid #ccc; padding:4px;"'----
knitr::include_graphics("../inst/extdata/geneabout_functions.png")

## ----Loop functions for folder, eval=FALSE------------------------------------
# data_folder <- "C:/path/to/folder"
# data_files <- (list.files(data_folder, pattern = "(?i)\\.bin$"))
# 
# for (seq in 1:length(data_files)) {
#   binfile_path <- file.path(data_folder, data_files[seq])
#   project <- gsub("\\.bin", "", basename(binfile_path))
#   output_folder <- file.path(data_folder, project)
#   if (!file.exists(output_folder)) {
#     dir.create(output_folder)
#   }
#   # Open file connection and read file
#   con <- file(binfile_path, "r")
#   binfile <- readLines(con, skipNul = TRUE)
#   close(con)
#   # Create MPI
#   MPI <- create_MPI(binfile, binfile_path, output_folder)
#   # Downsample file and detect non-movement
#   MPI <- detect_nonmovement(binfile, binfile_path, output_folder)
#   # Calculate auto-calibration parameters
#   MPI <- calc_autocalparams(
#     binfile, binfile_path, output_folder,
#     MPI$non_movement$sphere_points
#   )
# }

## ----Aggregating events day by day, eval = FALSE------------------------------
# # Prepare time borders of each day
# cut_times <- get_cut_times(MPI$file_data[["CutTime24Hr"]], MPI)
# date_range <- data.frame(start = cut_times[1:(length(cut_times) - 1)], end = cut_times[2:(length(cut_times))])
# date_range$day <- rownames(date_range)
# date_range$length <- date_range$end - date_range$start
# date_range <- date_range[date_range$length > 5, ]
# 
# # Read in downsampled measurement for transition detection
# downsampled_measurements <- readRDS(file.path(output_folder, paste0(UniqueBinFileIdentifier, "_downsample.rds")))
# 
# sample_frequency <- MPI$file_data[["MeasurementFrequency"]]
# 
# for (day_number in nrow(date_range)) {
#   steps_epochs_df <- data.frame()
#   steps_events_df <- data.frame()
# 
#   message(paste("Processing Day", day_number, "of", nrow(date_range)))
#   results <- readRDS(file.path(
#     output_folder,
#     paste0(UniqueBinFileIdentifier, "_", date_range$start[day_number], "_", date_range$end[day_number], "_rawdata.rds")
#   ))
# 
#   # Apply auto calibration and other calculated measures
#   if ("auto_calibration" %in% names(MPI)) {
#     calibrated <- apply_calibration(results, MPI$auto_calibration, MPI$file_data[["MeasurementDevice"]])
#   } else {
#     calibrated <- apply_calibration(results, MPI$factory_calibration, MPI$file_data[["MeasurementDevice"]])
#   }
# 
#   calibrated <- apply_all(calibrated) # apply_all calculates AGSA, ENMO, UpDown and Degrees
# 
#   day_measurements <- subset(
#     downsampled_measurements,
#     TimeUTC >= date_range$start[day_number] & TimeUTC < date_range$end[day_number]
#   )
#   day_transitions <- detect_transitions(day_measurements, cut_time_24hr = MPI$file_data[["CutTime24Hr"]])
#   events <- get_events(calibrated, day_transitions, sample_frequency)
# 
#   events_df <- aggregate_events(calibrated,
#     measure = c("x", "y", "z", "Light", "Temp", "AGSA", "ENMO", "UpDown", "Degrees"),
#     time = "TimeUTC",
#     sample_frequency = sample_frequency,
#     events = events,
#     fun = function(x) c(Mean = mean(x), Max = max(x), SD = sd(x))
#   )
#   events_df$DayNumber <- rep(day_number, nrow(events_df))
# 
#   # Step Counter
#   for (eventnumber in seq_len(nrow(events))) {
#     steps <- step_counter(calibrated[(events$start[eventnumber]:events$end[eventnumber]), "y"],
#       sample_frequency = sample_frequency
#     )
#     steps_events_df <- rbind(steps_events_df, steps)
#   }
#   colnames(steps_events_df) <- c("StepCount", "StepMean", "StepSD", "StepDiff")
#   events_df <- cbind(events_df, steps_events_df)
# 
#   if (!is.null(events_df)) {
#     events_df <- reorder_df(events_df)
#     events_df <- nonwear_rest_coverage(MPI, events_df, date_range$start[day_number], date_range$end[day_number], nonwear_by_day$nonwear_day)
#     output_location <- file.path(output_folder, paste0(MPI$file_data[["UniqueBinFileIdentifier"]], "_day", day_number, "_events"))
#     saveRDS(events_df, paste0(output_location, ".rds"))
#     write.csv(round_columns(events_df), file = paste0(output_location, ".csv"), row.names = FALSE)
#   }
# }

