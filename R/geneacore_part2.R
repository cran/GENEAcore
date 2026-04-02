#' GENEAcore Part 2
#'
#' @description GENEAcore function which performs the following tasks for each valid day:
#' \itemize{
#'  \item Samples daily raw bin file data and saves as daily RDS files
#'  \item Reads and saves button press timestamps to the Measurement Period Information (MPI)
#' }
#'
#' @param data_folder Folder that contains raw data bin files to process or path to single bin file.
#' @param control Named list of optional settings for geneacore functions.
#' @details
#' Control options (all optional):
#' \describe{
#'  \item{required_processing_hours}{Integer. Default 0. Hours of wear time in a 24-hour day to be considered a valid day to be processed.}
#'  \item{timer}{Logical. Default `FALSE`. Print elapsed times of each processing step.}
#' }
#'
#' @examples
#' \dontrun{
#' controls <- list(output_csv = TRUE, required_processing_hours = 4, timer = TRUE)
#' geneacore_part2("path/to/folder", controls)
#' }
#'
#' @returns Daily raw data sample of x, y, z, Light, Button, Temperature and Voltage (.rds)
#' @export
geneacore_part2 <- function(data_folder = data_folder, control = list()) {
  defaults <- list(
    required_processing_hours = 0,
    timer = FALSE
  )
  ctrl <- merge_control(defaults, control)

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
  total_files <- length(data_files)
  for (seq in 1:total_files) {
    tryCatch(
      {
        binfile_path <- file.path(data_folder, data_files[seq])
        project <- gsub("\\.bin", "", basename(binfile_path))
        output_folder <- file.path(data_folder, project)

        if (file.exists(output_folder)) {
          mpi_summary <- MPI_summary(output_folder)

          if (!apply(mpi_summary[setdiff(names(mpi_summary), c("BinfileName", "Errors"))], 1, function(x) all(is.na(x)))) {
            if (!mpi_summary$MeasurementDurationActual < 5) {
              UniqueBinFileIdentifier <- mpi_summary$UniqueBinFileIdentifier[mpi_summary$BinfileName == data_files[seq]]

              mpi_filepath <- file.path(output_folder, paste0(UniqueBinFileIdentifier, "_MPI.rds"))
              MPI <- readRDS(mpi_filepath)
              message(paste0("Processing file ", seq, "/", total_files, " : ", data_files[seq]))

              # Open file connection and read file
              elapsed_p1 <- system.time({
                con <- file(binfile_path, "r")
                binfile <- readLines(con, skipNul = TRUE)
                close(con)
              })
              if (ctrl$timer) print(paste0("File read time: ", round(elapsed_p1["user.self"] + elapsed_p1["sys.self"], 2), " seconds"))

              cut_time_24hr <- MPI$file_data[["CutTime24Hr"]]
              cut_times <- get_cut_times(cut_time_24hr, MPI)

              date_range <- data.frame(start = cut_times[1:(length(cut_times) - 1)], end = cut_times[2:(length(cut_times))])
              date_range$length <- date_range$end - date_range$start
              date_range <- date_range[date_range$length > 5, ]
              date_range$day <- rownames(date_range)

              nonwear_by_day <- valid_day_nonwear(MPI[["non_movement"]][["non_wear"]], date_range, ctrl$required_processing_hours, cut_time_24hr)
              valid_days <- as.integer(date_range$day[nonwear_by_day$valid_day$valid_day])

              if (length(valid_days) == 0) {
                message("No valid days found.")
                next
              }

              load_time <- system.time({
                for (day_number in date_range$day[1]:date_range$day[nrow(date_range)]) {
                  if (nonwear_by_day$valid_day$valid_day[nonwear_by_day$valid_day$day == day_number]) {
                    message(paste("Processing Day", day_number, "of", nrow(date_range)))
                    elapsed_res <- system.time({
                      results <- sample_binfile(binfile, binfile_path, output_folder,
                        start_time = date_range[day_number, 1],
                        end_time = date_range[day_number, 2],
                        downsample = FALSE, save_raw = TRUE
                      )
                    })

                    # Add button press timestamps to MPI
                    MPI$button <- sort(unique(c(MPI$button, results$TimeUTC[results$Button > 0])))
                  } else {
                    message(paste("Day", day_number, "of", nrow(date_range), " is not a valid day and will not be processed"))
                  }
                }
              })
              saveRDS(MPI, mpi_filepath)
              MPI_toJSON(MPI, mpi_filepath)
              # if (ctrl$timer) print(paste0("Total raw data read time: ", round(load_time["user.self"] + load_time["sys.self"], 2), " seconds"))
              # message("Total wall-clock time: ", round(load_time["elapsed"], 2), " seconds")
            } else {
              warning(paste0(basename(binfile_path), ": Measurement duration is less than the minimum duration of 5 seconds. File not processed further."), call. = FALSE)
              next
            }
          } else {
            warning(paste0(basename(binfile_path), ": File could not be processed. See MPI for error details."), call. = FALSE)
          }
        }
      },
      error = function(e) {
        warning(paste("Error processing", binfile_path, ",\n", e))
      }
    )
  }
}
