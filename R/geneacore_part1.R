#' GENEAcore Part 1
#'
#' @description GENEAcore function which performs the following tasks:
#' \itemize{
#'  \item Checks and reads the data file and creates the Measurement Period Information (MPI)
#'  \item Downsamples the file to 1Hz and detects periods of non movement
#'  \item Calculates auto calibration parameters for the device
#' }
#'
#' @param data_folder Folder that contains raw data bin files to process or path to single bin file.
#' @param control Named list of optional settings for geneacore functions.
#' @details
#' Control options (all optional):
#' \describe{
#'  \item{cut_time_24hr}{String. Default "15:00". Time in 24h to split days up by.}
#'  \item{timer}{Logical. Default `FALSE`. Print elapsed times of each processing step.}
#' }
#'
#' @examples
#' \dontrun{
#' controls <- list(output_csv = TRUE, required_processing_hours = 4, timer = TRUE)
#' geneacore_part1("path/to/folder", controls)
#' }
#'
#' @returns Measurement Period Information (.rds, .json) and 1Hz downsampled data (.rds)
#' @export
geneacore_part1 <- function(data_folder = data_folder, control = list()) {
  defaults <- list(
    cut_time_24hr = "15:00",
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

        message(paste0("Processing file ", seq, "/", total_files, " : ", data_files[seq]))

        ## Pre-processing
        # Open file connection and read file
        elapsed_p1 <- system.time({
          con <- file(binfile_path, "r")
          binfile <- readLines(con, skipNul = TRUE)
          close(con)
        })
        if (ctrl$timer) print(paste0("File read time: ", round(elapsed_p1["user.self"] + elapsed_p1["sys.self"], 2), " seconds"))

        # Create MPI
        elapsed_p2 <- system.time({
          MPI <- create_MPI(binfile, binfile_path, output_folder)
        })
        if (ctrl$timer) print(paste0("MPI creation time: ", round(elapsed_p2["user.self"] + elapsed_p2["sys.self"], 2), " seconds"))

        if (all(is.na(MPI))) {
          warning(paste0(basename(binfile_path), ": Not a valid GENEActiv bin file. MPI could not be created."), call. = FALSE)
          next
        }

        # Check MPI before further processing
        mpi_summary <- MPI_summary(MPI, FALSE)
        if (apply(mpi_summary[setdiff(names(mpi_summary), c("BinfileName", "Errors"))], 1, function(x) all(is.na(x)))) {
          next
        }

        # Downsample file and detect non-movement
        elapsed_p3 <- system.time({
          MPI <- detect_nonmovement(binfile, binfile_path, output_folder)
        })
        if (ctrl$timer) print(paste0("Non-movement detection time: ", round(elapsed_p3["user.self"] + elapsed_p3["sys.self"], 2), " seconds"))

        # Calculate auto-calibration parameters
        elapsed_p4 <- system.time({
          MPI <- calc_autocalparams(
            binfile, binfile_path, output_folder,
            MPI$non_movement$sphere_points
          )
        })
        if (ctrl$timer) print(paste0("Auto-calibration calculation time: ", round(elapsed_p4["user.self"] + elapsed_p4["sys.self"], 2), " seconds"))

        cut_time_24hr <- check_time_format(ctrl$cut_time_24hr)
        MPI$file_data[["CutTime24Hr"]] <- cut_time_24hr
        cut_times <- get_cut_times(cut_time_24hr, MPI)
        MPI$file_data[["NumberDays"]] <- length(cut_times) - 1

        # Calculate rest intervals
        elapsed_p5 <- system.time({
          MPI <- find_rest_intervals(26, 13, MPI$file_data$CutTime24Hr, MPI)
        })
        if (ctrl$timer) print(paste0("Rest intervals calculation time: ", round(elapsed_p5["user.self"] + elapsed_p5["sys.self"], 2), " seconds"))

        mpi_filepath <- file.path(output_folder, paste0(MPI$file_data$UniqueBinFileIdentifier, "_MPI.rds"))
        saveRDS(MPI, mpi_filepath)
        MPI_toJSON(MPI, mpi_filepath)
      },
      error = function(e) {
        warning(paste("Error processing", binfile_path, ",\n", e))
      }
    )
  }
}
