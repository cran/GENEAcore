#' Sample Bin File
#'
#' @details Function to read in a GENEActiv bin file with option to downsample
#' to 1Hz. This can read the whole of the file or just a portion of it by
#' setting the start_time and end_time parameters.
#' @param binfile Text lines read from an open connection to a bin file.
#' @param binfile_path Path to the bin file to be processed.
#' @param output_folder Path to the folder containing GENEAcore run outputs and Measurement Period Information (MPI) files.
#' @param start_time Time stamp to start the read from, default start of file.
#' @param end_time Time stamp to end the read from, default end of file.
#' @param downsample Logical to determine whether to downsample the file, default TRUE.
#' @param output_csv Allow outputs of bin file sampling to be saved as CSV.
#' @param save_raw Save daily raw sampled data as RDS to for quicker reprocessing, default FALSE.
#' @return List of 1Hz downsampled data or raw sample data.
#' @importFrom utils capture.output
#' @export
#' @examples
#' binfile_path <- system.file("extdata/20Hz_file.bin", package = "GENEAcore")
#' output_folder <- tempdir()
#' con <- file(binfile_path, "r")
#' binfile <- readLines(con, skipNul = TRUE)
#' close(con)
#' measurements <- sample_binfile(binfile, binfile_path, output_folder)
sample_binfile <- function(binfile,
                           binfile_path,
                           output_folder,
                           start_time = NULL,
                           end_time = NULL,
                           downsample = TRUE,
                           output_csv = FALSE,
                           save_raw = FALSE) {
  NUMOBS <- 300
  CHAROBS <- 12
  PAGELENGTH <- 10

  # Get UniqueBinFileIdentifier
  UniqueBinFileIdentifier <- get_UniqueBinFileIdentifier(binfile)

  if (!is.na(UniqueBinFileIdentifier)) {
    if (downsample) {
      # Check if downsample data file already exists
      file_pattern <- paste0(UniqueBinFileIdentifier, "_downsample.rds")
      files <- list.files(path = output_folder, pattern = file_pattern, recursive = TRUE)
      if (length(files) != 0) {
        measurements <- readRDS(file.path(output_folder, files[1]))
        warning(paste(basename(binfile_path), ": Downsample data already exists."))
        return(measurements)
      }
    } else {
      # Check if raw data file already exists
      file_pattern <- paste0(UniqueBinFileIdentifier, "_", start_time, "_", end_time, "_rawdata.rds")
      files <- list.files(path = output_folder, pattern = file_pattern, recursive = TRUE)
      if (length(files) != 0) {
        measurements <- readRDS(file.path(output_folder, files[1]))
        # warning(paste(basename(binfile_path), ": Raw data already exists."))
        return(measurements)
      }
    }
    # check if MPI file already exists and create if not
    MPI_filepath <- file.path(output_folder, paste0(UniqueBinFileIdentifier, "_MPI.rds"))
    if (file.exists(MPI_filepath)) {
      MPI <- readRDS(MPI_filepath)
    } else {
      MPI <- create_MPI(binfile, binfile_path, output_folder)
    }

    if (is.null(start_time)) start_time <- MPI$file_info$first_time_UTC
    if (is.null(end_time)) end_time <- MPI$file_info$last_time_UTC

    if (downsample) {
      # Define measurements needed by index (1s downsample first element)
      meas_index <- c(0, seq(
        MPI$file_data[["MeasurementFrequency"]],
        MPI$file_info[["number_measurements"]] - MPI$file_data[["MeasurementFrequency"]],
        MPI$file_data[["MeasurementFrequency"]]
      ))
    } else {
      # Sample raw data file
      start_index <- as.numeric(round((start_time - MPI$file_info[["first_time_UTC"]]) *
        MPI$file_data[["MeasurementFrequency"]]))
      end_index <- as.numeric(round((end_time - MPI$file_info[["first_time_UTC"]]) *
        MPI$file_data[["MeasurementFrequency"]]))
      output <- capture.output(
        {
          message(paste0(
            "Reading from ",
            format(as.POSIXlt(start_time, tz = "UTC", origin = "1970-01-01"), "%d/%m/%Y %H:%M:%S %z"), " to ",
            format(as.POSIXlt(end_time, tz = "UTC", origin = "1970-01-01"), "%d/%m/%Y %H:%M:%S %z"),
            " Indexes(", start_index, ":", end_index, ")"
          ))
        },
        type = "message"
      )
      cat(output, "\n")
      meas_index <- seq(start_index, end_index - 1, 1)
    }
    # Create timestamps for measurements needed
    measurements <- data.frame(meas_index)
    measurements$TimeUTC <- floor(MPI$file_info[["first_time_UTC"]] +
      (measurements$meas_index) / MPI$file_data[["MeasurementFrequency"]])

    # Round added to deal with floating point maths issues
    measurements$meas_index <- round(meas_index, 0)

    # Add measurement offset for 0.5s start files
    measurements$meas_index <- measurements$meas_index + MPI$measurement_numbers[["first_second"]]

    # Create line and position numbers for measurements needed
    measurements$line <- PAGELENGTH * floor((measurements$meas_index - 1) / NUMOBS) + MPI$line_numbers["first_page"] + 9
    measurements$position <- CHAROBS * round(NUMOBS * ((measurements$meas_index - 1) / NUMOBS - floor((measurements$meas_index - 1) / NUMOBS)), 0) + 1

    # Read lines and extract measurements blocks from positions
    measurements$meas_block <- substr(binfile[measurements$line], measurements$position, measurements$position + 11)

    # Extract measurements from blocks (hex to int and then 2s comp convert)
    measurements$x <- strtoi(substr(measurements$meas_block, 1, 3), base = 16L)
    measurements$x <- ifelse(measurements$x > (2^11 - 1), measurements$x - 2^CHAROBS, measurements$x)
    measurements$x <- measurements$x / 256

    measurements$y <- strtoi(substr(measurements$meas_block, 4, 6), base = 16L)
    measurements$y <- ifelse(measurements$y > (2^11 - 1), measurements$y - 2^CHAROBS, measurements$y)
    measurements$y <- measurements$y / 256

    measurements$z <- strtoi(substr(measurements$meas_block, 7, 9), base = 16L)
    measurements$z <- ifelse(measurements$z > (2^11 - 1), measurements$z - 2^CHAROBS, measurements$z)
    measurements$z <- measurements$z / 256

    # Last bit is zero so divide by 2
    button_light <- strtoi(substr(measurements$meas_block, CHAROBS, CHAROBS), base = 16L) / 2

    # Add the remainder of last byte with button remove to rest of light signal
    measurements$Light <- strtoi(substr(measurements$meas_block, CHAROBS - 2, CHAROBS - 1), base = 16L) * 2^2 + floor(button_light / 2)

    # Last bit is now button press so just check odd/even
    measurements$Button <- ifelse((button_light %% 2) == 0, 0, 1)

    # Extract temp & volts
    measurements$Temp <- as.numeric(sub(".*:", "", binfile[measurements$line - 4]))
    measurements$Volts <- as.numeric(sub(".*:", "", binfile[measurements$line - 3]))

    # Tidy up
    measurements <- subset(measurements, select = -c(meas_index, line, position, meas_block))

    # Update MPI and save downsampled output
    if (downsample) {
      MPI$file_history <- rbind(MPI$file_history, paste0(substr(Sys.time(), 0, 23), " bin file downsampled"))
      saveRDS(measurements, file.path(output_folder, paste0(UniqueBinFileIdentifier, "_downsample.rds")))
    } else {
      if (save_raw) {
        saveRDS(measurements, file.path(
          output_folder,
          paste0(UniqueBinFileIdentifier, "_", start_time, "_", end_time, "_rawdata.rds")
        ))
      }
    }
    saveRDS(MPI, file.path(output_folder, paste0(UniqueBinFileIdentifier, "_MPI.rds")))
    if (output_csv == TRUE) {
      if (downsample) {
        csvfilename <- file.path(output_folder, paste0(UniqueBinFileIdentifier, "_downsample.csv"))
      }
      #   else {
      #    csvfilename <- file.path(output_folder, paste0(UniqueBinFileIdentifier, "_rawdata.csv"))
      # }
      write.csv(measurements, csvfilename)
    }

    return(measurements)
  } else {
    warning(paste(basename(binfile_path), ": Unable to generate UniqueBinFileIdentifier."))
    measurements <- NA
    return(measurements)
  }
}
