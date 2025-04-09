#' Generate Unique Bin File Identifier
#'
#' @details Function to create a UniqueBinFileIdentifier from a GENEActiv bin file.
#' @param binfile Text lines read from an open connection to a bin file.
#' @return Single string identifier.
#' @export
#' @examples
#' binfile_path <- system.file("inst/extdata/20Hz_file.bin", package = "GENEAcore")
#' con <- file(binfile_path, "r")
#' binfile <- readLines(con, skipNul = TRUE)
#' close(con)
#' UniqueBinFileIdentifier <- get_UniqueBinFileIdentifier(binfile)
get_UniqueBinFileIdentifier <- function(binfile) {
  # If device is reset, use start time. Else use config time.
  MeasurementDevice <- paste0(trimws(sub("Device Type:", "", binfile[3])))
  FileLength <- length(binfile)
  if ((MeasurementDevice == "GENEActiv") & (FileLength > 31)) {
    MeasurementDeviceID <- (sub("Device Unique Serial Code:", "", binfile[2]))
    ConfigTime <- (sub("Config Time:", "", binfile[31]))
    if (ConfigTime == "2010-09-16 09:08:54:000") {
      if (FileLength > 70) {
        start_time <- sub("Page Time:", "", binfile[min(which(binfile == "Recorded Data")) + 3])
        start_time <- as.POSIXct(start_time, format = "%Y-%m-%d %H:%M:%OS", origin = "1970-01-01", tz = "GMT")
        start_time <- as.numeric(start_time)
        UniqueBinFileIdentifier <- paste0(
          MeasurementDeviceID, "_",
          start_time, "_",
          "1_",
          FileLength
        )
      } else {
        warning("Not a valid GENEActiv bin file.")
        UniqueBinFileIdentifier <- NA
      }
    } else {
      ConfigTime <- as.POSIXct(ConfigTime, format = "%Y-%m-%d %H:%M:%OS", origin = "1970-01-01", tz = "GMT")
      ConfigTime <- as.numeric(ConfigTime)
      UniqueBinFileIdentifier <- paste0(
        MeasurementDeviceID, "_",
        ConfigTime, "_",
        "1_",
        FileLength
      )
    }
  } else {
    warning("Not a valid GENEActiv bin file.")
    UniqueBinFileIdentifier <- NA
  }
  return(UniqueBinFileIdentifier)
}

#' Create Measurement Period Information
#'
#' @details Function to create measurement period information (MPI) from a GENEActiv bin file
#' @param binfile Text lines read from an open connection to a bin file.
#' @param binfile_path Path to the bin file to be processed.
#' @param output_folder Folder to write MPI file in.
#' @param out_rds Allows RDS output to be saved during MPI creation.
#' @return List of measurement period information.
#' @export
#' @examples
#' binfile_path <- system.file("inst/extdata/20Hz_file.bin", package = "GENEAcore")
#' con <- file(binfile_path, "r")
#' binfile <- readLines(con, skipNul = TRUE)
#' close(con)
#' MPI <- create_MPI(binfile)
create_MPI <- function(binfile, binfile_path, output_folder, out_rds = TRUE) {
  NUMOBS <- 300
  CHAROBS <- 12
  PAGELENGTH <- 10

  # get UniqueBinFileIdentifier
  UniqueBinFileIdentifier <- get_UniqueBinFileIdentifier(binfile)

  if (!is.na(UniqueBinFileIdentifier)) {
    # check if MPI file already exists and create if not
    MPI_filepath <- file.path(output_folder, paste0(UniqueBinFileIdentifier, "_MPI.rds"))
    if (file.exists(MPI_filepath)) {
      MPI <- readRDS(MPI_filepath)
      warning(paste(basename(binfile_path), ": MPI already exists."))
      return(MPI)
    }

    # set up output objects
    errors <- vector("list", 0)
    file_history <- vector("list", 0)
    line_numbers <- vector("integer", 7)
    names(line_numbers) <- c(
      "extractinfo",
      "subjectinfo",
      "calibrationdata",
      "memorystatus",
      "firstpage",
      "lastline",
      "lastpage"
    )
    measurement_numbers <- vector("integer", 5)
    names(measurement_numbers) <- c(
      "firstsecond",
      "firstminute",
      "firsthour",
      "firstUTCday",
      "firstlocalday"
    )
    file_info <- data.frame(matrix(ncol = 6, nrow = 1))
    colnames(file_info) <- c(
      "pagecount",
      "decimalseparator",
      "firsttimestamp",
      "lasttimestamp",
      "halfsecondstart",
      "numbermeasurements"
    )
    file_data <- data.frame(matrix(ncol = 42, nrow = 1))
    colnames(file_data) <- c(
      "UniqueBinFileIdentifier",
      "BinfileName",
      "ClockDrift",
      "ClockDriftDay",
      "ConfigInvestigatorID",
      "ConfigNotes",
      "ConfigTime",
      "ConfigTimeISO",
      "ExtractInvestigatorID",
      "ExtractNotes",
      "ExtractTime",
      "ExtractTimeISO",
      "FileLength",
      "FirstLocalMidnightTime",
      "MeasurementDevice",
      "MeasurementDeviceCalibrationDate",
      "MeasurementDeviceFirmwareVersion",
      "MeasurementDeviceID",
      "MeasurementDurationActual",
      "MeasurementDurationSet",
      "MeasurementEndTime",
      "MeasurementEndTimeISO",
      "MeasurementFrequency",
      "MeasurementStartDate",
      "MeasurementStartTime",
      "MeasurementStartTimeISO",
      "SiteID",
      "StudyID",
      "ParticipantDoB",
      "ParticipantHandedness",
      "ParticipantHeight",
      "ParticipantID",
      "ParticipantNotes",
      "ParticipantSex",
      "ParticipantWeight",
      "PrincipalInvestigator",
      "PeriodInfo",
      "TimeZone",
      "TimeOffset",
      "VoltsEnd",
      "VoltsStart",
      "WearLocationConfig"
    )

    # set up line numbers for bin file elements
    if (length(binfile) >= 69) {
      line_numbers["lastline"] <- length(binfile)
    } else {
      errors <- rbind(errors, "Bin file too short or no data pages.")
      warning(paste(basename(binfile_path), ": Bin file too short or no data pages."))
      MPI <- list(errors = errors)
      return(MPI)
    }
    i <- 0
    extract_check <- substr(binfile[33 + i], 1, 20)
    while (extract_check != "Extract Operator ID:") {
      i <- i + 1
      extract_check <- substr(binfile[33 + i], 1, 20)
    }
    line_numbers["extractinfo"] <- 33 + i
    line_numbers["subjectinfo"] <- which(binfile == "Subject Info")
    line_numbers["calibrationdata"] <- which(binfile == "Calibration Data")
    line_numbers["memorystatus"] <- which(binfile == "Memory Status")
    line_numbers["firstpage"] <- min(which(binfile == "Recorded Data"))
    line_numbers["lastline"] <- length(binfile)

    # check memory status line is present
    if (is.finite(line_numbers["memorystatus"]) & (line_numbers["lastline"] > line_numbers["memorystatus"])) {
      file_info$pagecount <- as.numeric(sub("Number of Pages:", "", binfile[line_numbers["memorystatus"] + 1]))
    } else {
      errors <- rbind(errors, "Bin file header incomplete.")
      warning(paste(basename(binfile_path), ": Bin file header incomplete."))
      MPI <- list(errors = errors)
      return(MPI)
    }

    # check not zero pages
    if (file_info$pagecount == 0) {
      errors <- rbind(errors, "No data in bin file.")
      warning(paste(basename(binfile_path), ": No data in bin file."))
      MPI <- list(errors = errors)
      return(MPI)
    }

    # find first + last data and check file is complete
    if ((line_numbers["lastline"] == file_info$pagecount * PAGELENGTH + line_numbers["firstpage"] - 1) &
      (nchar(binfile[line_numbers["lastline"]]) == NUMOBS * CHAROBS)) {
      ## file complete ##
      line_numbers["lastpage"] <- (file_info$pagecount - 1) * PAGELENGTH + line_numbers["firstpage"]
    } else {
      errors <- rbind(errors, "Bin file is incomplete.")
      warning(paste(basename(binfile_path), ": Bin file is incomplete."))
      file_info$pagecount <- floor((line_numbers["lastline"] - line_numbers["firstpage"] + 1) / PAGELENGTH)
      line_numbers["lastpage"] <- (file_info$pagecount - 1) * PAGELENGTH + line_numbers["firstpage"]
    }

    # check the decimal separator
    file_info$decimalseparator <- "."
    if (length(grep(",", paste(binfile[line_numbers["memorystatus"] + 8:9], collapse = ""))) > 0) {
      file_info$decimalseparator <- ","
    }

    # check GENEActiv model
    file_data$MeasurementDevice <- paste0(
      trimws(sub("Device Type:", "", binfile[3])),
      " ",
      trimws(sub("Device Model:", "", binfile[4]))
    )

    # get measurement frequency from first page
    file_data$MeasurementFrequency <- (sub("Measurement Frequency:", "", binfile[line_numbers["firstpage"] + 8]))
    if (file_info$decimalseparator == ",") {
      file_data$MeasurementFrequency <- sub(",", ".", file_data$MeasurementFrequency, fixed = TRUE)
    }
    file_data$MeasurementFrequency <- as.numeric(file_data$MeasurementFrequency)
    file_data$MeasurementFrequency <- 300 / round(300 / file_data$MeasurementFrequency, 1)

    # get & decode timezone
    file_data$TimeZone <- sub("Time Zone:GMT ", "", binfile[23])
    file_data$TimeOffset <- 60 * 60 * as.numeric(unlist(strsplit(file_data$TimeZone, ":"))[1]) +
      sign(as.numeric(unlist(strsplit(file_data$TimeZone, ":"))[1])) *
        60 * as.numeric(unlist(strsplit(file_data$TimeZone, ":"))[2])

    # get first & second timestamps, checking if it has 0.5s element - may need to come from 2nd page
    first_timestamp <- sub("Page Time:", "", binfile[line_numbers["firstpage"] + 3])
    first_timestamp <- gsub(":", ".", first_timestamp)
    first_timestamp <- as.POSIXct(first_timestamp, format = "%Y-%m-%d %H.%M.%OS", origin = "1970-01-01", tz = "GMT")
    first_timestamp <- as.numeric(first_timestamp)

    if (file_data$MeasurementDevice == "GENEActiv 1.1") {
      if (file_info$pagecount > 1) {
        second_timestamp <- sub("Page Time:", "", binfile[line_numbers["firstpage"] + 13])
        second_timestamp <- gsub(":", ".", second_timestamp)
        second_timestamp <- as.POSIXct(second_timestamp, format = "%Y-%m-%d %H.%M.%OS", origin = "1970-01-01", tz = "GMT")
        second_timestamp <- as.numeric(second_timestamp)
        if ((second_timestamp - first_timestamp) != (NUMOBS / file_data$MeasurementFrequency)) {
          first_timestamp <- second_timestamp - NUMOBS / file_data$MeasurementFrequency
        }
      }
    }

    if ((ceiling(first_timestamp) - first_timestamp) > 0) {
      file_info$halfsecondstart <- TRUE
      file_info$firsttimestamp <- first_timestamp + 0.5 - file_data$TimeOffset
    } else {
      file_info$halfsecondstart <- FALSE
      file_info$firsttimestamp <- first_timestamp - file_data$TimeOffset
    }
    file_data$MeasurementStartTime <- file_info$firsttimestamp
    file_data$MeasurementStartTimeISO <- paste0(gsub(" ", "T", as.character(as.POSIXct((file_info$firsttimestamp + file_data$TimeOffset), origin = "1970-01-01", tz = "GMT"))), file_data$TimeZone)
    file_data$MeasurementStartDate <- format(as.Date(as.POSIXct(file_info$firsttimestamp, origin = "1970-01-01", tz = "GMT")), format = "%d-%b-%y")

    # get last timestamp, checking if it has 0.5s element
    last_timestamp <- sub("Page Time:", "", binfile[line_numbers["lastpage"] + 3])
    last_timestamp <- gsub(":", ".", last_timestamp)
    last_timestamp <- as.POSIXct(last_timestamp, format = "%Y-%m-%d %H.%M.%OS", origin = "1970-01-01", tz = "GMT")
    last_timestamp <- as.numeric(last_timestamp)

    if ((ceiling(last_timestamp) - last_timestamp) > 0) {
      file_info$lasttimestamp <- last_timestamp - file_data$TimeOffset + NUMOBS / file_data$MeasurementFrequency - 0.5
    } else {
      file_info$lasttimestamp <- last_timestamp - file_data$TimeOffset + NUMOBS / file_data$MeasurementFrequency
    }
    file_data$MeasurementEndTime <- file_info$lasttimestamp
    file_data$MeasurementEndTimeISO <- paste0(gsub(" ", "T", as.character(as.POSIXct((file_info$lasttimestamp + file_data$TimeOffset), origin = "1970-01-01", tz = "GMT"))), file_data$TimeZone)

    # check file is contiguous
    if (abs((file_info$lasttimestamp - file_info$firsttimestamp) -
      (file_info$pagecount * NUMOBS / file_data$MeasurementFrequency)) > 1) {
      errors <- rbind(errors, "Bin file is not contiguous.")
      warning(paste(basename(binfile_path), ": Bin file is not contiguous."))
      MPI <- list(errors = errors)
      return(MPI)
    }

    # record the number of measurements available (removing 0.5s starts & ends)
    file_info$numbermeasurements <- round((file_info$lasttimestamp - file_info$firsttimestamp) * file_data$MeasurementFrequency, 0)

    # set up measurement numbers
    if (file_info$halfsecondstart == TRUE) {
      measurement_numbers["firstsecond"] <- as.integer(file_data$MeasurementFrequency * 0.5) + 1
    } else {
      measurement_numbers["firstsecond"] <- 1
    }
    measurement_numbers["firstminute"] <- round((60 * ceiling(file_info$firsttimestamp / 60) - file_info$firsttimestamp) * file_data$MeasurementFrequency, 0) +
      measurement_numbers[["firstsecond"]]
    measurement_numbers["firsthour"] <- round((60 * 60 * ceiling(file_info$firsttimestamp / (60 * 60)) - file_info$firsttimestamp) * file_data$MeasurementFrequency, 0) +
      measurement_numbers[["firstsecond"]]

    UTC_day <- 60 * 60 * 24 * ceiling(file_info$firsttimestamp / (60 * 60 * 24)) - file_info$firsttimestamp
    measurement_numbers["firstUTCday"] <- round(UTC_day * file_data$MeasurementFrequency, 0) + measurement_numbers[["firstsecond"]]
    local_day <- UTC_day - file_data$TimeOffset
    if (local_day >= 60 * 60 * 24) {
      local_day <- local_day - 60 * 60 * 24
    }
    if (local_day < 0) {
      local_day <- local_day + 60 * 60 * 24
    }

    file_data$FirstLocalMidnightTime <- file_info$firsttimestamp + local_day

    measurement_numbers["firstlocalday"] <- round(local_day * file_data$MeasurementFrequency, 0) + measurement_numbers[["firstsecond"]]

    # populate MPI with already calculated parameters
    file_data$UniqueBinFileIdentifier <- UniqueBinFileIdentifier
    file_data$BinfileName <- basename(binfile_path)
    file_data$FileLength <- length(binfile)
    file_data$MeasurementDurationActual <- file_info$lasttimestamp - file_info$firsttimestamp

    # populate rest of MPI from bin file header
    file_data$MeasurementDeviceID <- sub("Device Unique Serial Code:", "", binfile[2])
    file_data$MeasurementDeviceFirmwareVersion <- trimws(sub("Device Firmware Version:", "", binfile[5]))
    file_data$MeasurementDeviceCalibrationDate <- as.POSIXct(sub("Calibration Date:", "", binfile[6]), format = "%Y-%m-%d", origin = "1970-01-01", tz = "GMT")
    file_data$MeasurementDeviceCalibrationDate <- format(file_data$MeasurementDeviceCalibrationDate, format = "%d-%b-%y")
    file_data$MeasurementDurationSet <- 60 * 60 * as.numeric(gsub("[^0-9.-]", "", binfile[21]))
    file_data$SiteID <- trimws(sub("Study Centre:", "", binfile[26]))
    file_data$StudyID <- trimws(sub("Study Code:", "", binfile[27]))
    file_data$PrincipalInvestigator <- trimws(sub("Investigator ID:", "", binfile[28]))
    file_data$PeriodInfo <- trimws(sub("Exercise Type:", "", binfile[29]))
    file_data$ConfigInvestigatorID <- trimws(sub("Config Operator ID:", "", binfile[30]))
    file_data$ConfigTime <- sub("Config Time:", "", binfile[31])
    if (file_data$ConfigTime == "2010-09-16 09:08:54:000") {
      errors <- rbind(errors, "Config data overwritten due to device reset.")
    }
    file_data$ConfigTime <- as.POSIXct(file_data$ConfigTime, format = "%Y-%m-%d %H:%M:%OS", origin = "1970-01-01", tz = "GMT")
    file_data$ConfigTime <- as.numeric(file_data$ConfigTime) - file_data$TimeOffset
    file_data$ConfigTimeISO <- paste0(gsub(" ", "T", as.character(as.POSIXct((file_data$ConfigTime + file_data$TimeOffset), origin = "1970-01-01", tz = "GMT"))), file_data$TimeZone)
    file_data$ConfigNotes <- trimws(sub("Config Notes:", "", binfile[32]))
    extra_lines <- line_numbers[["extractinfo"]] - 33
    if (extra_lines > 0) {
      for (i in 1:extra_lines) {
        file_data$ConfigNotes <- paste(file_data$ConfigNotes, trimws(binfile[32 + i]), sep = " ")
      }
    }
    file_data$ExtractInvestigatorID <- trimws(sub("Extract Operator ID:", "", binfile[line_numbers[["extractinfo"]]]))
    file_data$ExtractTime <- trimws(sub("Extract Time:", "", binfile[line_numbers[["extractinfo"]] + 1]))
    file_data$ExtractTime <- as.POSIXct(file_data$ExtractTime, format = "%Y-%m-%d %H:%M:%OS", origin = "1970-01-01", tz = "GMT")
    file_data$ExtractTime <- as.numeric(file_data$ExtractTime) - file_data$TimeOffset
    file_data$ExtractTimeISO <- paste0(gsub(" ", "T", as.character(as.POSIXct((file_data$ExtractTime + file_data$TimeOffset), origin = "1970-01-01", tz = "GMT"))), file_data$TimeZone)
    file_data$ExtractNotes <- trimws(sub("Extract Notes:", "", binfile[line_numbers[["extractinfo"]] + 2]))
    file_data$ClockDrift <- regmatches(file_data$ExtractNotes, gregexpr("(?<=\\().*?(?=\\))", file_data$ExtractNotes, perl = T))[[1]][1]
    file_data$ExtractNotes <- unlist(strsplit(file_data$ExtractNotes, split = paste0("(", file_data$ClockDrift, ")"), fixed = TRUE))[2]
    file_data$ClockDrift <- as.numeric(gsub("[^0-9.-]", "", file_data$ClockDrift))
    extra_lines <- line_numbers[["subjectinfo"]] - line_numbers[["extractinfo"]] - 4
    if (extra_lines > 0) {
      for (i in 1:extra_lines) {
        file_data$ExtractNotes <- paste(file_data$ExtractNotes, trimws(binfile[line_numbers[["extractinfo"]] + 2 + i]), sep = " ")
      }
    }
    file_data$ClockDriftDay <- round((file_data$ClockDrift / ((file_data$ExtractTime - file_data$ConfigTime) / (24 * 60 * 60))), digits = 7)
    file_data$WearLocationConfig <- sub("Device Location Code:", "", binfile[line_numbers[["subjectinfo"]] + 1])
    file_data$ParticipantID <- trimws(sub("Subject Code:", "", binfile[line_numbers[["subjectinfo"]] + 2]))
    file_data$ParticipantDoB <- sub("Date of Birth:", "", binfile[line_numbers[["subjectinfo"]] + 3])
    if (file_data$ParticipantDoB == "1900-1-1") {
      file_data$ParticipantDoB <- ""
    } else {
      file_data$ParticipantDoB <- format(as.Date(file_data$ParticipantDoB, format = "%Y-%m-%d"), format = "%d-%b-%y")
    }
    file_data$ParticipantSex <- sub("Sex:", "", binfile[line_numbers[["subjectinfo"]] + 4])
    file_data$ParticipantHeight <- sub("Height:", "", binfile[line_numbers[["subjectinfo"]] + 5])
    file_data$ParticipantWeight <- sub("Weight:", "", binfile[line_numbers[["subjectinfo"]] + 6])
    file_data$ParticipantHandedness <- sub("Handedness Code:", "", binfile[line_numbers[["subjectinfo"]] + 7])
    file_data$ParticipantNotes <- trimws(sub("Subject Notes:", "", binfile[line_numbers[["subjectinfo"]] + 8]))
    extra_lines <- line_numbers[["calibrationdata"]] - line_numbers[["subjectinfo"]] - 10
    if (extra_lines > 0) {
      for (i in 1:extra_lines) {
        file_data$ParticipantNotes <- paste(file_data$ParticipantNotes, trimws(binfile[line_numbers[["subjectinfo"]] + 8 + i]), sep = " ")
      }
    }
    file_data$VoltsEnd <- sub("Battery voltage:", "", binfile[line_numbers[["lastline"]] - 3])
    file_data$VoltsStart <- sub("Battery voltage:", "", binfile[line_numbers[["firstpage"]] + 6])

    # calibration information
    scale <- c(
      as.numeric(sub("x gain:", "", binfile[line_numbers[["calibrationdata"]] + 1])),
      as.numeric(sub("y gain:", "", binfile[line_numbers[["calibrationdata"]] + 3])),
      as.numeric(sub("z gain:", "", binfile[line_numbers[["calibrationdata"]] + 5]))
    )
    scale <- ((256 * 100) / scale)

    offset <- c(
      as.numeric(sub("x offset:", "", binfile[line_numbers[["calibrationdata"]] + 2])),
      as.numeric(sub("y offset:", "", binfile[line_numbers[["calibrationdata"]] + 4])),
      as.numeric(sub("z offset:", "", binfile[line_numbers[["calibrationdata"]] + 6]))
    )
    offset <- (-offset / (256 * 100))

    light_denominator <- as.numeric(sub("Volts:", "", binfile[line_numbers[["calibrationdata"]] + 7]))
    light_numerator <- as.numeric(sub("Lux:", "", binfile[line_numbers[["calibrationdata"]] + 8]))

    factory_calibration <- list(
      scale = scale,
      offset = offset,
      temperatureoffset = c(0, 0, 0),
      error = NA,
      lightdenominator = light_denominator,
      lightnumerator = light_numerator
    )

    # output error list to console as warning
    if (length(errors) > 0) {
      warning(errors)
    }

    # add file history note
    file_history <- rbind(file_history, paste0(substr(Sys.time(), 0, 23), " MPI created"))

    # set up MPI object for return
    MPI <- list(
      file_history = file_history,
      errors = errors,
      line_numbers = line_numbers,
      measurement_numbers = measurement_numbers,
      file_info = file_info,
      file_data = file_data,
      factory_calibration = factory_calibration
    )

    if (out_rds) {
      saveRDS(MPI, file.path(output_folder, paste0(UniqueBinFileIdentifier, "_MPI.rds")))
    }

    return(MPI)
  } else {
    MPI <- NA
    return(MPI)
  }
}

#' MPI Summary
#'
#' @details Wrapper function that calls \code{create_summary} for MPI only.
#' @param input MPI path.
#' @param recursive TRUE applies the operation to all nested elements.
#' @returns Data frame of MPI summary.
#' @export
MPI_summary <- function(input, recursive = TRUE) {
  create_summary(input, path_type = "MPI", recursive = recursive)
}

#' Bin File Summary
#'
#' @details Wrapper function that calls \code{create_summary} for bin files only.
#' @param input Bin file path.
#' @param recursive TRUE applies the operation to all nested elements.
#' @returns Data frame of bin file or MPI summary.
#' @export
binfile_summary <- function(input, recursive = TRUE) {
  create_summary(input, path_type = "BIN", recursive = recursive)
}

#' Create Summary
#'
#' @details Function to create a summary of key information of a bin file or MPI path.
#' @param input Input type of either a bin file path, MPI path or an MPI object.
#' @param path_type The file type within the folder to create summary for.
#' @param recursive TRUE applies the operation to all nested elements.
#' @returns Data frame of bin file or MPI summary.
#' @importFrom utils tail
create_summary <- function(input, path_type, recursive) {
  if (is.character(input)) {
    if (file.info(input)$isdir) {
      all_items <- list.files(input, full.names = TRUE, recursive = recursive)
      files <- all_items[!file.info(all_items)$isdir]
      all_summaries <- list()
        for (file in files) {
          summary <- single_summary(file, path_type = path_type)
          all_summaries[[basename(file)]] <- summary
        }
        summaries <- do.call(rbind, all_summaries)
        summaries <- summaries[rowSums(is.na(summaries)) != ncol(summaries), ]
        row.names(summaries) <- NULL
      if (all(is.na(summaries))) {
        warning(paste(basename(input), ": No summary produced"))
        summary <- NA
      } else {
        summary <- summaries
      }
    } else {
      summary <- single_summary(input)
      if (all(is.na(summary))) {
        warning(paste(basename(input), ": No summary produced"))
      }
    }
  } else {
    summary <- single_summary(input)
    if (all(is.na(summary))) {
      warning("MPI object: No summary produced")
    }
  }
  return(summary)
}

single_summary <- function(input, path_type = c("BIN", "MPI")) {
  # If an input is a bin file path or a first round MPI, do quick summary.
  # If input is a full MPI with non-movement and transitions, do full summary.
  PAGELENGTH <- 10
  UBI_warning_message <- NULL
  if (is.character(input)) {
    # Check that file exists
    if (file.exists(input)) {
      if (("BIN" %in% path_type && grepl("\\.bin$", input))) {
        binfile_path <- input
        first100 <- readLines(binfile_path, n = 100, skipNul = TRUE)
        firstpage <- which(grepl("Recorded Data", first100))[1]
        pagecount <- as.numeric(sub("Number of Pages:", "", first100[which(grepl("Number of Pages", first100))]))
        binfile <- tryCatch(
          {
            if (length(first100) >= 69) {
              lastline <- pagecount * 10 + firstpage - 1
              if (lastline > 100) {
                # Open the file in binary mode
                con <- file(binfile_path, open = "r")
                seek(con, where = -10000, origin = "end") # Adjust 1e3 to the number of bytes to read from the end
                last_chunk <- readLines(con, n = 100)
                close(con)
                last20 <- tail(last_chunk, 20)
                if (grep("Recorded Data", last20)[1] == 1) {
                  last_page_start <- grep("Recorded Data", last20)[2]
                  last_page_pages <- as.numeric(strsplit(last20[grep("Sequence Number", last20)[2]], ":")[[1]][2])
                  last_page <- last20[last_page_start:length(last20)]
                } else {
                  last_page_start <- grep("Recorded Data", last20)[1]
                  last_page_pages <- as.numeric(strsplit(last20[grep("Sequence Number", last20)[1]], ":")[[1]][2])
                  last_page <- last20[last_page_start:(last_page_start + PAGELENGTH - 1)]
                }
                c(first100, rep("", (last_page_pages * 10 + firstpage - 1 - 100)), last_page)
              } else {
                first100
              }
            } else {
              first100
              warning("Bin file too short or no data pages.")
            }
          },
          error = function(e) {
            warning(paste0("Error in MPI_summary(", binfile_path, ")"))
            NA
          }
        )

        UniqueBinFileIdentifier <- tryCatch({
          get_UniqueBinFileIdentifier(binfile)
        })

        if (is.na(UniqueBinFileIdentifier)) UBI_warning_message <- "Not a valid GENEActiv bin file."

        MPI <- create_MPI(binfile, binfile_path, ".", out_rds = FALSE)
      } else if ("MPI" %in% path_type && (grepl("\\MPI.rds", input))) {
        UBI_warning_message <- NA
        MPI <- readRDS(input)
      } else {
        UBI_warning_message <- NA
        MPI <- NA
        # warning(paste(input, ": The file must have a .bin extension."))
      }
    } else {
      UBI_warning_message <- NA
      MPI <- NA
      # warning(paste(input, ": The file path provided does not exist."))
    }
  } else if (is.list(input) && inherits(input, "list") && length(input) >= 6) {
    MPI <- input
  } else {
    UBI_warning_message <- NA
    MPI <- NA
    warning("Invalid input type. Accepted inputs are binfile paths, MPI paths or an MPI object.")
  }

  if (all(!is.na(MPI))) {
    if (is.list(MPI) && length(MPI) >= 6) {
      summary <- data.frame(MPI$file_data)

      summary <- subset(summary, select = c(
        "UniqueBinFileIdentifier",
        "BinfileName",
        "ClockDrift",
        "ConfigTimeISO",
        "ExtractTimeISO",
        "MeasurementDevice",
        "MeasurementDeviceID",
        "MeasurementDurationActual",
        "MeasurementFrequency",
        "MeasurementStartTimeISO",
        "MeasurementEndTimeISO",
        "SiteID",
        "StudyID",
        "ParticipantID",
        "TimeZone",
        "VoltsEnd",
        "VoltsStart",
        "WearLocationConfig"
      ))

      errors <- as.list(paste((MPI$error), collapse = " "))
      summary$Errors <- do.call(paste, errors)
    } else {
      summary <- data.frame(
        "UniqueBinFileIdentifier" = NA,
        "BinfileName" = basename(binfile_path),
        "ClockDrift" = NA,
        "ConfigTimeISO" = NA,
        "ExtractTimeISO" = NA,
        "MeasurementDevice" = NA,
        "MeasurementDeviceID" = NA,
        "MeasurementDurationActual" = NA,
        "MeasurementFrequency" = NA,
        "MeasurementStartTimeISO" = NA,
        "MeasurementEndTimeISO" = NA,
        "SiteID" = NA,
        "StudyID" = NA,
        "ParticipantID" = NA,
        "TimeZone" = NA,
        "VoltsEnd" = NA,
        "VoltsStart" = NA,
        "WearLocationConfig" = NA
      )

      if (is.list(MPI) && length(MPI) >= 1) {
        errors <- as.list(paste((MPI$error), collapse = " "))
        summary$Errors <- do.call(paste, errors)
      } else {
        summary$Errors <- UBI_warning_message
      }
    }
    summary$NumberDays <- NA
    summary$Transitions <- NA
    summary$NonWear <- NA
    summary$StillBouts <- NA

    if ("transitions" %in% names(MPI)) {
      summary$NumberDays <- MPI$file_data$NumberDays
      summary$Transitions <- nrow(MPI$transitions)
    }
    if ("non_movement" %in% names(MPI)) {
      summary$NonWear <- sum(MPI$non_movement$non_wear$duration)
      summary$StillBouts <- sum(MPI$non_movement$still_bouts$duration)
    }
  } else if ((all(is.na(MPI))) && is.character(UBI_warning_message)) {
    summary <- data.frame(
      "UniqueBinFileIdentifier" = NA,
      "BinfileName" = basename(binfile_path),
      "ClockDrift" = NA,
      "ConfigTimeISO" = NA,
      "ExtractTimeISO" = NA,
      "MeasurementDevice" = NA,
      "MeasurementDeviceID" = NA,
      "MeasurementDurationActual" = NA,
      "MeasurementFrequency" = NA,
      "MeasurementStartTimeISO" = NA,
      "MeasurementEndTimeISO" = NA,
      "SiteID" = NA,
      "StudyID" = NA,
      "ParticipantID" = NA,
      "TimeZone" = NA,
      "VoltsEnd" = NA,
      "VoltsStart" = NA,
      "WearLocationConfig" = NA,
      "Errors" = UBI_warning_message,
      "NumberDays" = NA,
      "Transitions" = NA,
      "NonWear" = NA,
      "StillBouts" = NA
    )
  } else {
    summary <- NA
  }
  return(summary)
}
