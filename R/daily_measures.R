#' Daily Measures for Data Folder
#' @details Wrapper to process and output daily activity measures aggregate for all bin files in the data folder
#'
#' @param data_folder Folder that contains raw data bin files to process or path to single bin file.
#' @param measure_type The type of daily measure summary to generate.
#' @param minimum_valid_hours Minimum hours of wear time in a 24-hour day to be considered a valid day for reported measures.
#' @param include_participant_info If `TRUE`, appends participant information extracted from the bin file to both the activity and sleep measures.
#' @param identifier_mapping_record File path of the exported identifier_mapping_record CSV file.
#' @returns Daily aggregated activity measures (.csv)
#' @export
folder_measures <- function(data_folder,
                            measure_type = c("activity", "sleep", "both"),
                            minimum_valid_hours = 22,
                            include_participant_info = FALSE,
                            identifier_mapping_record = NULL) {
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
  if (!all(is.na(mpi_summary))) {
    total_files <- length(data_files)
    for (seq in 1:total_files) {
      project <- gsub("\\.bin", "", basename(data_files[seq]))
      output_folder <- file.path(data_folder, project)

      UniqueBinFileIdentifier <- mpi_summary$UniqueBinFileIdentifier[mpi_summary$BinfileName == data_files[seq]]

      single_file_measures(output_folder,
                           UniqueBinFileIdentifier,
                           measure_type,
                           minimum_valid_hours,
                           include_participant_info,
                           identifier_mapping_record)
    }
  } else {
    warning(sprintf("Bin files in '%s' have not yet been processed by GENEAcore. Run geneacore parts 1-3 first.", data_folder))
  }
}

#' Daily Measures for Single Bin File
#' @details Wrapper to process and output daily activity measures aggregate for a single bin file
#'
#' @param output_folder Path to the folder containing GENEAcore run outputs and Measurement Period Information (MPI) files.
#' @param UniqueBinFileIdentifier The Unique Bin File Identifier of a bin file.
#' @param measure_type The type of daily measure summary to generate.
#' @param minimum_valid_hours Minimum hours of wear time in a 24-hour day to be considered a valid day for reported measures.
#' @param include_participant_info If `TRUE`, appends participant information extracted from the bin file to both the activity and sleep measures.
#' @param identifier_mapping_record File path of the exported identifier_mapping_record CSV file.
#' @returns Daily aggregated activity measures (.csv)
#' @export
#' @keywords internal
#' @importFrom utils write.table
single_file_measures <- function(output_folder,
                                 UniqueBinFileIdentifier,
                                 measure_type = c("activity", "sleep", "both"),
                                 minimum_valid_hours = 22,
                                 include_participant_info = FALSE,
                                 identifier_mapping_record = NULL) {
  measure_type <- match.arg(measure_type)

  mpi_filepath <- file.path(output_folder, paste0(UniqueBinFileIdentifier, "_MPI.rds"))
  MPI <- readRDS(mpi_filepath)

  # Option 1: Create measures from daily bouts
  daily_bouts_files <- list.files(output_folder,
    pattern = paste0(UniqueBinFileIdentifier, "_day[0-9]+_bouts\\.rds"),
    recursive = TRUE
  )

  # Option 2: Create measures from aggregated bouts file
  aggregated_bouts_file <- list.files(output_folder,
    pattern = paste0(UniqueBinFileIdentifier, "_bouts\\.rds"),
    full.names = TRUE
  )

  if (length(daily_bouts_files) > 0) {
    daily_bouts_reader <- function(day_number) {
      pattern <- paste0(UniqueBinFileIdentifier, "_day", day_number, "_bouts\\.rds")
      f <- daily_bouts_files[grepl(pattern, daily_bouts_files)]
      if (length(f) == 0) {
        return(NULL)
      }
      readRDS(file.path(output_folder, f[1]))
    }
  } else if (length(aggregated_bouts_file) > 0) {
    all_bouts <- readRDS(aggregated_bouts_file[1])

    daily_bouts_reader <- function(day_number) {
      b <- all_bouts[all_bouts$DayNumber == day_number, , drop = FALSE]
      if (nrow(b) == 0) {
        return(NULL)
      }
      b
    }
  } else {
    warning(sprintf("'%s' has no bouts outputs", output_folder))
    return(invisible(NULL))
  }

  activity_measures <- NULL
  sleep_measures <- NULL
  daily_measures <- NULL

  if (measure_type %in% c("activity", "both")) {
    activity_measures <- calculate_daily_activity_measures(
      MPI = MPI,
      daily_bouts = daily_bouts_reader,
      minimum_valid_hours = minimum_valid_hours,
      include_participant_info = include_participant_info
    )
  }

  if (measure_type %in% c("sleep", "both")) {
    sleep_measures <- calculate_daily_sleep_measures(
      MPI = MPI,
      daily_bouts = daily_bouts_reader,
      minimum_valid_hours = minimum_valid_hours,
      include_participant_info = include_participant_info
    )
  }

  if (measure_type == "activity") {
    daily_measures <- activity_measures
    output_suffix <- "_daily_activity_summary.csv"
  } else if (measure_type == "sleep") {
    daily_measures <- sleep_measures
    output_suffix <- "_daily_sleep_summary.csv"
  } else if (measure_type == "both") {
    merge_notes <- gsub("NA", "", paste0(sleep_measures$Notes, activity_measures$Notes))

    if (include_participant_info) {
      # Merge on participant info columns
      participant_info_cols <- c(
        "StudyID",
        "SiteID",
        "PeriodInfo",
        "ParticipantID",
        "BinfileName",
        "LocalExtractTime",
        "UniqueBinFileIdentifier",
        "MeasurementDeviceID",
        "MeasurementDevice",
        "MeasurementFrequency",
        "MeasurementStartDate",
        "MeasurementDuration",
        "WearLocation"
      )
      daily_measures <- cbind(
        activity_measures,
        sleep_measures[, !(names(activity_measures) %in% participant_info_cols), drop = FALSE]
      )

      daily_measures <- daily_measures[
        , c(setdiff(names(daily_measures), participant_info_cols), participant_info_cols),
        drop = FALSE
      ]

      daily_measures$Notes <- merge_notes

      cols <- names(daily_measures)

      new_order <- c(
        "NightofDate",
        setdiff(cols, c("NightofDate", "Notes")),
        "Notes"
      )

      daily_measures <- daily_measures[, new_order, drop = FALSE]
    } else {
      daily_measures <- cbind(
        sleep_measures[, names(sleep_measures) != "Notes", drop = FALSE],
        activity_measures
      )
      daily_measures$Notes <- merge_notes
    }

    daily_measures$Date <- NULL

    output_suffix <- "_daily_measures_summary.csv"
  }

  dir.create(file.path(dirname(output_folder), "GENEAbout"), showWarnings = FALSE)
  output_location <- file.path(
    dirname(output_folder),
    "GENEAbout",
    paste0(UniqueBinFileIdentifier, output_suffix)
  )

  if (!is.null(identifier_mapping_record)) {
    daily_measures <- identifier_mapping(
      x = daily_measures,
      cols_to_map = c("SiteID", "StudyID", "PeriodInfo", "ParticipantID"),
      identifier_mapping_record = identifier_mapping_record
    )
  }

  write.csv(daily_measures, output_location, row.names = FALSE, na = "")

  invisible(daily_measures)
}

#' Calculate Daily Activity Measures
#'
#' @details Function that calculates the daily activity measures only
#' @param MPI Measurement Period Information.
#' @param daily_bouts Function used to read daily bouts or extract them from the aggregated bouts file
#' @param minimum_valid_hours Minimum hours of wear time in a 24-hour day to be considered a valid day for reported measures.
#' @param include_participant_info If `TRUE`, appends participant information extracted from the bin file to both the activity and sleep measures.
#' @returns Activity measures data frame for all days
#' @keywords internal
calculate_daily_activity_measures <- function(MPI,
                                              daily_bouts,
                                              minimum_valid_hours = 22,
                                              include_participant_info = FALSE) {
  number_days <- MPI$file_data$NumberDays
  time_offset <- MPI$file_data$TimeOffset

  cut_time_24hr <- MPI$file_data[["CutTime24Hr"]]
  cut_times <- get_cut_times(cut_time_24hr, MPI)
  cut_time_24hr_offset <- 60 * (60 * as.numeric(unlist(strsplit(cut_time_24hr, ":"))[1]) + as.numeric(unlist(strsplit(cut_time_24hr, ":"))[2]))


  date_range <- data.frame(start = cut_times[1:(length(cut_times) - 1)], end = cut_times[2:(length(cut_times))])
  date_range$length <- date_range$end - date_range$start
  date_range <- date_range[date_range$length > 5, ]
  date_range$day <- rownames(date_range)

  nonwear_by_day <- valid_day_nonwear(MPI[["non_movement"]][["non_wear"]], date_range, minimum_valid_hours, cut_time_24hr)
  valid_days <- as.integer(date_range$day[nonwear_by_day$valid_day$valid_day])

  Date <- format(as.POSIXct(date_range$start + time_offset - cut_time_24hr_offset, tz = "GMT"), "%d-%b-%Y")
  DayNumber <- seq(1:number_days)
  TotalStepsDay <- WearDurationDay <- NonWearDurationDay <- SleepDurationDay <- ActiveDurationDay <-
    SedentaryDurationDay <- LightDurationDay <- ModerateDurationDay <- VigorousDurationDay <-
    MVPADurationDay <- ActiveVolumeDay <- ActiveVolumeDayMET <- ActiveIntensityDay <-
    M6MinIntensity <- M6MinTime <- M5HrIntensity <- M5HrTime <- L5HrIntensity <- L5HrTime <-
    M10HrIntensity <- M10HrTime <- L10HrIntensity <- L10HrTime <- METMinutesDay <- EEDay <-
    SlowStepsDay <- FastStepsDay <- WalkingDurationDay <- MeanCadenceDay <- Cadence95Day <-
    RunningStepsDay <- RunningDurationDay <- SitStandTransitionsDay <- rep(NA_real_, number_days)
  Notes <- rep(NA, number_days)
  if (include_participant_info) {
    StudyID <- rep(MPI$file_data$StudyID, number_days)
    SiteID <- rep(MPI$file_data$SiteID, number_days)
    PeriodInfo <- rep(MPI$file_data$PeriodInfo, number_days)
    ParticipantID <- rep(MPI$file_data$ParticipantID, number_days)
    BinfileName <- rep(MPI$file_data$BinfileName, number_days)
    LocalExtractTime <- rep(MPI$file_data$ExtractTimeUTC, number_days)
    UniqueBinFileIdentifier <- rep(MPI$file_data$UniqueBinFileIdentifier, number_days)
    MeasurementDeviceID <- rep(MPI$file_data$MeasurementDeviceID, number_days)
    MeasurementDevice <- rep(MPI$file_data$MeasurementDevice, number_days)
    MeasurementFrequency <- rep(MPI$file_data$MeasurementFrequency, number_days)
    MeasurementStartDate <- rep(MPI$file_data$MeasurementStartDate, number_days)
    MeasurementDuration <- rep(MPI$file_data$MeasurementDurationSet, number_days)
    WearLocation <- rep(MPI$file_data$WearLocationConfig, number_days)
  }

  ## Setting active rules ##
  # Taken for MSSE GENEA 2011 paper
  # Average cutpoints
  Sedentary_Threshold <- 0.0625
  Moderate_Threshold <- (644 + 439) / 9600
  Vigorous_Threshold <- (2098 + 1810) / 9600

  for (day_number in seq_len(number_days)) {
    bouts <- daily_bouts(day_number)
    if (is.null(bouts)) {
      Notes[day_number] <- "No bouts for this day."
      next
    }
    if (!day_number %in% valid_days) {
      Notes[day_number] <- sprintf("Less than %s hours of wear time.", minimum_valid_hours)
      next
    }

    bouts$PhysicalActivityLevel <- bouts$Classification

    # Sedentary activity
    if (length(bouts[bouts$AGSAMean < Sedentary_Threshold &
      !bouts$Rest &
      !bouts$NonWear, ][, 1]) != 0) {
      bouts[bouts$AGSAMean < Sedentary_Threshold &
        !bouts$Rest &
        !bouts$NonWear, ]$PhysicalActivityLevel <- "sedentary" # Sedentary outside Rest Interval
    }

    # Light activity
    if (length(bouts[bouts$AGSAMean > Sedentary_Threshold &
      bouts$AGSAMean <= Moderate_Threshold, ][, 1]) != 0) {
      bouts[bouts$AGSAMean > Sedentary_Threshold &
        bouts$AGSAMean <= Moderate_Threshold, ]$PhysicalActivityLevel <- "light" # Light Activity
    }

    # Moderate activity
    if (length(bouts[bouts$AGSAMean > Moderate_Threshold &
      bouts$AGSAMean <= Vigorous_Threshold, ][, 1]) != 0) {
      bouts[bouts$AGSAMean > Moderate_Threshold &
        bouts$AGSAMean <= Vigorous_Threshold, ]$PhysicalActivityLevel <- "moderate" # Moderate Activity
    }

    # Vigorous activity
    if (length(bouts[bouts$AGSAMean > Vigorous_Threshold, ][, 1]) != 0) {
      bouts[bouts$AGSAMean > Vigorous_Threshold, ]$PhysicalActivityLevel <- "vigorous" # Vigorous Activity
    }

    idx_active <- bouts$Active & !bouts$NonWear

    WearDurationDay[day_number] <- sum(bouts$Duration[bouts$PhysicalActivityLevel != "non_wear"], na.rm = TRUE)
    NonWearDurationDay[day_number] <- sum(bouts$Duration[bouts$PhysicalActivityLevel == "non_wear"], na.rm = TRUE)
    SleepDurationDay[day_number] <- sum(bouts$Duration[bouts$PhysicalActivityLevel == "sleep"], na.rm = TRUE)
    SedentaryDurationDay[day_number] <- sum(bouts$Duration[bouts$PhysicalActivityLevel == "sedentary"], na.rm = TRUE)
    ActiveDurationDay[day_number] <- sum(bouts$Duration[idx_active], na.rm = TRUE)
    LightDurationDay[day_number] <- sum(bouts$Duration[bouts$PhysicalActivityLevel == "light"], na.rm = TRUE)
    ModerateDurationDay[day_number] <- sum(bouts$Duration[bouts$PhysicalActivityLevel == "moderate"], na.rm = TRUE)
    VigorousDurationDay[day_number] <- sum(bouts$Duration[bouts$PhysicalActivityLevel == "vigorous"], na.rm = TRUE)
    MVPADurationDay[day_number] <- sum(bouts$Duration[bouts$PhysicalActivityLevel %in% c("moderate", "vigorous")], na.rm = TRUE)

    # METs
    bouts$MET <- ((bouts$AGSAMean * 60 * 80) / 125.34)^0.6983
    bouts$MET[bouts$PhysicalActivityLevel == "sleep"] <- 0.9
    bouts$MET <- ifelse((bouts$MET < 0.9), 0.9, bouts$MET)
    bouts$METmins <- bouts$MET * bouts$Duration / 60
    METMinutesDay[day_number] <- sum(bouts$METmins, na.rm = TRUE)
    bouts$EE <- bouts$METmins * as.numeric(MPI$file_data$ParticipantWeight) * 3.5 / 200
    EEDay[day_number] <- sum(bouts$EE)
    if (is.na(EEDay[day_number])) {
      Notes[day_number] <- paste(Notes[day_number], "Energy expenditure could not be calculated because weight was not provided.")
    }

    ActiveVolumeDay[day_number] <- weighted_sum(bouts$AGSAMean, bouts$Duration, idx_active)
    ActiveVolumeDayMET[day_number] <- weighted_sum(bouts$MET, bouts$Duration / 60, idx_active)
    ActiveIntensityDay[day_number] <- weighted_mean(bouts$AGSAMean, bouts$Duration, idx_active)

    # M6 Minute
    M6Min <- mx_lx_window(bouts, 6 * 60, FALSE)
    M6MinIntensity[day_number] <- M6Min$M_intensity
    # M6MinTime[day_number]      = M6Min$M_time
    #
    # # M5Hr & L5Hr
    # M5Hr <- mx_lx_window(bouts, 5*3600, TRUE)
    # M5HrIntensity[day_number] = M5Hr$M_intensity
    # M5HrTime[day_number]      = M5Hr$M_time
    # L5HrIntensity[day_number] = M5Hr$L_intensity
    # L5HrTime[day_number]      = M5Hr$L_time
    #
    # # M10Hr & L10Hr
    # M10Hr <- mx_lx_window(bouts, 10*3600, TRUE)
    # M10HrIntensity[day_number] = M10Hr$M_intensity
    # M10HrTime[day_number]      = M10Hr$M_time
    # L10HrIntensity[day_number] = M10Hr$L_intensity
    # L10HrTime[day_number]      = M10Hr$L_time

    # Steps
    TotalStepsDay[day_number] <- sum(bouts$StepCount[idx_active], na.rm = TRUE)
    SlowStepsDay[day_number] <- sum(bouts$StepCount[idx_active & bouts$Ambulatory & bouts$StepMean <= 70], na.rm = TRUE)
    FastStepsDay[day_number] <- sum(bouts$StepCount[idx_active & bouts$Ambulatory & bouts$StepMean > 70], na.rm = TRUE)
    WalkingDurationDay[day_number] <- sum(bouts$Duration[idx_active & bouts$Ambulatory], na.rm = TRUE)
    MeanCadenceDay[day_number] <- 60 * sum(bouts$StepCount[idx_active & bouts$Ambulatory]) / sum(bouts$Duration[idx_active & bouts$Ambulatory])
    RunningStepsDay[day_number] <- sum(bouts$StepCount[bouts$Classification == "run"], na.rm = TRUE)
    RunningDurationDay[day_number] <- sum(bouts$Duration[bouts$Classification == "run"], na.rm = TRUE)

    # Cadence95
    cadence95 <- bouts[
      idx_active & bouts$Ambulatory & !is.na(bouts$Duration) & !is.na(bouts$StepMean),
      c("Duration", "StepMean")
    ]
    Cadence95Day[day_number] <- cx_percentile(cadence95, 95)

    SitStandTransitionsDay[day_number] <- sum(bouts$Classification[-length(bouts$Classification)] == "sedentary" &
      bouts$Active[-1] & !bouts$NonWear[-1], na.rm = TRUE)

    if (is.na(ActiveIntensityDay[day_number]) || is.na(MeanCadenceDay[day_number]) || is.na(Cadence95Day[day_number])) {
      Notes[day_number] <- gsub("NA", "", paste0(Notes[day_number], "No active bouts in day."))
    }
  }

  activity_measures <- data.frame(
    Date = Date,
    DayNumber = DayNumber,
    WearDurationDay = WearDurationDay,
    NonWearDurationDay = NonWearDurationDay,
    ActiveDurationDay = ActiveDurationDay,
    SedentaryDurationDay = SedentaryDurationDay,
    LightDurationDay = LightDurationDay,
    ModerateDurationDay = ModerateDurationDay,
    VigorousDurationDay = VigorousDurationDay,
    MVPADurationDay = MVPADurationDay,
    ActiveVolumeDay = round(ActiveVolumeDay, 0),
    ActiveVolumeDayMET = round(ActiveVolumeDayMET, 0),
    ActiveIntensityDay = round(ActiveIntensityDay, 3),
    M6MinIntensity = round(M6MinIntensity, 3),
    # M6MinTime = M6MinTime,
    # M5HrIntensity = M5HrIntensity,
    # M5HrTime = M5HrTime,
    # L5HrIntensity = L5HrIntensity,
    # L5HrTime = L5HrTime,
    # M10HrIntensity = M10HrIntensity,
    # M10HrTime = M10HrTime,
    # L10HrIntensity = L10HrIntensity,
    # L10HrTime = L10HrTime,
    METMinutesDay = round(METMinutesDay, 0),
    EEDay = round(EEDay, 0),
    TotalStepsDay = TotalStepsDay,
    SlowStepsDay = SlowStepsDay,
    FastStepsDay = FastStepsDay,
    WalkingDurationDay = WalkingDurationDay,
    MeanCadenceDay = round(MeanCadenceDay, 1),
    Cadence95Day = round(Cadence95Day, 1),
    RunningStepsDay = RunningStepsDay,
    RunningDurationDay = RunningDurationDay,
    SitStandTransitionsDay = SitStandTransitionsDay,
    Notes = Notes
  )

  if (include_participant_info) {
    activity_measures <- cbind(activity_measures,
      StudyID = StudyID,
      SiteID = SiteID,
      PeriodInfo = PeriodInfo,
      ParticipantID = ParticipantID,
      BinfileName = BinfileName,
      LocalExtractTime = format(as.POSIXct(LocalExtractTime, tz = "GMT"), "%d-%b-%Y %H:%M:%S"),
      UniqueBinFileIdentifier = UniqueBinFileIdentifier,
      MeasurementDeviceID = MeasurementDeviceID,
      MeasurementDevice = MeasurementDevice,
      MeasurementFrequency = MeasurementFrequency,
      MeasurementStartDate = MeasurementStartDate,
      MeasurementDuration = MeasurementDuration,
      WearLocation = WearLocation
    )
  }

  return(activity_measures)
}

#' Calculate Daily Sleep Measures
#'
#' @details Function that calculates the daily sleep measures only
#' @param MPI Measurement Period Information.
#' @param daily_bouts Function used to read daily bouts or extract them from the aggregated bouts file
#' @param minimum_valid_hours Minimum hours of wear time in a 24-hour day to be considered a valid day for reported measures.
#' @param include_participant_info If `TRUE`, appends participant information extracted from the bin file to both the activity and sleep measures.
#' @returns Sleep measures data frame for all days
#' @keywords internal
calculate_daily_sleep_measures <- function(MPI,
                                           daily_bouts,
                                           minimum_valid_hours = 22,
                                           include_participant_info = FALSE) {
  number_days <- MPI$file_data$NumberDays
  time_offset <- MPI$file_data$TimeOffset

  cut_time_24hr <- MPI$file_data[["CutTime24Hr"]]
  cut_times <- get_cut_times(cut_time_24hr, MPI)
  cut_time_24hr_offset <- 60 * (60 * as.numeric(unlist(strsplit(cut_time_24hr, ":"))[1]) + as.numeric(unlist(strsplit(cut_time_24hr, ":"))[2]))

  date_range <- data.frame(start = cut_times[1:(length(cut_times) - 1)], end = cut_times[2:(length(cut_times))])
  date_range$length <- date_range$end - date_range$start
  date_range <- date_range[date_range$length > 5, ]
  date_range$day <- rownames(date_range)

  nonwear_by_day <- valid_day_nonwear(MPI[["non_movement"]][["non_wear"]], date_range, minimum_valid_hours, cut_time_24hr)
  valid_days <- as.integer(date_range$day[nonwear_by_day$valid_day$valid_day])

  rest_intervals <- MPI[["non_movement"]][["rest_intervals"]]

  NightofDate <- format(as.POSIXct(date_range$start + time_offset - cut_time_24hr_offset, tz = "GMT"), "%d-%b-%Y")
  RestStartTime <- RestEndTime <- RestIntervalDuration <-
    SleepOnsetTime <- SleepEndTime <- SleepIntervalDuration <- TotalSleepDuration <- SleepEfficiency <-
    SleepOnsetLatency <- SleepOffsetDuration <- WakeAfterSleepOnsetDuration <- WakeAfterSleepOnsetCount <-
    TotalNapDuration <- MeanNapDuration <- NapsCountDay <- Notes <- rep(NA, number_days)
  if (include_participant_info) {
    StudyID <- rep(MPI$file_data$StudyID, number_days)
    SiteID <- rep(MPI$file_data$SiteID, number_days)
    PeriodInfo <- rep(MPI$file_data$PeriodInfo, number_days)
    ParticipantID <- rep(MPI$file_data$ParticipantID, number_days)
    BinfileName <- rep(MPI$file_data$BinfileName, number_days)
    LocalExtractTime <- rep(MPI$file_data$ExtractTimeUTC, number_days)
    UniqueBinFileIdentifier <- rep(MPI$file_data$UniqueBinFileIdentifier, number_days)
    MeasurementDeviceID <- rep(MPI$file_data$MeasurementDeviceID, number_days)
    MeasurementDevice <- rep(MPI$file_data$MeasurementDevice, number_days)
    MeasurementFrequency <- rep(MPI$file_data$MeasurementFrequency, number_days)
    MeasurementStartDate <- rep(MPI$file_data$MeasurementStartDate, number_days)
    MeasurementDuration <- rep(MPI$file_data$MeasurementDurationSet, number_days)
    WearLocation <- rep(MPI$file_data$WearLocationConfig, number_days)
  }

  for (day_number in seq_len(number_days)) {
    bouts <- daily_bouts(day_number)
    if (is.null(bouts)) {
      Notes[day_number] <- "No bouts for this day."
      next
    }
    if (!day_number %in% rest_intervals$day_number) {
      Notes[day_number] <- "No primary rest interval."
      next
    }
    if (!day_number %in% valid_days) {
      Notes[day_number] <- sprintf("Less than %s hours of wear time.", minimum_valid_hours)
      next
    }

    SLEEPBOUT <- bouts$Sleep & !bouts$NonWear

    # Rest Start Time (Auto)
    RestStartTime[day_number] <- rest_intervals$start_time[rest_intervals$day_number == day_number]

    # Rest Interval Duration
    RestIntervalDuration[day_number] <- rest_intervals$duration[rest_intervals$day_number == day_number]

    # Rest End Time (Auto)
    RestEndTime[day_number] <- RestStartTime[day_number] + RestIntervalDuration[day_number]

    # Sleep
    sleep_index <- which(SLEEPBOUT & bouts$Rest)
    if (length(sleep_index) > 0) {
      sleep_interval <- bouts[min(sleep_index):max(sleep_index), ]
      SLEEP <- sleep_interval$Sleep & !sleep_interval$NonWear
      NONSLEEP <- !sleep_interval$Sleep & !sleep_interval$NonWear

      # Sleep Onset Time
      SleepOnsetTime[day_number] <- sleep_interval$TimeUTC[1]

      # Sleep End Time
      SleepEndTime[day_number] <- sleep_interval$TimeUTC[nrow(sleep_interval)] + sleep_interval$Duration[nrow(sleep_interval)]

      # Sleep Interval Duration
      SleepIntervalDuration[day_number] <- SleepEndTime[day_number] - SleepOnsetTime[day_number]

      # Total Sleep Duration
      if (length(sleep_index) > 1) {
        TotalSleepDuration[day_number] <- sum(sleep_interval$Duration[SLEEP])
      } else {
        TotalSleepDuration[day_number] <- sleep_interval$Duration[1]
      }

      # Sleep Onset Latency
      SleepOnsetLatency[day_number] <- SleepOnsetTime[day_number] - RestStartTime[day_number]
      if (SleepOnsetLatency[day_number] < 0) {
        RestStartTime[day_number] <- SleepOnsetTime[day_number]
        RestIntervalDuration[day_number] <- RestEndTime[day_number] - RestStartTime[day_number]
        SleepOnsetLatency[day_number] <- 0
      }

      # Sleep Offset Duration
      SleepOffsetDuration[day_number] <- RestEndTime[day_number] - SleepEndTime[day_number]
      if (SleepOffsetDuration[day_number] < 0) {
        RestEndTime[day_number] <- SleepEndTime[day_number]
        RestIntervalDuration[day_number] <- RestEndTime[day_number] - RestStartTime[day_number]
        SleepOffsetDuration[day_number] <- 0
      }

      # Sleep Efficiency
      SleepEfficiency[day_number] <- 100 * TotalSleepDuration[day_number] / RestIntervalDuration[day_number]

      # Wake After Sleep Onset Duration
      WakeAfterSleepOnsetDuration[day_number] <- sum(sleep_interval$Duration[NONSLEEP])

      # Wake After Sleep Onset Count
      WakeAfterSleepOnsetCount[day_number] <- max(0, sum(diff(SLEEP) == 1))
    } else {
      Notes[day_number] <- "No sleep bouts within the primary rest interval."
    }

    # Naps
    naps <- which(SLEEPBOUT & !bouts$Rest)
    NapsCountDay[day_number] <- length(naps)
    if (NapsCountDay[day_number] > 0) {
      # Total Nap Duration
      TotalNapDuration[day_number] <- sum(bouts$Duration[naps])

      # Mean Nap Duration
      MeanNapDuration[day_number] <- TotalNapDuration[day_number] / NapsCountDay[day_number]
    } else {
      TotalNapDuration[day_number] <- 0
      MeanNapDuration[day_number] <- 0
    }
  }

  sleep_measures <- data.frame(
    NightofDate = NightofDate,
    RestStartTime = format(as.POSIXct(RestStartTime + time_offset, tz = "GMT"), "%H:%M:%S"),
    RestEndTime = format(as.POSIXct(RestEndTime + time_offset, tz = "GMT"), "%H:%M:%S"),
    RestIntervalDuration = RestIntervalDuration,
    SleepOnsetTime = format(as.POSIXct(SleepOnsetTime + time_offset, tz = "GMT"), "%H:%M:%S"),
    SleepEndTime = format(as.POSIXct(SleepEndTime + time_offset, tz = "GMT"), "%H:%M:%S"),
    SleepIntervalDuration = SleepIntervalDuration,
    TotalSleepDuration = TotalSleepDuration,
    SleepEfficiency = round(SleepEfficiency, 1),
    SleepOnsetLatency = SleepOnsetLatency,
    SleepOffsetDuration = SleepOffsetDuration,
    WakeAfterSleepOnsetDuration = WakeAfterSleepOnsetDuration,
    WakeAfterSleepOnsetCount = WakeAfterSleepOnsetCount,
    NapsCountDay = NapsCountDay,
    TotalNapDuration = TotalNapDuration,
    MeanNapDuration = round(MeanNapDuration, 0),
    Notes = Notes
  )

  if (include_participant_info) {
    sleep_measures <- cbind(sleep_measures,
      StudyID = StudyID,
      SiteID = SiteID,
      PeriodInfo = PeriodInfo,
      ParticipantID = ParticipantID,
      BinfileName = BinfileName,
      LocalExtractTime = format(as.POSIXct(LocalExtractTime, tz = "GMT"), "%d-%b-%Y %H:%M:%S"),
      UniqueBinFileIdentifier = UniqueBinFileIdentifier,
      MeasurementDeviceID = MeasurementDeviceID,
      MeasurementDevice = MeasurementDevice,
      MeasurementFrequency = MeasurementFrequency,
      MeasurementStartDate = MeasurementStartDate,
      MeasurementDuration = MeasurementDuration,
      WearLocation = WearLocation
    )
  }

  return(sleep_measures)
}
