local({
  bouts <- data.frame(
    TimeUTC = c(1619417001, 1619417016, 1619417030, 1619417043, 1619417055, 1619417066, 1619417076, 1619417085, 1619417092, 1619417100, 1619417107),
    Duration = c(15, 14, 13, 12, 11, 10, 9, 7, 8, 7, 6),
    NonWear = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
    Rest = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
    Sleep = c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE),
    Classification = c(
      "sleep", "sedentary", "sleep", "sleep", "sedentary",
      "sleep", "non_wear", "sedentary", "sleep", "sedentary", "sleep"
    ),
    stringsAsFactors = FALSE
  )

  TEMPMPI <- list()
  TEMPMPI$non_movement$non_wear <- data.frame(start_time = 1619417076, duration = 9)
  TEMPMPI$non_movement$rest_intervals <- data.frame(
    day_number = 1,
    start_time = 1619417001,
    duration = 91
  )
  TEMPMPI$file_data$NumberDays <- 1
  TEMPMPI$file_data$TimeOffset <- 3600
  TEMPMPI$file_data$CutTime24Hr <- "15:00"
  TEMPMPI$file_data[["MeasurementStartTimeUTC"]] <- 1619416801
  TEMPMPI$file_data[["MeasurementEndTimeUTC"]] <- 1619417501
  TEMPMPI$file_data[["FirstLocalMidnightTimeUTC"]] <- 1619478000

  test_dir <- file.path(tempdir(), "sleep_test")
  dir.create(test_dir, showWarnings = FALSE)
  UniqueBinFileIdentifier <- "TEST123"
  dir.create(file.path(test_dir, "GENEAbout"))
  saveRDS(TEMPMPI, file.path(test_dir, paste0(UniqueBinFileIdentifier, "_MPI.rds")))
  saveRDS(bouts, file.path(test_dir, "GENEAbout", paste0(UniqueBinFileIdentifier, "_day1_bouts.rds")))

  expected_measures <- data.frame(
    NightofDate = "25-Apr-2021",
    RestStartTime = "07:03:21",
    RestEndTime = "07:04:52",
    RestIntervalDuration = 91,
    SleepOnsetTime = "07:03:21",
    SleepEndTime = "07:04:36",
    SleepIntervalDuration = 75,
    TotalSleepDuration = 50,
    SleepEfficiency = 54.9,
    SleepOnsetLatency = 0,
    SleepOffsetDuration = 16,
    WakeAfterSleepOnsetDuration = 25,
    WakeAfterSleepOnsetCount = 2,
    NapsCountDay = 2,
    TotalNapDuration = 14,
    MeanNapDuration = 7,
    Notes = NA
  )


  actual_measures <- single_file_measures(
    output_folder = test_dir,
    UniqueBinFileIdentifier = UniqueBinFileIdentifier,
    minimum_valid_hours = 0,
    measure_type = "sleep"
  )

  test_that("Daily sleep measures are aggregated correctly.", {
    expect_equal(actual_measures, expected_measures)
  })
})
