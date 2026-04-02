local({
  # Reorder data frame

  df <- data.frame(
    TimeUTC = 1619416801,
    EpochNumber = 1,
    AGSAMean = 0.2,
    Duration = 15,
    xMean = 0.2
  )

  expected_df <- data.frame(
    TimeUTC = 1619416801,
    DateTimeUTC = "2021-04-26 06:00:01",
    Duration = 15,
    AGSAMean = 0.2,
    xMean = 0.2
  )

  test_that("Data frame is reordered.", {
    expect_equal(reorder_df(df), expected_df)
  })

  # Check time format

  test_that("Time format is invalid.", {
    expect_warning(check_time_format("24:52"))
  })

  # Round columns

  epochs_df <- data.frame(
    xMean = c(0.001111, 0.001222, 0.001333, 0.001444),
    yMean = c(0.2111, 0.2222, 0.2333, 0.2444),
    LightMean = c(1.2521, 1.7322, 1.9329, 2.0342),
    xSD = c(1.2521, 1.7322, 1.9329, 2.0342),
    DegreesMean = c(0.03, 1.25, 4.22, 7.98)
  )
  epochs_df <- round_columns(epochs_df)

  expected_epochs_df <- data.frame(
    xMean = c(0.00111, 0.00122, 0.00133, 0.00144),
    yMean = c(0.2111, 0.2222, 0.2333, 0.2444),
    xSD = c(1.252, 1.732, 1.933, 2.034),
    LightMean = c(1, 2, 2, 2),
    DegreesMean = c(0, 1.3, 4.2, 8)
  )

  test_that("Time format is invalid.", {
    expect_warning(check_time_format("24:52"))
  })

  # MPI to JSON

  TEMPMPI <- list()
  TEMPMPI$non_movement$sphere_points <- data.frame(
    x = 1619417076,
    y = 9,
    z = 12,
    temp = 12
  )
  TEMPMPI$file_history <- list("MPI created", "Downsampled")
  TEMPMPI$errors <- list()
  TEMPMPI$file_info <- data.frame(page_count = 1000, decimal_separator = ".", half_second_start = TRUE)
  TEMPMPI$file_data <- data.frame(
    NumberDays = 1,
    TimeOffset = 3600,
    CutTime24Hr = "15:00",
    MeasurementDeviceCalibrationDate = "23-Jun-2021",
    MeasurementStartDate = "30-May-2022",
    VoltsStart = "4.10",
    VoltsEnd = "4.01",
    ClockDrift = NA,
    ClockDriftDay = NA
  )
  TEMPMPI$button <- 32

  test_dir <- file.path(tempdir(), "mpi_json_test")
  dir.create(test_dir, showWarnings = FALSE)
  UniqueBinFileIdentifier <- "TEST123"
  mpi_path <- file.path(test_dir, paste0(UniqueBinFileIdentifier, "_MPI.rds"))
  json_path <- file.path(test_dir, paste0(UniqueBinFileIdentifier, "_MPI.json"))

  MPI_toJSON(TEMPMPI, mpi_path)

  mpi_json <- jsonlite::read_json(json_path)

  test_that("MPI JSON did not store sphere points.", {
    expect_null(mpi_json$non_movement$sphere_points)
  })

  test_that("Dates are in %Y-%m-%d format.", {
    expect_equal(mpi_json$file_data$MeasurementDeviceCalibrationDate, "2021-06-23")
    expect_equal(mpi_json$file_data$MeasurementStartDate, "2022-05-30")
  })

  test_that("Volts and clock drift are stored as numeric values.", {
    expect_equal(mpi_json$file_data$VoltsStart, 4.1)
    expect_equal(mpi_json$file_data$VoltsEnd, 4.01)
    expect_equal(mpi_json$file_data$ClockDrift, 0)
    expect_equal(mpi_json$file_data$ClockDriftDay, 0)
  })

  test_that("Button is stored as an array.", {
    expect_equal(mpi_json$button, list(32))
  })

  # Valid day nonwear

  MPI_nonwear <- data.frame(
    start_time = NULL,
    duration = NULL
  )

  date_range <- data.frame(start = c(1, 1), end = c(501, 701), length = c(500, 700), day = c(1, 2))

  expected_valid_day <- data.frame(day = c(1, 2), MPI_nonwear_duration = c(0, 0), wear_duration = c(500, 700), valid_day = c(TRUE, TRUE))

  expected_nonwear_day <- data.frame(
    interval = integer(), day = integer(),
    nonwear_start = numeric(), nonwear_end = numeric(), MPI_nonwear_duration = numeric()
  )

  test_that("Day is valid for no non-wear.", {
    expect_equal(valid_day_nonwear(MPI_nonwear, date_range)$valid_day, expected_valid_day)
  })

  test_that("Day is valid for no non-wear.", {
    expect_equal(valid_day_nonwear(MPI_nonwear, date_range)$nonwear_day, expected_nonwear_day, ignore_attr = TRUE)
  })
})

local({
  id_map_path <- testthat::test_path("testdata", "identifier-mapping-record-TEST.csv")

  x <- data.frame(
    ParticipantID = c("a01234", "a02345", "B01234", "c01234"),
    SiteID = c("sites", "site12341", "sites2", "site12343"),
    StudyID = c("test", "testtest", "testtesttest", "Testtesttest"),
    PeriodInfo = c("V...", "v...", "V?!", "v?!")
  )

  expected_mapped_x <- data.frame(
    ParticipantID = c("P-A", "a02345", "B01234", "P-C"),
    SiteID = c("sites", "S1", "sites2", "S3"),
    StudyID = c("test", "testtest", "TTT", "Testtesttest"),
    PeriodInfo = c("V...", "V00", "V?!", "V0")
  )

  mapped_x <- identifier_mapping(x,
    cols_to_map = c("SiteID", "StudyID", "PeriodInfo", "ParticipantID"),
    id_map_path
  )

  test_that("ID mapping is correct.", {
    expect_equal(mapped_x, expected_mapped_x)
  })

  test_that("ID mapping is correct.", {
    expect_warning(identifier_mapping(x,
      cols_to_map = c("SITE", "StudyID", "PeriodInfo", "ParticipantID"),
      id_map_path
    ), regexp = "unsupported names in cols_to_map: SITE")
  })

  x2 <- data.frame(
    PID = c("a01234", "a02345", "B01234", "c01234"),
    SiteID = c("sites", "site12341", "sites2", "site12343"),
    StudyID = c("test", "testtest", "testtesttest", "Testtesttest"),
    PeriodInfo = c("V...", "v...", "V?!", "v?!")
  )

  test_that("ID mapping is correct.", {
    expect_warning(identifier_mapping(x2,
      cols_to_map = c("StudyID", "PeriodInfo", "ParticipantID"),
      id_map_path
    ), regexp = "will be skipped: ParticipantID")
  })
})
