library(testthat)
local({
  ## Nonwear rest coverage

  bouts <- data.frame(
    TimeUTC = c(1, 16, 30, 43, 55, 66, 76),
    Duration = c(15, 14, 13, 12, 11, 10, 9),
    AGSAMean = c(0.0062, 0.5, 0.18, 0.15, 0.0822, 0.0078, 0.0023)
  )

  nonwear_day <- data.frame(
    nonwear_start = c(33, 75),
    nonwear_end = c(58, 87),
    MPI_nonwear_duration = c(25, 12)
  )

  TEMPMPI <- list()
  TEMPMPI$non_movement$rest_intervals <- data.frame(
    start_time = 1,
    duration = 20
  )

  expected_nonweartime <- c(0, 0, 10, 12, 3, 1, 9)
  expected_resttime <- c(15, 5, 0, 0, 0, 0, 0)

  bouts <- nonwear_rest_coverage(MPI = TEMPMPI, aggregated_data = bouts, nonwear_day = nonwear_day)

  test_that("Nonwear rest coverage calculates the correct non-wear time per event.", {
    expect_equal(bouts$nonwear.time, expected_nonweartime)
  })
  test_that("Nonwear rest coverage calculates the correct rest time per event.", {
    expect_equal(bouts$rest.time, expected_resttime)
  })

  nonwear_day <- data.frame(
    nonwear_start = NULL,
    nonwear_end = NULL,
    MPI_nonwear_duration = NULL
  )

  TEMPMPI$non_movement$rest_intervals <- data.frame(
    start_time = NULL,
    duration = NULL
  )

  expected_nonweartime <- c(0, 0, 0, 0, 0, 0, 0)
  expected_resttime <- c(0, 0, 0, 0, 0, 0, 0)

  bouts <- nonwear_rest_coverage(MPI = TEMPMPI, aggregated_data = bouts, nonwear_day = nonwear_day)

  test_that("Nonwear rest coverage calculates no non-wear time per event.", {
    expect_equal(bouts$nonwear.time, expected_nonweartime)
  })
  test_that("Nonwear rest coverage calculates no rest time per event.", {
    expect_equal(bouts$rest.time, expected_resttime)
  })

  ## Find rest intervals

  TEMPMPI <- list()
  TEMPMPI$non_movement$non_wear <- data.frame(start_time = NULL, duration = NULL)
  TEMPMPI$non_movement$still_bouts <- data.frame(
    start_time = c(1, 6, 16, 31, 81, 86, 111, 146, 191),
    duration = c(5, 10, 15, 20, 5, 25, 35, 45, 2),
    number_events = rep(1, 9)
  )

  TEMPMPI$file_data[["MeasurementStartTimeUTC"]] <- 1
  TEMPMPI$file_data[["MeasurementEndTimeUTC"]] <- 200
  TEMPMPI$file_data[["FirstLocalMidnightTimeUTC"]] <- 80

  TEMPMPI <- find_rest_intervals(10, 5, "00:00", TEMPMPI)

  expected_rest_intervals <- data.frame(
    start_time = c(1, 81),
    duration = c(50, 112),
    day_number = c(1, 2)
  )

  test_that("MPI stores the correct rest interval recommendations for each day.", {
    expect_equal(TEMPMPI$non_movement$rest_intervals, expected_rest_intervals)
  })

  ## Bouts decision tree

  bouts <- data.frame(
    Duration = c(15, 14, 13, 12, 11, 10, 9),
    xSD = c(0.0001, 0.2, 0.002, 0.002, 0.4, 0.008, 0.0001),
    ySD = c(0.0001, 0.2, 0.002, 0.002, 0.272, 0.01, 0.0001),
    zSD = c(0.0001, 0.2, 0.002, 0.002, 0.413, 0.014, 0.0001),
    AGSAMean = c(0.0062, 0.5, 0.18, 0.15, 0.0822, 0.0078, 0.0023),
    StepMean = c(0, 80, 72, 46, 10, 2, 0),
    nonwear.time = c(0, 0, 0, 0, 0, 3, 5),
    rest.time = c(10, 0, 0, 0, 0, 0, 0)
  )

  expected_bouts <- data.frame(
    NonWear = c(rep(FALSE, 6), TRUE),
    Rest = c(TRUE, rep(FALSE, 6)),
    Active = c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
    Ambulatory = c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
    Vigorous = c(FALSE, TRUE, rep(FALSE, 5)),
    FastWalk = c(FALSE, TRUE, TRUE, rep(FALSE, 4)),
    Sleep = c(TRUE, rep(FALSE, 5), TRUE),
    Classification = c("sleep", "run", "fast_walk", "slow_walk", "active", "sedentary", "non_wear")
  )

  bouts <- bouts_decision_tree(bouts, 5.7e-5, 0.0625, 0.407)

  test_that("Bouts decision tree assigns the correct classifications.", {
    expect_equal(bouts[, 10:17], expected_bouts)
  })
})
