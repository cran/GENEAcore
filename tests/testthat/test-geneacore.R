library(testthat)
library(jsonlite)
local({
  # Test the successful completion of the processing, not necessarily the
  # accuracy of the results which is covered by the individual unit tests
  test_folder <- file.path(tempdir(), "GENEAcore")
  if (!dir.exists(test_folder)) dir.create(test_folder)

  # Ensure there aren't any existing output files in the temp directory
  existing_files <- list.files(test_folder,
    pattern = "\\.rds || \\.csv || \\.bin",
    full.names = TRUE, recursive = TRUE
  )
  if (length(existing_files) > 0) {
    cat("Deleting existing files from temp directory\n")
    result <- lapply(existing_files, FUN = function(x) {
      cat(paste("Deleting", x, "\n"))
      file.remove(x)
    })
    expect_true(all(result == TRUE))
  }
  existing_dirs <- list.dirs(test_folder)
  existing_dirs <- existing_dirs[-which(sapply(existing_dirs, function(x) test_folder %in% x))] # remove test folder from the list
  result_dir <- lapply(existing_dirs, FUN = function(x, test_folder) {
    if (x != test_folder) {
      cat(paste("Deleting", x, "\n"))
      unlink(x)
    }
  }, test_folder)
  if (length(existing_dirs) > 0) expect_true(all(result_dir == TRUE))

  # # Move bin files to temp dir for processing
  result <- file.copy(
    file.path(system.file("extdata", package = "GENEAcore"), "10Hz_calibration_file_20Nov25.bin"),
      test_folder
    )
  expect_true(all(result == TRUE))

  controls <- list(timer = TRUE, minimum_report_day = 0, output_epochs = TRUE)

  test_that("geneacore_part1 generates no warnings and outputs expected number of output files", {
    expect_warning(geneacore_part1(data_folder = test_folder, control = controls), regexp = NA) # Don't allow any warnings
    outputs <- list.files(test_folder, pattern = "\\.(rds|csv)$", recursive = TRUE, full.names = TRUE)
    expect_equal(length(outputs), 2)
  })

  test_that("MPI has correct size", {
    MPI <- readRDS(file.path(test_folder, "10Hz_calibration_file_20Nov25", "048297_1619380675_1_16862_MPI.rds"))
    expect_equal(length(MPI), 10)
    expect_equal(sum(MPI[["non_movement"]][["non_wear"]][["duration"]]), 50399)
    expect_equal(sum(MPI[["non_movement"]][["still_bouts"]][["duration"]]), 0)
  })

  test_that("geneacore_part2 generates no warnings and outputs expected number of output files", {
    expect_warning(geneacore_part2(data_folder = test_folder, control = controls), regexp = NA) # Don't allow any warnings
    outputs <- list.files(test_folder, pattern = "\\.(rds|csv)$", recursive = TRUE, full.names = TRUE)
    expect_equal(length(outputs), 4)
  })

  test_that("Downsample and raw sample have correct size", {
    downsample <- readRDS(file.path(test_folder, "10Hz_calibration_file_20Nov25", "048297_1619380675_1_16862_downsample.rds"))
    rawsample <- readRDS(file.path(test_folder, "10Hz_calibration_file_20Nov25", "048297_1619380675_1_16862_1619445600_1619467200_rawdata.rds"))
    expect_equal(nrow(downsample), 50399)
    expect_equal(nrow(rawsample), 216000)
    expect_equal(sum(downsample$x + downsample$y + downsample$z), 2531.023, tolerance = 1e-6)
    expect_equal(sum(rawsample$x + rawsample$y + rawsample$z), -2424.27, tolerance = 1e-6)
  })

  test_that("geneacore_part3 generates no warnings and outputs expected number of output files", {
    expect_warning(geneacore_part3(data_folder = test_folder, control = controls), regexp = NA) # Don't allow any warnings
    outputs <- list.files(test_folder, pattern = "\\.(rds|csv)$", recursive = TRUE, full.names = TRUE)
    expect_equal(length(outputs), 8)
  })

  test_that("Events and epochs have correct size", {
    epochs <- readRDS(file.path(test_folder, "10Hz_calibration_file_20Nov25", "048297_1619380675_1_16862_day1_epochs_1s.rds"))
    events <- readRDS(file.path(test_folder, "10Hz_calibration_file_20Nov25", "048297_1619380675_1_16862_day1_events.rds"))
    expect_equal(nrow(epochs), 28799)
    expect_equal(nrow(events), 47)
    expect_equal(epochs$TimeUTC[28799] + epochs$Duration[28799], 1619445600)
    expect_equal(events$TimeUTC[47] + events$Duration[47], 1619445600)
    expect_equal(sum(events$StepCount), 217)
    expect_equal(sum(events$StepMean), 371.5686, tolerance = 1e-6)
  })

  test_that("geneacore_part4 generates no warnings and outputs expected number of output files", {
    expect_warning(geneacore_part4(data_folder = test_folder, control = controls), regexp = NA) # Don't allow any warnings
    outputs <- list.files(test_folder, pattern = "\\.(rds|csv)$", recursive = TRUE, full.names = TRUE)
    expect_equal(length(outputs), 10)
  })
})


local({
  # Test the successful completion of the processing, not necessarily the
  # accuracy of the results which is covered by the individual unit tests
  test_folder <- file.path(tempdir(), "GENEAcore")
  if (!dir.exists(test_folder)) dir.create(test_folder)

  # Ensure there aren't any existing output files in the temp directory
  existing_files <- list.files(test_folder,
    pattern = "\\.rds || \\.csv || \\.bin",
    full.names = TRUE, recursive = TRUE
  )
  if (length(existing_files) > 0) {
    cat("Deleting existing files from temp directory\n")
    result <- lapply(existing_files, FUN = function(x) {
      cat(paste("Deleting", x, "\n"))
      file.remove(x)
    })
    expect_true(all(result == TRUE))
  }
  existing_dirs <- list.dirs(test_folder)
  existing_dirs <- existing_dirs[-which(sapply(existing_dirs, function(x) test_folder %in% x))] # remove test folder from the list
  result_dir <- lapply(existing_dirs, FUN = function(x, test_folder) {
    if (x != test_folder) {
      cat(paste("Deleting", x, "\n"))
      unlink(x)
    }
  }, test_folder)
  if (length(existing_dirs) > 0) expect_true(all(result_dir == TRUE))

  # # Move bin files to temp dir for processing
  result <- file.copy(
    file.path(system.file("extdata", package = "GENEAcore"), "10Hz_calibration_file_20Nov25.bin"),
      test_folder
    )
  expect_true(all(result == TRUE))

  controls <- list(timer = TRUE, required_processing_hours = 24, output_epochs = TRUE)
  geneacore_part1(data_folder = test_folder, control = controls)

  test_that("geneacore_part2 with 24 hour minimum valid day does not sample any data", {
    expect_warning(geneacore_part2(data_folder = test_folder, control = controls), regexp = NA) # Don't allow any warnings
    outputs <- list.files(test_folder, pattern = "\\.(rds|csv)$", recursive = TRUE, full.names = TRUE)
    expect_equal(length(outputs), 2)
  })

  test_that("geneacore_part3 generates warnings and no additional output files are created", {
    suppressWarnings(geneacore_part3(data_folder = test_folder, control = controls))
    outputs <- list.files(test_folder, pattern = "\\.(rds|csv)$", recursive = TRUE, full.names = TRUE)
    expect_equal(length(outputs), 2)
  })

  test_that("geneacore_part4 generates no warnings and no additional output files are created", {
    expect_warning(geneacore_part4(data_folder = test_folder, control = controls), regexp = NA) # Don't allow any warnings
    outputs <- list.files(test_folder, pattern = "\\.(rds|csv)$", recursive = TRUE, full.names = TRUE)
    expect_equal(length(outputs), 2)
  })
})

local({
  # Test the successful completion of the processing, not necessarily the
  # accuracy of the results which is covered by the individual unit tests
  test_folder <- file.path(tempdir(), "GENEAcore")
  if (!dir.exists(test_folder)) dir.create(test_folder)

  # Ensure there aren't any existing output files in the temp directory
  existing_files <- list.files(test_folder,
    pattern = "\\.rds || \\.csv || \\.bin",
    full.names = TRUE, recursive = TRUE
  )
  if (length(existing_files) > 0) {
    cat("Deleting existing files from temp directory\n")
    result <- lapply(existing_files, FUN = function(x) {
      cat(paste("Deleting", x, "\n"))
      file.remove(x)
    })
    expect_true(all(result == TRUE))
  }
  existing_dirs <- list.dirs(test_folder)
  existing_dirs <- existing_dirs[-which(sapply(existing_dirs, function(x) test_folder %in% x))] # remove test folder from the list
  result_dir <- lapply(existing_dirs, FUN = function(x, test_folder) {
    if (x != test_folder) {
      cat(paste("Deleting", x, "\n"))
      unlink(x)
    }
  }, test_folder)
  if (length(existing_dirs) > 0) expect_true(all(result_dir == TRUE))

  # # Move bin files to temp dir for processing
  result <- file.copy(
    file.path(system.file("extdata", package = "GENEAcore"), "10Hz_calibration_file_20Nov25.bin"),
      test_folder
    )
  expect_true(all(result == TRUE))

  controls <- list(timer = TRUE, required_processing_hours = 24, output_epochs = TRUE)

  test_that("geneacore with 24 hour required processing hours does not sample any data", {
    expect_warning(geneacore(data_folder = test_folder, control = controls), regexp = NA)
    outputs <- list.files(test_folder, pattern = "\\.(rds|csv)$", recursive = TRUE, full.names = TRUE)
    expect_equal(length(outputs), 2)
  })
})

### Full test - Short files, Error files and Large multi-day files

# local({
#   test_folder <- tempfile("GENEAcore_")
#   dir.create(test_folder)
#
#   nok <- file.copy(
#     from = list.files(testthat::test_path("testdata", "NoK"), full.names = TRUE),
#     to = test_folder
#   )
#
#   expect_true(all(nok))
#
#   geneacore(test_folder)
#
#   test_that("NoK folder creates the expected number of files", {
#     expect_equal(length(list.files(test_folder, recursive = T, pattern = ".rds")), 5)
#   })
# })
#
# local({
#   test_folder <- tempfile("GENEAcore_")
#   dir.create(test_folder)
#
#   ok <- file.copy(
#     from = list.files(testthat::test_path("testdata", "OK"), full.names = TRUE),
#     to = test_folder
#   )
#
#   expect_true(all(ok))
#
#   geneacore(test_folder)
#
#   number_bin_files <- length(list.files(test_folder, recursive = T, pattern = ".bin"))
#
#   test_that("OK folder creates the expected number of files", {
#     expect_equal(length(list.files(test_folder, recursive = T, pattern = "MPI.rds")), number_bin_files)
#     expect_equal(length(list.files(test_folder, recursive = T, pattern = "downsample.rds")), number_bin_files)
#     expect_equal(length(list.files(test_folder, recursive = T, pattern = "events.rds")), number_bin_files)
#   })
# })
#
#
# local({
#   test_folder <- tempfile("GENEAcore_")
#   dir.create(test_folder)
#
#   nok <- file.copy(
#     from = list.files(testthat::test_path("testdata", "NoK"), full.names = TRUE),
#     to = test_folder
#   )
#
#   expect_true(all(nok))
#
#   geneacore(test_folder)
#
#   test_that("NoK folder creates the expected number of files", {
#     expect_true(length(list.files(test_folder, recursive = T, pattern = ".rds")) == 5)
#   })
# })
#
# local({
#   test_folder <- tempfile("GENEAcore_")
#   dir.create(test_folder)
#
#   ok <- file.copy(
#     from = list.files(testthat::test_path("testdata", "Other"), full.names = TRUE),
#     to = test_folder
#   )
#
#   expect_true(all(ok))
#
#   geneacore(test_folder)
#   geneacore_part4(test_folder)
#
#   MPI_100565 <- readRDS(file.path(test_folder, "100565/100565_1708593627_1_574829_MPI.rds"))
#   MPI_tz <- readRDS(file.path(test_folder, "tz-2/100565_1708593627_1_864062_MPI.rds"))
#   events_files <- list.files(test_folder, recursive = T, pattern = "events.rds")
#   events_100565 <- readRDS(file.path(test_folder, events_files[4]))
#   events_tz <- readRDS(file.path(test_folder, events_files[9]))
#
#   number_bin_files <- length(list.files(test_folder, recursive = T, pattern = ".bin"))
#
#   test_that("OK folder creates the expected number of files", {
#     expect_true(length(list.files(test_folder, recursive = T, pattern = "MPI.rds")) == number_bin_files)
#     expect_true(length(list.files(test_folder, recursive = T, pattern = "downsample.rds")) == number_bin_files)
#     expect_true(length(list.files(test_folder, recursive = T, pattern = "events.rds")) == 9)
#   })
#
#   test_that("100565 events as expected", {
#     expect_equal(nrow(events_100565), 1695)
#     expect_equal(sum(MPI_100565$non_movement$still_bouts$duration), 21585)
#     expect_equal(sum(MPI_100565$non_movement$non_wear$duration), 62831)
#     expect_equal(sum(events_100565$StepCount), 23815)
#     expect_equal(sum(events_100565$rest.time), sum(MPI_100565$non_movement$rest_intervals$duration))
#     expect_equal(sum(events_100565$nonwear.time), sum(MPI_100565$non_movement$non_wear$duration))
#     expect_equal(sum(events_100565$AGSAMean), 117.224, tolerance = 1e-6)
#     expect_equal(max(events_100565$Duration), 31040)
#   })
#
#   test_that("tz-2 events as expected", {
#     expect_equal(nrow(events_tz), 2732)
#     expect_equal(sum(MPI_tz$non_movement$still_bouts$duration), 94286)
#     expect_equal(sum(MPI_tz$non_movement$non_wear$duration), 32514)
#     expect_equal(sum(events_tz$StepCount), 37678)
#     expect_equal(sum(events_tz$rest.time), sum(MPI_tz$non_movement$rest_intervals$duration))
#     expect_equal(sum(events_tz$nonwear.time), sum(MPI_tz$non_movement$non_wear$duration))
#     expect_equal(sum(events_tz$AGSAMean), 154.888, tolerance = 1e-6)
#     expect_equal(max(events_tz$Duration), 31040)
#     expect_equal(sum(events_tz$Duration[1:1193]), sum(events_100565$Duration[1:1193]))
#   })
# })
