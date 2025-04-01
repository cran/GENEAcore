library(testthat)
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
      file.remove(x)
    }
  }, test_folder)
  if (length(existing_dirs) > 0) expect_true(all(result_dir == TRUE))

  # # Move bin files to temp dir for processing
  bin_files <- c("10Hz_calibration_file.bin") # "One_page_bin_file.bin")
  result <- lapply(list(bin_files), FUN = function(x) {
    file.copy(
      file.path(system.file("extdata", package = "GENEAcore"), x),
      test_folder
    )
  })
  expect_true(all(result == TRUE))

  test_that("geneacore generates no warnings", {
    expect_warning(
      geneacore(
        data_folder = test_folder,
        CutTime24Hr = "15:00",
        output_epochs = TRUE,
        epoch_duration = 1,
        output_events = TRUE,
        output_csv = TRUE,
        output_steps = TRUE,
        timer = TRUE
      ),
      regexp = NA
    ) # Don't allow any warnings
  })

  # Confirm expected output files exist
  outputs <- list.files(test_folder, "\\.rds || \\.csv", recursive = TRUE, full.names = TRUE)
  expect_true(length(outputs) == 9)
})
