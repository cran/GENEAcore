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

  geneacore(data_folder = test_folder)
  geneabout(data_folder = test_folder)

  test_that("create_coel_atoms_json generates no warnings and outputs expected number of json files", {
    expect_warning(create_coel_atoms_json(test_folder), regexp = NA)
    outputs <- list.files(test_folder, pattern = "\\.(json)$", recursive = TRUE, full.names = TRUE)
    expect_true(length(outputs) == 5)
  })
})
