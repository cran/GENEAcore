library(testthat)
# print(getwd())
# source("../../R/MPI_create.R")
# source("../../R/MPI_calibrate.R")
# source("../../R/MPI_detect.R")

## Unique BinFile Identifier and MPI ##
local({
  output_folder <- file.path(tempdir(), "GENEAcore")
  if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

  # Delete all MPI files generated
  files <- list.files(output_folder, pattern = "\\.(rds|csv)$", full.names = TRUE)
  file.remove(files)

  binfile_name_10Hz <- file.path(system.file("extdata", package = "GENEAcore"), "10Hz_calibration_file_20Nov25.bin")
  con_10Hz <- file(binfile_name_10Hz, "r")
  binfile_10Hz <- readLines(con_10Hz, skipNul = TRUE)
  close(con_10Hz)
  binfile_name <- binfile_name_10Hz

  MPI_10Hz <- create_MPI(binfile_10Hz, binfile_name, output_folder)

  ## For binfile summary tests
  summary_path <- binfile_summary(binfile_name_10Hz)
  summary_MPI_partial <- MPI_summary(MPI_10Hz)

  # binfile_name_tz <- file.path(system.file("extdata", package = "GENEAcore"),"tz-2.bin")
  # binfile_name <- binfile_name_tz
  # con_tz <- file(binfile_name_tz, "r")
  # binfile_tz <- readLines(con_tz, skipNul = TRUE)
  # close(con_tz)
  # MPI_tz <- create_MPI(binfile_tz, binfile_name_tz)

  USI_10Hz <- "048297_1619380675_1_16862"
  USI_tz <- "100565_1708593627_1_864062"

  line_numbers_10Hz <- c(34, 39, 50, 60, 63, 16862, 16853)
  line_numbers_tz <- c(34, 39, 50, 60, 63, 864062, 864053)

  measurement_numbers_10Hz <- c(6, 596, 35996, 647996, 611996)
  measurement_numbers_tz <- c(0, 2600, 236600, 4556600, 5276600)

  file_info_10Hz <- c(1680, ".", 1619416801, 1619467200, TRUE, 503990)
  file_info_tz <- c(86400, ".", 1708600834, 1708860034, FALSE, 25920000)

  # Binfilename set as NA
  file_data_10Hz <- c(
    "048297_1619380675_1_16862", "10Hz_calibration_file_20Nov25.bin", 0.775, 0.7310842, "JL",
    "First line of config notes. Second line of config notes.", 1619377075, "2021-04-25T19:57:55+01:00",
    "", "First line of extract notes. Second line of extract notes.", 1619468665, "2021-04-26T21:24:25+01:00", 16862, 1619478000, "GENEActiv 1.1", "11-Jan-18", "Ver4.08a date14Jul14", "048297",
    50399, 50400, 1619467200, "2021-04-26T21:00:00+01:00", 10, "26-Apr-21", 1619416801, "2021-04-26T07:00:01+01:00",
    "Activinsights", "Calibration", "", "", "", "", "First line of subject notes. Second line of subject notes.", "", "", "JL",
    "Verification", "+01:00", 3600, "4.1642", "4.1791", ""
  )

  file_data_tz <- c(
    "100565_1708593627_1_864062", "tz-2.bin", 2.570, 0.4293222, "", "This is line 1 of config notes. This is line 2 of config notes.", 1708600827, "2024-02-22T09:20:27-02:00",
    "Extract operator ID", "Extract notes line 1. Extract notes line 2.", 1709118033, "2024-02-28T09:00:33-02:00", 864062, 1708653600, "GENEActiv 1.2", "02-Aug-23", "Ver06.17 15June23", "100565",
    259200, 259200, 1708860034, "2024-02-25T09:20:34-02:00", 100, "22-Feb-24", 1708600834, "2024-02-22T09:20:34-02:00",
    "", "", "10-Feb-80", "right", 162, "A01", "This is the first line of subject notes. This is the second line of subject notes.", "Female", "61.8", "",
    "", "-02:00", -7200, "3.9800", "4.1100", "left wrist"
  )

  factory_calibration_10Hz <- c(
    "1.0021138", "1.0162763", "1.0178117",
    "-0.0027344", "0.0083594", "-0.013672",
    0, 0, 0, NA, 49, 913
  )

  factory_calibration_tz <- c(
    "1.0116578", "1.0230997", "1.0132594",
    "0.042461", "-7.8125e-05", "-0.16441",
    0, 0, 0, NA, 55, 1016
  )

  test_that("Unique BinFile Identifier is correct", {
    expect_equal(get_UniqueBinFileIdentifier(binfile_10Hz), USI_10Hz)
    # expect_equal(get_UniqueBinFileIdentifier(binfile_tz), USI_tz)
  })

  test_that("MPI$line_numbers is correct", {
    expect_equal(unname(MPI_10Hz$line_numbers), line_numbers_10Hz)
    # expect_equal(unname(MPI_tz$line_numbers), line_numbers_tz)
  })

  test_that("MPI$measurement_numbers is correct", {
    expect_equal(unname(MPI_10Hz$measurement_numbers), measurement_numbers_10Hz)
    # expect_equal(unname(MPI_tz$measurement_numbers), measurement_numbers_tz)
  })

  test_that("MPI$file_info is correct", {
    expect_equal(as.character(unlist((MPI_10Hz$file_info))), file_info_10Hz)
    # expect_equal(as.character(unlist((MPI_tz$file_info))), file_info_tz)
  })

  test_that("MPI$file_data is correct", {
    expect_equal(as.character(unlist((MPI_10Hz$file_data))), file_data_10Hz)
    # expect_equal(as.character(unlist((MPI_tz$file_data))), file_data_tz)
  })

  test_that("MPI$factory_calibration is correct", {
    expect_equal(unname(unlist(MPI_10Hz$factory_calibration)), as.numeric(factory_calibration_10Hz), tolerance = 1e-4)
    # expect_equal(unname(unlist(MPI_tz$factory_calibration)), as.numeric(factory_calibration_tz), tolerance = 1e-4)
  })

  MPI_10Hz <- detect_nonmovement(binfile_10Hz, binfile_name, output_folder)

  MPI_10Hz <- calc_autocalparams(
    binfile_10Hz, binfile_name, output_folder,
    MPI_10Hz$non_movement$sphere_points
  )

  MPI_10Hz <- detect_transitions(binfile_10Hz, binfile_name, output_folder)

  summary_MPI_full <- MPI_summary(MPI_10Hz)

  ##### Negative tests #####

  binfiles <- c(
    file.path(system.file("extdata", package = "GENEAcore"), "Not_a_GENEActiv_bin_file.bin"),
    file.path(system.file("extdata", package = "GENEAcore"), "One_page_reset_config_time.bin")
  )

  for (file in binfiles) {
    binfile_name <- file
    con <- file(binfile_name, "r")
    binfile <- readLines(con, skipNul = TRUE)
    close(con)
    MPI_summary_NA <- binfile_summary(file)
    test_that("UniqueBinFileIdentifier is NA.", {
      expect_warning(get_UniqueBinFileIdentifier(binfile), "Not a valid GENEActiv bin file.")
    })
  }

  binfile_name <- file.path(system.file("extdata", package = "GENEAcore"), "Zero_data_pages.bin")
  con <- file(binfile_name, "r")
  binfile <- readLines(con, skipNul = TRUE)
  close(con)
  test_that("MPI warning.", {
    expect_warning(create_MPI(binfile, binfile_name, output_folder), "no data pages")
  })
  test_that("Binfile summary for a file with zero data pages", {
    expect_warning(binfile_summary(binfile_name), "Bin file too short or no data pages")
  })

  binfile_name <- file.path(system.file("extdata", package = "GENEAcore"), "Incomplete_bin_file.bin")
  con <- file(binfile_name, "r")
  binfile <- readLines(con, skipNul = TRUE)
  close(con)
  test_that("MPI warning.", {
    expect_warning(create_MPI(binfile, binfile_name, output_folder), "Bin file is incomplete.")
  })

  binfile_name <- file.path(system.file("extdata", package = "GENEAcore"), "Short_header.bin")
  con <- file(binfile_name, "r")
  binfile <- readLines(con, skipNul = TRUE)
  close(con)
  test_that("MPI warning.", {
    expect_warning(create_MPI(binfile, binfile_name, output_folder), "Bin file too short")
  })

  binfile_name <- file.path(system.file("extdata", package = "GENEAcore"), "Not_contiguous.bin")
  con <- file(binfile_name, "r")
  binfile <- readLines(con, skipNul = TRUE)
  close(con)
  test_that("MPI warning.", {
    expect_warning(create_MPI(binfile, binfile_name, output_folder), "not contiguous")
  })

  binfile_name <- file.path(system.file("extdata", package = "GENEAcore"), "Multi_page_reset_config_time.bin")
  con <- file(binfile_name, "r")
  binfile <- readLines(con, skipNul = TRUE)
  close(con)
  test_that("MPI warning.", {
    expect_warning(create_MPI(binfile, binfile_name, output_folder), "Config data overwritten")
  })

  # Delete all MPI files generated
  files <- list.files(output_folder, pattern = "MPI", full.names = TRUE)
  file.remove(files)
  unlink(file.path(output_folder, "*.csv"))
  unlink(file.path(output_folder, "*.rds"))

  ## Binfile summary
  expected_summary_path <- c(
    "048297_1619380675_1_16862", "10Hz_calibration_file_20Nov25.bin", "0.775",
    "2021-04-25T19:57:55+01:00", "2021-04-26T21:24:25+01:00", "GENEActiv 1.1",
    "048297", "50399", "10", "2021-04-26T07:00:01+01:00", "2021-04-26T21:00:00+01:00",
    "Activinsights", "Calibration", "", "+01:00", "4.1642", "4.1791", "", "", NA, NA, NA, NA
  )
  expected_summary_MPI_partial <- expected_summary_path

  expected_summary_NA <- c(
    NA, "One_page_reset_config_time.bin", NA, NA, NA, NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Not a valid GENEActiv bin file.", NA, NA, NA, NA
  )

  expected_summary_MPI_full <- c(
    "048297_1619380675_1_16862", "10Hz_calibration_file_20Nov25.bin", "0.775",
    "2021-04-25T19:57:55+01:00", "2021-04-26T21:24:25+01:00", "GENEActiv 1.1",
    "048297", "50399", "10", "2021-04-26T07:00:01+01:00", "2021-04-26T21:00:00+01:00",
    "Activinsights", "Calibration", "", "+01:00", "4.1642", "4.1791", "",
    "", "2", "76", "50399", "0"
  )

  test_that("Binfile summary when a binfile path is supplied is correct", {
    expect_equal(as.character(unlist(summary_path)), expected_summary_path)
  })

  test_that("Binfile summary when a first MPI is supplied is correct", {
    expect_equal(as.character(unlist(summary_MPI_partial)), expected_summary_MPI_partial)
  })

  test_that("Binfile summary when a first MPI is supplied is correct", {
    expect_equal(as.character(unlist(MPI_summary_NA)), expected_summary_NA)
  })

  test_that("Binfile summary when a full MPI is supplied is correct", {
    expect_equal(as.character(unlist(summary_MPI_full)), expected_summary_MPI_full)
  })
})

# Test the overridden methods of get_UniqueBinFileIdentifier
local({
  # Prepare a temp folder with some bin files to process
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
  bin_files <- c("10Hz_calibration_file_20Nov25.bin", "100650Hz_file.bin", "1008667Hz.bin")
  result <- file.copy(
    file.path(system.file("extdata", package = "GENEAcore"), bin_files),
    test_folder
  )
  expect_true(all(result == TRUE))

  test_files <- file.path(test_folder, bin_files)
  expected_results <- data.frame(cbind(
    "filename" = c(
      "10Hz_calibration_file_20Nov25.bin",
      "100650Hz_file.bin",
      "1008667Hz.bin"
    ),
    "id" = c(
      "048297_1619380675_1_16862",
      "000030_1686916921_1_1599",
      "000030_1686922297_1_2989"
    )
  ))

  # Get the unique Id for a single file
  id <- get_UniqueBinFileIdentifier(test_files[1])
  expect_equal(id, expected_results[expected_results$filename == basename(test_files[1]), "id"])

  # Get the unique Id for a directory of files
  ids <- get_UniqueBinFileIdentifier(test_folder)
  for (i in 1:length(test_files)) {
    # Note test_files and ids won't necessarily be in the same order, ignore_attr to ignore the names, only test the value
    expect_equal(ids[i], expected_results[expected_results$filename == basename(names(ids[i])), "id"], ignore_attr = TRUE)
  }

  # Get the unique Id for the file contents
  con <- file(test_files[1], "r")
  binfile <- readLines(con, skipNul = TRUE)
  close(con)
  id <- get_UniqueBinFileIdentifier(binfile)
  expect_equal(id, expected_results[expected_results$filename == basename(test_files[1]), "id"], ignore_attr = TRUE)

  # Get the unique Id for an invalid file path
  expect_warning(get_UniqueBinFileIdentifier(bin_files[1]),
    regexp = "get_UniqueBinFileIdentifier: Unrecognised input type for:"
  )
})


local({
  output_folder <- file.path(tempdir(), "GENEAcore")
  if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

  # Delete all MPI files generated
  files <- list.files(output_folder, pattern = "\\.(rds|csv)$", full.names = TRUE)
  file.remove(files)

  print(files)


  binfile_name_10Hz <- file.path(system.file("extdata", package = "GENEAcore"), "10Hz_calibration_file_20Nov25.bin")
  con_10Hz <- file(binfile_name_10Hz, "r")
  binfile_10Hz <- readLines(con_10Hz, skipNul = TRUE)
  close(con_10Hz)
  binfile_name <- binfile_name_10Hz

  MPI_10Hz <- create_MPI(binfile_10Hz, binfile_name, output_folder)
  MPI_10Hz <- detect_nonmovement(binfile_10Hz, binfile_name, output_folder)

  MPI_10Hz <- calc_autocalparams(
    binfile_10Hz, binfile_name, output_folder,
    MPI_10Hz$non_movement$sphere_points
  )

  print(output_folder)

  checkmpi_res <- check_MPI(output_folder, print_output = TRUE)
  test_that("check_MPI runs without errors", {
    expect_true(any(grepl("Bin file:", checkmpi_res$output)))
    expect_equal(length(checkmpi_res$output), 12)
    expect_equal(checkmpi_res$results[["048297_1619380675_1_16862_MPI.rds"]]$file_data$BinfileName, "10Hz_calibration_file_20Nov25.bin")
  })
})
