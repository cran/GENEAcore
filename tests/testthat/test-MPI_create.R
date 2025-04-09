library(testthat)
# print(getwd())
# source("../../R/MPI_create.R")
# source("../../R/MPI_calibrate.R")
# source("../../R/MPI_detect.R")

## Unique BinFile Identifier and MPI ##
local({
  binfile_name_10Hz <- file.path(system.file("extdata", package = "GENEAcore"), "10Hz_calibration_file.bin")
  con_10Hz <- file(binfile_name_10Hz, "r")
  binfile_10Hz <- readLines(con_10Hz, skipNul = TRUE)
  close(con_10Hz)
  binfile_name <- binfile_name_10Hz
  output_folder <- file.path(tempdir(), "GENEAcore")
  if (!dir.exists(output_folder)) dir.create(output_folder)

  # Delete all MPI files generated
  files <- list.files(output_folder, pattern = "MPI", full.names = TRUE)
  file.remove(files)
  unlink(file.path(output_folder, "*.csv"))
  unlink(file.path(output_folder, "*.rds"))

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

  USI_10Hz <- "048297_1619380675_1_16859"
  USI_tz <- "100565_1708593627_1_864062"

  line_numbers_10Hz <- c(33, 37, 47, 57, 60, 16859, 16850)
  line_numbers_tz <- c(34, 39, 50, 60, 63, 864062, 864053)

  measurement_numbers_10Hz <- c(6, 596, 35996, 647996, 611996)
  measurement_numbers_tz <- c(0, 2600, 236600, 4556600, 5276600)

  file_info_10Hz <- c(1680, ".", 1619416801, 1619467200, TRUE, 503990)
  file_info_tz <- c(86400, ".", 1708600834, 1708860034, FALSE, 25920000)

  # Binfilename set as NA
  file_data_10Hz <- c(
    "048297_1619380675_1_16859", "10Hz_calibration_file.bin", 0.775, 0.7310842, "JL", "", 1619377075, "2021-04-25T19:57:55+01:00",
    "", NA, 1619468665, "2021-04-26T21:24:25+01:00", 16859, 1619478000, "GENEActiv 1.1", "11-Jan-18", "Ver4.08a date14Jul14", "048297",
    50399, 50400, 1619467200, "2021-04-26T21:00:00+01:00", 10, "26-Apr-21", 1619416801, "2021-04-26T07:00:01+01:00",
    "Activinsights", "Calibration", "", "", "", "", "", "", "", "JL",
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
    "048297_1619380675_1_16859", "10Hz_calibration_file.bin", "0.775",
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
    "048297_1619380675_1_16859", "10Hz_calibration_file.bin", "0.775",
    "2021-04-25T19:57:55+01:00", "2021-04-26T21:24:25+01:00", "GENEActiv 1.1",
    "048297", "50399", "10", "2021-04-26T07:00:01+01:00", "2021-04-26T21:00:00+01:00",
    "Activinsights", "Calibration", "", "+01:00", "4.1642", "4.1791", "",
    "", "2", "84", "44436", "0"
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
