local({
  # GENAcore accuracy is tested by comparing to the data read in by the existing
  # package GENEAread
  # Note: Half second start whole integer frequency tested in test-MPI_calibrate.R

  folder_path <- system.file("extdata", package = "GENEAcore")
  unlink(file.path(folder_path, "*.csv"))
  unlink(file.path(folder_path, "*.rds"))

  binfile_path <- file.path(folder_path, "One_page_bin_file.bin")

  con <- file(binfile_path, "r")
  binfile <- readLines(con, skipNul = TRUE)
  close(con)

  # Remove RDS data file if it already exists
  UniqueBinFileIdentifier <- get_UniqueBinFileIdentifier(binfile)
  file_pattern <- paste0(UniqueBinFileIdentifier, "_.*.rds")
  files <- list.files(pattern = file_pattern, recursive = TRUE)
  for (afile in files) {
    print(paste0("test-MPI_sample_binfile.R: Deleting ", afile))
    rm(afile)
  }

  output_folder <- file.path(tempdir(), "GENEAcore")
  if (!dir.exists(output_folder)) dir.create(output_folder)
  downsampled_binfile <- sample_binfile(binfile, binfile_path, output_folder)
  rawdata <- sample_binfile(binfile, binfile_path, output_folder, downsample = FALSE)

  library(GENEAread)
  expected_downsample <- as.data.frame(read.bin(binfile_path, calibrate = FALSE, downsample = 100)$data.out, mmap.load = FALSE)
  expected_downsample[, c("x", "y", "z")] <- expected_downsample[, c("x", "y", "z")] / 256 # Apply scaling factor
  names(expected_downsample)[c(1, 2, 3, 4, 7)] <- c("timestamp", "x", "y", "z", "temp") # Adjust column names
  expected_downsample$timestamp <- expected_downsample$timestamp - 3600 # Compensate for time zone bug in GENEAread

  expected_rawdata <- as.data.frame(read.bin(binfile_path, calibrate = FALSE)$data.out, mmap.load = FALSE)
  expected_rawdata[, c("x", "y", "z")] <- expected_rawdata[, c("x", "y", "z")] / 256 # Apply scaling factor
  names(expected_rawdata)[c(1, 2, 3, 4, 7)] <- c("timestamp", "x", "y", "z", "temp") # Adjust column names
  expected_rawdata$timestamp <- expected_rawdata$timestamp - 3600 # Compensate for time zone bug in GENEAread

  test_that("Raw sampled data for whole second start integer frequency file matches GENEAread", {
    expect_equal(rawdata[, 1:7], expected_rawdata, tolerance = 1e-6)
  })

  test_that("Downsampled data for whole second start integer frequency file matches GENEAread", {
    expect_equal(downsampled_binfile[, 1:7], expected_downsample, tolerance = 1e-6)
  })

  # Recurring decimal file

  binfile_path <- file.path(folder_path, "1008667Hz.bin")

  con <- file(binfile_path, "r")
  binfile <- readLines(con, skipNul = TRUE)
  close(con)

  # Remove RDS data file if it already exists
  UniqueBinFileIdentifier <- get_UniqueBinFileIdentifier(binfile)
  file_pattern <- paste0(UniqueBinFileIdentifier, "_.*.rds")
  files <- list.files(pattern = file_pattern, recursive = TRUE)
  for (afile in files) {
    print(paste0("test-MPI_sample_binfile.R: Deleting ", afile))
    rm(afile)
  }

  output_folder <- file.path(tempdir(), "GENEAcore")
  if (!dir.exists(output_folder)) dir.create(output_folder)
  downsampled_binfile <- sample_binfile(binfile, binfile_path, output_folder)
  rawdata <- sample_binfile(binfile, binfile_path, output_folder, downsample = FALSE)

  library(GENEAread)
  expected_downsample <- as.data.frame(read.bin(binfile_path, calibrate = FALSE, downsample = 100)$data.out, mmap.load = FALSE)
  expected_downsample[, c("x", "y", "z")] <- expected_downsample[, c("x", "y", "z")] / 256 # Apply scaling factor
  names(expected_downsample)[c(1, 2, 3, 4, 7)] <- c("timestamp", "x", "y", "z", "temp") # Adjust column names
  expected_downsample$timestamp <- expected_downsample$timestamp - 3600 # Compensate for time zone bug in GENEAread

  expected_rawdata <- as.data.frame(read.bin(binfile_path, calibrate = FALSE)$data.out, mmap.load = FALSE)
  expected_rawdata[, c("x", "y", "z")] <- expected_rawdata[, c("x", "y", "z")] / 256 # Apply scaling factor
  names(expected_rawdata)[c(1, 2, 3, 4, 7)] <- c("timestamp", "x", "y", "z", "temp") # Adjust column names
  expected_rawdata$timestamp <- expected_rawdata$timestamp - 3600 # Compensate for time zone bug in GENEAread

  test_that("Raw sampled data for whole second start recurring decimal frequency file matches GENEAread", {
    expect_equal(rawdata[1:1000, 1:7], expected_rawdata[1:1000, ], tolerance = 1e-6)
  })

  # test_that("Downsampled data is correct", {
  #   expect_equal(downsampled_binfile[, 1:7], expected_downsample, tolerance = 1e-6)
  # })

  # Half second start file

  binfile_path <- file.path(folder_path, "667Hz.bin")

  con <- file(binfile_path, "r")
  binfile <- readLines(con, skipNul = TRUE)
  close(con)

  # Remove RDS data file if it already exists
  UniqueBinFileIdentifier <- get_UniqueBinFileIdentifier(binfile)
  file_pattern <- paste0(UniqueBinFileIdentifier, "_.*.rds")
  files <- list.files(pattern = file_pattern, recursive = TRUE)
  for (afile in files) {
    print(paste0("test-MPI_sample_binfile.R: Deleting ", afile))
    rm(afile)
  }

  output_folder <- file.path(tempdir(), "GENEAcore")
  if (!dir.exists(output_folder)) dir.create(output_folder)
  downsampled_binfile <- sample_binfile(binfile, binfile_path, output_folder)
  rawdata <- sample_binfile(binfile, binfile_path, output_folder, downsample = FALSE)

  library(GENEAread)
  expected_downsample <- as.data.frame(read.bin(binfile_path, calibrate = FALSE, downsample = 100)$data.out, mmap.load = FALSE)
  expected_downsample[, c("x", "y", "z")] <- expected_downsample[, c("x", "y", "z")] / 256 # Apply scaling factor
  names(expected_downsample)[c(1, 2, 3, 4, 7)] <- c("timestamp", "x", "y", "z", "temp") # Adjust column names

  expected_rawdata <- as.data.frame(read.bin(binfile_path, calibrate = FALSE)$data.out, mmap.load = FALSE)
  expected_rawdata[, c("x", "y", "z")] <- expected_rawdata[, c("x", "y", "z")] / 256 # Apply scaling factor
  names(expected_rawdata)[c(1, 2, 3, 4, 7)] <- c("timestamp", "x", "y", "z", "temp") # Adjust column names

  test_that("Raw sampled data for half second start recurring decimal frequency file matches GENEAread", {
    expect_equal(rawdata[1:2000, 1:7], expected_rawdata[34:2033, ], tolerance = 1e-6, ignore_attr = TRUE)
  })

  # test_that("Downsampled data is correct", {
  #   expect_equal(downsampled_binfile[, 1:7], expected_downsample, tolerance = 1e-6)
  # })
})
