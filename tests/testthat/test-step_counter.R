local({
  folder_path <- system.file("extdata", package = "GENEAcore")
  unlink(file.path(folder_path, "*.csv"))
  unlink(file.path(folder_path, "*.rds"))

  ## 50Hz 1.2
  binfile_path_50 <- file.path(folder_path, "100650Hz_file.bin")
  output_folder <- file.path(tempdir(), "GENEAcore")
  if (!dir.exists(output_folder)) dir.create(output_folder)

  con <- file(binfile_path_50, "r")
  binfile_50 <- readLines(con, skipNul = TRUE)
  close(con)

  MPI_50 <- create_MPI(binfile_50, binfile_path_50, output_folder)
  rawdata_50 <- sample_binfile(binfile_50, binfile_path_50, output_folder, downsample = FALSE)
  calibrated_50 <- apply_calibration(rawdata_50, MPI_50$factory_calibration, MPI_50$file_data[["MeasurementDevice"]])
  steps_Gcore_50 <- stepCounter(calibrated_50$y, samplefreq = MPI_50$file_data[["MeasurementFrequency"]])

  ## 20Hz 1.1
  binfile_path_20 <- file.path(folder_path, "20Hz_file.bin")

  con <- file(binfile_path_20, "r")
  binfile_20 <- readLines(con, skipNul = TRUE)
  close(con)

  MPI_20 <- create_MPI(binfile_20, binfile_path_20, output_folder)
  rawdata_20 <- sample_binfile(binfile_20, binfile_path_20, output_folder, downsample = FALSE)
  calibrated_20 <- apply_calibration(rawdata_20, MPI_20$factory_calibration, MPI_20$file_data[["MeasurementDevice"]])
  steps_Gcore_20 <- stepCounter(calibrated_20$y, samplefreq = MPI_20$file_data[["MeasurementFrequency"]])

  library(GENEAread)
  library(GENEAclassify)
  AccData_50 <- read.bin(binfile_path_50)
  steps_Gclassify_50 <- GENEAclassify::stepCounter(AccData_50)

  test_that("Step counter outputs match GENEAclassify", {
    expect_equal(steps_Gcore_50, steps_Gclassify_50[1:3])
  })

  AccData_20 <- read.bin(binfile_path_20)
  steps_Gclassify_20 <- GENEAclassify::stepCounter(AccData_20)

  test_that("Step counter outputs match GENEAclassify", {
    expect_equal(steps_Gcore_20, steps_Gclassify_20[1:3])
  })
})
