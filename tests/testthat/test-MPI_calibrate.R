local({
  cal_params <- list(
    scale = c(1.015430, 1.017286, 1.027081),
    offset = c(0.001289063, 0.038398438, 0.013828125),
    temperature_offset = c(0, 0, 0),
    error = NA,
    light_denominator = 48,
    light_numerator = 911
  )

  # Raw data from test-MPI_sample_binfile
  rawdata <- data.frame(
    TimeUTC = c(rep(1726650857, 5)),
    x = c(
      0.2421875,
      0.24609375,
      0.25390625,
      0.24609375,
      0.23828125
    ),
    y = c(
      -0.04296875,
      -0.04687500,
      -0.03515625,
      -0.03125000,
      -0.04296875
    ),
    z = c(
      -0.9453125,
      -0.9453125,
      -0.9531250,
      -0.9531250,
      -0.9609375
    ),
    Light = c(rep(22, 5)),
    Button = c(rep(0, 5)),
    Temp = c(rep(21.3, 5)),
    Volts = c(rep(4.0896, 5))
  )

  # Expected data from read.bin()$data.out
  expected_data_G11 <- data.frame(
    TimeUTC = c(rep(1726650857, 5)),
    x = c(
      0.2472334,
      0.2511999,
      0.2591329,
      0.2511999,
      0.2432668
    ),
    y = c(
      -0.004649315,
      -0.008623088,
      0.003298232,
      0.007272005,
      -0.004649315
    ),
    z = c(
      -0.9567101,
      -0.9567101,
      -0.9647342,
      -0.9647342,
      -0.9727583
    ),
    Light = c(rep(417.542, 5)),
    Button = c(rep(0, 5)),
    Temp = c(rep(21.3, 5)),
    Volts = c(rep(4.0896, 5))
  )

  # Expected data from read.bin()$data.out
  expected_data_G12 <- data.frame(
    TimeUTC = c(rep(1726650857, 5)),
    x = c(
      0.2472334,
      0.2511999,
      0.2591329,
      0.2511999,
      0.2432668
    ),
    y = c(
      -0.004649315,
      -0.008623088,
      0.003298232,
      0.007272005,
      -0.004649315
    ),
    z = c(
      -0.9567101,
      -0.9567101,
      -0.9647342,
      -0.9647342,
      -0.9727583
    ),
    Light = c(rep(417.5417, 5)),
    Button = c(rep(0, 5)),
    Temp = c(rep(21.3, 5)),
    Volts = c(rep(4.0896, 5))
  )


  rownames(rawdata) <- NULL
  rownames(expected_data_G11) <- NULL
  rownames(expected_data_G12) <- NULL

  test_that("Calibrated values are the same as GENEAread for GENEActiv 1.1 with Temp TRUE.", {
    expect_equal(apply_calibration(rawdata, cal_params, "GENEActiv 1.1"), expected_data_G11, tolerance = 1e-6)
  })

  test_that("Calibrated values are the same as GENEAread for GENEActiv 1.1 with Temp FALSE.", {
    expect_equal(apply_calibration(rawdata, cal_params, "GENEActiv 1.1", FALSE), expected_data_G11, tolerance = 1e-6)
  })

  test_that("Calibrated values are the same as GENEAread for GENEActiv 1.2 with Temp TRUE.", {
    expect_equal(apply_calibration(rawdata, cal_params, "GENEActiv 1.2"), expected_data_G12, tolerance = 1e-6)
  })

  # Test calibration against GENEAread

  binfile_path <- file.path(system.file("extdata", package = "GENEAcore"), "10Hz_calibration_file_20Nov25.bin")
  output_folder <- file.path(tempdir(), "GENEAcore")
  if (!dir.exists(output_folder)) dir.create(output_folder)

  con <- file(binfile_path, "r")
  binfile <- readLines(con, skipNul = TRUE)
  close(con)

  # Remove RDS data file if it already exists
  UniqueBinFileIdentifier <- get_UniqueBinFileIdentifier(binfile)
  file_pattern <- paste0(UniqueBinFileIdentifier, "_.*.rds")
  files <- list.files(pattern = file_pattern, recursive = TRUE)
  for (afile in files) {
    print(paste0("test-MPI_calibrate.R: Deleting ", afile))
    rm(afile)
  }

  MPI <- create_MPI(binfile, binfile_path, output_folder)
  MPI <- detect_nonmovement(binfile, binfile_path, output_folder)
  MPI <- calc_autocalparams(
    binfile, binfile_path, output_folder,
    MPI$non_movement$sphere_points
  )

  rawdata <- sample_binfile(binfile, binfile_path, output_folder, downsample = FALSE)
  auto_calibrated <- apply_calibration(rawdata, MPI$auto_calibration, MPI$file_data[["MeasurementDevice"]])
  factory_calibrated <- apply_calibration(rawdata, MPI$factory_calibration, MPI$file_data[["MeasurementDevice"]])

  library(GENEAread)
  expected_rawdata <- as.data.frame(read.bin(binfile_path)$data.out)
  names(expected_rawdata)[c(1, 2, 3, 4, 7)] <- c("TimeUTC", "x", "y", "z", "Temp") # Adjust column names
  expected_rawdata$TimeUTC <- expected_rawdata$TimeUTC - 3600 # Compensate for TimeUTC zone bug in GENEAread

  # Adjust expected_rawdata for half second start
  test_that("Raw data is correct", {
    expect_equal(factory_calibrated[1:2000, 1:7], expected_rawdata[6:2005, ], tolerance = 1e-6, ignore_attr = TRUE)
  })
})
