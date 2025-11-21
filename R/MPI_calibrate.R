#' Calculate Auto-calibration Parameters
#' @description Function to calculate auto-calibration parameters from known
#' still points from a bin file that create a unitary sphere.
#' @param binfile Text lines read from an open connection to a bin file.
#' @param binfile_path Path to the bin file to be processed.
#' @param output_folder Path to the folder containing GENEAcore run outputs and Measurement Period Information (MPI) files.
#' @param sphere_points List of points that populate a unitary sphere and their
#' associated temperature in the form (x,y,z,temp).
#' @param use_temp Allows auto-calibration to be run with and without
#' temperature compensation.
#' @param spherecrit The minimum required acceleration value for each axis in
#'  both directions for auto-calibration to be reliable.
#' @param maxiter The maximum number of sphere fit iterations attempted during
#' auto-calibration.
#' @param tol The limit of incremental sphere fit improvements before
#' auto-calibration is considered complete.
#' @return List of auto-calibration parameters within the measurement period
#' information (MPI).
#' @export
#' @importFrom stats lm.wfit
#' @examples
#' binfile_path <- system.file("extdata/10Hz_calibration_file_20Nov25.bin", package = "GENEAcore")
#' output_folder <- tempdir()
#' con <- file(binfile_path, "r")
#' binfile <- readLines(con, skipNul = TRUE)
#' close(con)
#' MPI <- create_MPI(binfile, binfile_path, output_folder)
#' nonmovement_list <- detect_nonmovement(binfile, binfile_path, output_folder)
#' MPI <- calc_autocalparams(
#'   binfile, binfile_path, output_folder,
#'   nonmovement_list$non_movement$sphere_points
#' )
calc_autocalparams <- function(binfile,
                               binfile_path,
                               output_folder,
                               sphere_points,
                               use_temp = TRUE,
                               spherecrit = 0.3,
                               maxiter = 500,
                               tol = 1e-13) {
  # :DEV: - add checks that sphere data is in right format

  # get UniqueBinFileIdentifier
  UniqueBinFileIdentifier <- get_UniqueBinFileIdentifier(binfile)

  if (!is.na(UniqueBinFileIdentifier)) {
    # check if MPI file already exists and create if not
    MPI_filepath <- file.path(output_folder, paste0(UniqueBinFileIdentifier, "_MPI.rds"))
    if (file.exists(MPI_filepath)) {
      MPI <- readRDS(MPI_filepath)
    } else {
      MPI <- create_MPI(binfile, binfile_path, output_folder)
    }

    tel <- 0

    if ((nrow(sphere_points) > 5)) {
      for (axis in 1:3) {
        if (min(sphere_points[, axis]) < -spherecrit && max(sphere_points[, axis]) > spherecrit) {
          tel <- tel + 1
        }
      }

      if (tel != 3) {
        MPI$errors <- rbind(MPI$errors, "Auto-calibration not calculated because not enough points on all sides of the sphere.")
        warning(paste(basename(binfile_path), ": Auto-calibration not calculated because not enough points on all sides of the sphere."))
      } else {
        # Start of Zhou Fang's calibration code #
        input <- sphere_points[, 1:3]
        inputtemp <- cbind(sphere_points[, 4], sphere_points[, 4], sphere_points[, 4])
        meantemp <- mean(as.numeric(inputtemp[, 1]), na.rm = TRUE)
        inputtemp <- inputtemp - meantemp
        offset <- rep(0, ncol(input))
        scale <- rep(1, ncol(input))
        tempoffset <- rep(0, ncol(input))
        weights <- rep(1, nrow(input))
        res <- Inf

        for (iter in 1:maxiter) {
          curr <- scale(input, center = -offset, scale = 1 / scale) +
            scale(inputtemp, center = F, scale = 1 / tempoffset)
          closestpoint <- curr / sqrt(rowSums(curr^2))
          k <- 1
          offsetch <- rep(0, ncol(input))
          scalech <- rep(1, ncol(input))
          toffch <- rep(0, ncol(inputtemp))

          for (k in 1:ncol(input)) {
            fobj <- lm.wfit(cbind(1, curr[, k], inputtemp[, k]),
              closestpoint[, k, drop = F],
              w = weights
            )
            offsetch[k] <- fobj$coef[1]
            scalech[k] <- fobj$coef[2]
            if (use_temp == TRUE) {
              toffch[k] <- fobj$coeff[3]
            }
            curr[, k] <- fobj$fitted.values
          }
          offset <- offset + offsetch / (scale * scalech)
          if (use_temp == TRUE) {
            tempoffset <- tempoffset * scalech + toffch
          }
          scale <- scale * scalech
          res <- c(res, 3 * mean(weights * (curr - closestpoint)^2 / sum(weights)))
          weights <- pmin(1 / sqrt(rowSums((curr - closestpoint)^2)), 1 / 0.01)
          if (abs(res[iter + 1] - res[iter]) < tol) break

          iter <- iter
        }

        if (use_temp == FALSE) {
          sphere_points_cal <- scale(as.matrix(sphere_points[, 1:3]),
            center = -offset, scale = 1 / scale
          )
        } else {
          yy <- as.matrix(cbind(
            sphere_points[, 4], sphere_points[, 4],
            sphere_points[, 4]
          ))
          sphere_points_cal <- scale(as.matrix(sphere_points[, 1:3]),
            center = -offset, scale = 1 / scale
          ) +
            scale(yy, center = rep(meantemp, 3), scale = 1 / tempoffset)
        }

        # End of Zhou Fang's code #

        # calculate error from factory calibration to MPI
        factory_calibration <- MPI$factory_calibration
        cal_error_factory <- scale(as.matrix(sphere_points[, 1:3]),
          center = -factory_calibration$offset,
          scale = 1 / factory_calibration$scale
        )
        cal_error_factory <- sqrt(cal_error_factory[, 1]^2 +
          cal_error_factory[, 2]^2 +
          cal_error_factory[, 3]^2)
        cal_error_factory <- mean(abs(cal_error_factory - 1))

        # calculate error from auto-calibration and build output object
        cal_error_auto <- sqrt(sphere_points_cal[, 1]^2 +
          sphere_points_cal[, 2]^2 +
          sphere_points_cal[, 3]^2)
        cal_error_auto <- mean(abs(cal_error_auto - 1))
        auto_calibration <- list(
          scale = scale,
          offset = offset,
          temperature_offset = tempoffset,
          error = cal_error_auto,
          light_denominator = factory_calibration$light_denominator,
          light_numerator = factory_calibration$light_numerator,
          iter = iter
        )

        # update MPI and add note to file history
        MPI$file_history <- rbind(
          MPI$file_history,
          paste0(
            substr(Sys.time(), 0, 23),
            " auto-calibration calculated successfully ",
            "(parameters: ",
            "use_temp = ", use_temp, ", ",
            "spherecrit = ", spherecrit, ", ",
            "maxiter = ", maxiter, ", ",
            "tol = ", tol, ")"
          )
        )
        MPI$auto_calibration <- auto_calibration
        MPI$factory_calibration["error"] <- cal_error_factory
      }
    } else {
      MPI$errors <- rbind(MPI$errors, "Auto-calibration not completed due to insufficient sphere points.")
      warning(paste(basename(binfile_path), ": Auto-calibration not reliable because not enough points on all sides of the sphere."))
      MPI$file_history <- rbind(
        MPI$file_history,
        paste0(
          substr(Sys.time(), 0, 23),
          " auto-calibration calculation attempted and failed"
        )
      )
    }

    # save MPI
    saveRDS(MPI, MPI_filepath)

    return(MPI)
  } else {
    MPI <- NA
    return(MPI)
  }
}



#' Apply Calibration
#'
#' @details Function to apply calibration to sensor-level data from a bin file.
#' @param sensor_data Raw sensor-level data from a bin file in the form
#' (x, y, z, Light, Button, Temp).
#' @param cal_params Calibration parameters for acceleration and
#' light from MPI.
#' @param measurement_device Name of the measurement device used "GENEActiv 1.1"
#' or "GENEActiv 1.2".
#' @param use_temp Allows auto-calibration to be run with and without
#' temperature compensation.
#' @return Data frame of calibrated sensor data.
#' @export
#' @examples
#' cal_params <- list(
#'   scale = c(1.015, 1.017, 1.027),
#'   offset = c(0.00128, 0.0383, 0.0138),
#'   temperature_offset = c(0, 0, 0),
#'   error = NA,
#'   light_denominator = 48,
#'   light_numerator = 911
#' )
#'
#' rawdata <- data.frame(
#'   time = c(rep(1726650857, 5)),
#'   x = c(0.2421875, 0.24609375, 0.25390625, 0.24609375, 0.23828125),
#'   y = c(-0.04296875, -0.04687500, -0.03515625, -0.03125000, -0.04296875),
#'   z = c(-0.9453125, -0.9453125, -0.9531250, -0.9531250, -0.9609375),
#'   light = c(rep(22, 5)),
#'   button = c(rep(0, 5)),
#'   temp = c(rep(21.3, 5)),
#'   volts = c(rep(4.0896, 5))
#' )
#' calibrated <- apply_calibration(rawdata, cal_params, "GENEActiv 1.1")
apply_calibration <- function(sensor_data,
                              cal_params,
                              measurement_device,
                              use_temp = TRUE) {
  # :DEV: - add checks that sensor_data & cal_params are in right format
  min_sensor_data_cols <- c("x", "y", "z", "Temp", "Light")
  measurement_device_options <- c("GENEActiv 1.1", "GENEActiv 1.2")
  data_format_correct <- TRUE

  if (!measurement_device %in% measurement_device_options) {
    warning("Measurement device must be \"GENEActiv 1.1\" or \"GENEActiv 1.2\".")
    data_format_correct <- FALSE
  }

  if (!all(min_sensor_data_cols %in% colnames(sensor_data))) {
    warning("Sensor data must have columns x, y, z, Temp and Light.")
    data_format_correct <- FALSE
  }

  if (data_format_correct == TRUE) {
    if (use_temp == FALSE) {
      sensor_data[, c("x", "y", "z")] <- scale(as.matrix(sensor_data[, c("x", "y", "z")]),
        center = -cal_params$offset, scale = 1 / cal_params$scale
      )
    } else {
      duptemp <- as.matrix(cbind(
        sensor_data[, "Temp"],
        sensor_data[, "Temp"],
        sensor_data[, "Temp"]
      ))
      meantemp <- mean(sensor_data[, "Temp"], na.rm = TRUE)
      sensor_data[, c("x", "y", "z")] <- scale(as.matrix(sensor_data[, c("x", "y", "z")]),
        center = -cal_params$offset, scale = 1 / cal_params$scale
      ) +
        scale(duptemp, center = rep(meantemp, 3), scale = 1 / cal_params$temperature_offset)
    }

    if (measurement_device == "GENEActiv 1.1") {
      sensor_data[, "Light"] <- sensor_data[, "Light"] * cal_params$light_numerator / cal_params$light_denominator
    } else {
      sensor_data[, "Light"] <- ifelse(sensor_data[, "Light"] < 256,
        sensor_data[, "Light"],
        ifelse(sensor_data[, "Light"] < 512, (sensor_data[, "Light"] - 128) * 2,
          ifelse(sensor_data[, "Light"] < 768, (sensor_data[, "Light"] - 320) * 4,
            ifelse(sensor_data[, "Light"] < 1024, (sensor_data[, "Light"] - 656) * 16,
              5888
            )
          )
        )
      )
      sensor_data[, "Light"] <- sensor_data[, "Light"] *
        cal_params$light_numerator / cal_params$light_denominator
    }
  } else {
    warning("Input data not correctly formatted.")
    sensor_data <- NA
  }
  return(sensor_data)
}
