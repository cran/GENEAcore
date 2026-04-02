#' GENEAcore Part 4
#'
#' @description GENEAcore function which performs the following tasks:
#' \itemize{
#'  \item Consolidates daily epochs or bouts outputs into a single file
#' }
#'
#' @param data_folder Folder that contains raw data bin files to process or path to single bin file.
#' @param control Named list of optional settings for geneacore functions.
#' @details
#' Control options (all optional):
#' \describe{
#'  \item{output_epochs}{Logical. Default `TRUE`. Create epoch outputs.}
#'  \item{epoch_duration}{Integer. Default 1. Specify duration of fixed epochs.}
#'  \item{output_events}{Logical. Default `TRUE`. Create event outputs.}
#'  \item{output_csv}{Logical. Default `FALSE`. Allows CSV output to be saved during epoch and event processing.}
#' }
#'
#' @examples
#' \dontrun{
#' controls <- list(output_csv = TRUE, minimum_valid_hours = 4, timer = TRUE)
#' geneacore_part4("path/to/folder", controls)
#' }
#'
#' @returns Consolidated epochs and/or bouts output (.rds/.csv)
#' @export
#' @importFrom utils write.csv
geneacore_part4 <- function(data_folder = data_folder, control = list()) {
  defaults <- list(
    output_epochs = TRUE,
    epoch_duration = 1,
    output_events = TRUE,
    output_csv = FALSE
  )
  ctrl <- merge_control(defaults, control)
  ## Add: check that MPI and Raw sample RDS files exist
  if (file.info(data_folder)$isdir) {
    data_files <- list.files(data_folder, pattern = "(?i)\\.bin$")
    if (length(data_files) == 0) {
      stop(sprintf("No .bin files found in directory: %s", data_folder))
    }
  } else {
    if (!grepl("(?i)\\.bin$", data_folder)) {
      stop(sprintf("The file '%s' does not have a .bin extension.", data_folder))
    }
    # Process a single file
    data_files <- basename(data_folder)
    data_folder <- dirname(data_folder)
  }
  total_files <- length(data_files)
  mpi_summary <- MPI_summary(data_folder)
  for (seq in 1:total_files) {
    project <- gsub("\\.bin", "", basename(data_files[seq]))
    output_folder <- file.path(data_folder, project)

    UniqueBinFileIdentifier <- mpi_summary$UniqueBinFileIdentifier[mpi_summary$BinfileName == data_files[seq]]

    if (ctrl$output_epochs) {
      daily_output_files <- list.files(output_folder,
        pattern = paste0(UniqueBinFileIdentifier, "_day[0-9]+_epochs_[0-9]+s\\.rds"),
        full.names = TRUE
      )

      if (length(daily_output_files > 0)) {
        # Read all CSV files into a list of data.frames
        list_of_dfs <- lapply(daily_output_files, readRDS)
        combined_df <- do.call(rbind, list_of_dfs)
        combined_df <- combined_df[order(combined_df$TimeUTC), ]

        output_location <- file.path(output_folder, paste0(UniqueBinFileIdentifier, "_epochs_", ctrl$epoch_duration, "s.rds"))
        saveRDS(combined_df, output_location)
        if (ctrl$output_csv) write.csv(round_columns(combined_df), file = sub(".rds", ".csv", output_location), row.names = FALSE)
      }
    }

    if (ctrl$output_events) {
      daily_output_files <- list.files(output_folder,
        pattern = paste0(UniqueBinFileIdentifier, "_day[0-9]+_events\\.rds"),
        full.names = TRUE
      )

      if (length(daily_output_files > 0)) {
        # Read all CSV files into a list of data.frames
        list_of_dfs <- lapply(daily_output_files, readRDS)
        combined_df <- do.call(rbind, list_of_dfs)
        combined_df <- combined_df[order(combined_df$TimeUTC), ]

        output_location <- file.path(output_folder, paste0(UniqueBinFileIdentifier, "_events.rds"))
        saveRDS(combined_df, output_location)
        if (ctrl$output_csv) write.csv(round_columns(combined_df), file = sub(".rds", ".csv", output_location), row.names = FALSE)
      }
    }
  }
}
