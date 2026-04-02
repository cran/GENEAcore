  #' GENEAbout
  #'
  #' @description GENEAbout function which performs the following tasks for each available day:
  #' \itemize{
  #'  \item Assign bout classifications to events only
  #'  \item Reports aggregated daily activity and sleep measures
  #'  \item Creates COEL v2.0 Behavioural Atoms JSON
  #' }
  #'
  #' @param data_folder Folder that contains raw data bin files to process or path to single bin file.
  #' @param control Named list of optional settings for geneacore functions.
  #' @details
  #' Control options (all optional):
  #' \describe{
  #'  \item{timer}{Logical. Default `FALSE`. Print elapsed times of each processing step.}
  #'  \item{minimum_valid_hours}{Integer. Default 22.
  #'  Minimum hours of wear time in a 24-hour day to be considered a valid day for reported measures.}
  #'  \item{save_daily_bouts}{Logical. Default `FALSE`.
  #'  If `TRUE`,daily bouts are saved as RDS files and CSV files (if `output_csv` is set to `TRUE`).
  #'  If `FALSE`, only a single RDS file containing bouts for all processed days is saved.}
  #'  \item{coel_json}{Logical. Default `FALSE`. Exports bouts as COEL behavioural atoms in JSON format.
  #'  For each bin file in the data folder, behavioural bout atoms and rest activity atoms are produced.
  #'  Additionally, aggregated JSON files are created in the `Batch JSON outputs` subfolder.}
  #'  \item{identifier_mapping_record}{String. Default `NULL`.
  #'  If a file path to the exported `identifier_mapping_record.csv` is provided,
  #'  encrypted participant IDs in the activity and sleep measures will be replaced
  #'  with their corresponding mapped identifiers.}
  #' }
  #'
  #' @examples
  #' \dontrun{
  #' controls <- list(minimum_valid_hours = 4, timer = TRUE, coel_json = TRUE)
  #' geneabout("path/to/folder", controls)
  #' }
  #'
  #' @export
  geneabout <- function(data_folder = data_folder, control = list()) {
    defaults <- list(
      timer = FALSE,
      minimum_valid_hours = 22,
      save_daily_bouts = FALSE,
      coel_json = FALSE,
      identifier_mapping_record = NULL
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
    mpi_summary <- MPI_summary(data_folder)
    if (all(is.na(mpi_summary))) stop("No MPI files found in data folder.")
    total_files <- length(data_files)
    for (seq in 1:total_files) {
      project <- gsub("\\.bin", "", basename(data_files[seq]))
      output_folder <- file.path(data_folder, project)
      UniqueBinFileIdentifier <- mpi_summary$UniqueBinFileIdentifier[which(mpi_summary$BinfileName == data_files[seq])]

      if (length(UniqueBinFileIdentifier) == 0) {
        warning(paste0(basename(data_files[seq]), ": Cannot be processed with geneabout. Not a valid GENEActiv bin file."), call. = FALSE)
        next
      }
      if (is.na(UniqueBinFileIdentifier)) {
        warning(paste0(basename(data_files[seq]), ": Cannot be processed with geneabout. See MPI for error details."), call. = FALSE)
        next
      }
      events_list <- list.files(output_folder, pattern = paste0(UniqueBinFileIdentifier, "_day[0-9]+_events\\.rds"))
      if (length(events_list) == 0) {
        warning(paste0(basename(data_files[seq]), ": Cannot be processed with geneabout. No daily events found."), call. = FALSE)
        next
      }
      day_numbers <- sort(as.integer(sub(".*_day([0-9]+)_events\\.rds$", "\\1", events_list)))

      message(paste("Running geneabout for file", seq, "of", length(data_files), ":", project))
      dir.create(file.path(data_folder, "GENEAbout"), showWarnings = FALSE)

      all_bouts <- vector("list", max(0, day_numbers))

      load_time <- system.time({
        for (day_number in day_numbers) {
          message(paste("Processing Day", day_number))
          events_df <- readRDS(file.path(
            output_folder,
            paste0(UniqueBinFileIdentifier, "_day", day_number, "_events.rds")
          ))
          bouts <- bouts_decision_tree(events_df, 5.7e-5, 0.0625, 0.407)

          all_bouts[[day_number]] <- bouts

          if (ctrl$save_daily_bouts && !is.null(bouts)) {
            output_location <- file.path(output_folder, paste0(UniqueBinFileIdentifier, "_day", day_number, "_bouts"))
            saveRDS(bouts, paste0(output_location, ".rds"))
            write.csv(round_columns(bouts), file = paste0(output_location, ".csv"), row.names = FALSE)
          }
        }
      })
      if (ctrl$timer) print(paste0("Total bouts creation time: ", round(load_time["user.self"] + load_time["sys.self"], 2), " seconds"))

      all_bouts_df <- do.call(rbind, all_bouts)
      if (!is.null(all_bouts_df)) {
        output_location <- file.path(output_folder, paste0(UniqueBinFileIdentifier, "_bouts"))
        saveRDS(all_bouts_df, paste0(output_location, ".rds"))
        write.csv(round_columns(all_bouts_df), file = paste0(output_location, ".csv"), row.names = FALSE)
      }

      report_time <- system.time({
        single_file_measures(output_folder,
                             UniqueBinFileIdentifier,
                             measure_type = "both",
                             minimum_valid_hours = ctrl$minimum_valid_hours,
                             include_participant_info = TRUE,
                             identifier_mapping_record = ctrl$identifier_mapping_record)
      })
      if (ctrl$timer) print(paste0("Total daily measures creation time: ", round(report_time["user.self"] + report_time["sys.self"], 2), " seconds"))
    }

    if (ctrl$coel_json) create_coel_atoms_json(data_folder)
  }
