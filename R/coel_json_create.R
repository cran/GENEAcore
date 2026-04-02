# ==============================================================================
# Recommended user workflow (how to create per-participant Atoms and a payload)
# ==============================================================================
#
# 1) For each participant, obtain:
#    - mpi_data (from GENEAcore output: *_MPI.rds)
#    - bouts_df (from GENEAcore output: *_bouts.rds) for Behavioural Bout Model stream
#
# 2) Build and write per-participant Atom JSON files, one file per stream per participant.
#    Example patterns:
#      out_dir/<ParticipantID>_rest_activity_atoms.json
#      out_dir/<ParticipantID>_behavioural_bout_atoms.json
#
# 3) Combine those per-participant JSON files into a single payload JSON (+ optional gzip).
# ==============================================================================

#' Create COEL Behavioural Atoms JSON
#'
#' @description Creates per-participant rest activity and behavioural bout atoms and a payload
#' @param data_folder Folder that contains raw data bin files for which bouts have been created for.
#' @export
#' @importFrom utils read.csv
create_coel_atoms_json <- function(data_folder) {
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

    UniqueBinFileIdentifier <- mpi_summary$UniqueBinFileIdentifier[which(mpi_summary$BinfileName == data_files[seq])]

    if (length(UniqueBinFileIdentifier) == 0 || all(is.na(UniqueBinFileIdentifier))) {
      next
    }

    MPI <- readRDS(file.path(
      output_folder,
      paste0(UniqueBinFileIdentifier, "_MPI.rds")
    ))

    bouts_file <- file.path(
      output_folder,
      paste0(UniqueBinFileIdentifier, "_bouts.rds")
    )

    if (file.exists(bouts_file)) {
      bouts <- readRDS(bouts_file)
    } else {
      warning(sprintf("%s has no bouts outputs. No COEL atoms created.", file.path(output_folder, "GENEAbout")))
      next
    }

    dir.create(file.path(data_folder, "GENEAbout"), showWarnings = FALSE)
    # --- Rest Activity Model v1.0 stream from MPI only ----------------------------

    # Load mapping CSV (Rest Activity -> COEL Model v2.0)
    rest_map <- read.csv(
      system.file("extdata", "rest-activity-model-v1.0-to-coel-model-v2.0.csv", package = "GENEAcore"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    # Build contiguous rest-activity events from MPI
    rest_events_df <- build_rest_activity_events_from_mpi(MPI)

    # Build Atom JSON
    rest_atoms_json <- build_coel_atoms_json(
      events_df = rest_events_df,
      mpi_data = MPI,
      output_folder = file.path(data_folder, "GENEAbout"),
      coel_map = rest_map,
      environment_id = NULL,
      evidence_type = 7,
      classification_model = "Rest Activity Model v1.0",
      classification_model_iri = "https://w3id.org/coel/models/activinsights/rest_activity/1.0/",
      include_extension = FALSE,
      report_time = FALSE
    )

    # --- Behavioural Bout Model v1.0 stream (requires bouts_df already computed) ---
    bout_map <- read.csv(
      system.file("extdata", "behavioural-bout-model-v1.0-to-coel-model-v2.0.csv", package = "GENEAcore"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    names(bouts)["Classification" %in% names(bouts)] <- "behavioural_bouts"

    bout_atoms_json <- build_coel_atoms_json(
      events_df = bouts,
      mpi_data = MPI,
      output_folder = file.path(data_folder, "GENEAbout"),
      coel_map = bout_map,
      environment_id = NULL,
      evidence_type = 7,
      classification_model = "Behavioural Bout Model v1.0",
      classification_model_iri = "https://w3id.org/coel/models/activinsights/behavioural_bout/1.0/",
      include_extension = TRUE,
      extension_registry_iri = "https://w3id.org/coel/atom/2.0/extension-registry.csv",
      report_time = FALSE
    )
  }

  # Combine per-participant files into a payload ---------------------------------
  if (length(list.files(file.path(data_folder, "GENEAbout"), pattern = "atoms.json")) > 0) {
    combined_json_path <- combine_atoms_to_payload(
      in_dir = file.path(data_folder, "GENEAbout"),
      out_json_path = file.path(data_folder, "GENEAbout", "Batch JSON outputs", "coel_behavioural_bouts_atoms_payload.json"),
      pattern = "behavioural_bouts_atoms.json$",
      gzip = TRUE,
      recursive = TRUE
    )

    combined_json_path <- combine_atoms_to_payload(
      in_dir = file.path(data_folder, "GENEAbout"),
      out_json_path = file.path(data_folder, "GENEAbout", "Batch JSON outputs", "coel_rest_activity_atoms_payload.json"),
      pattern = "rest_activity_atoms.json$",
      gzip = TRUE,
      recursive = TRUE
    )
  }
}
