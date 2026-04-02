#' GENEAcore Main Wrapper
#'
#' @description GENEAcore function that wraps the functionalities of geneacore_part1 to geneacore_part4.
#' GENEAcore function which performs the following tasks for each valid day:
#' \itemize{
#'  \item Checks and reads the data file and creates the Measurement Period Information (MPI)
#'  \item Downsamples the file to 1Hz and detects periods of non movement
#'  \item Calculates auto calibration parameters for the device
#'  \item Samples daily raw bin file data and saves as daily RDS files
#'  \item Reads and saves button press timestamps to the Measurement Period Information (MPI)
#'  \item Applies calibration parameters to raw data
#'  \item Identifies transitions for events processing only
#'  \item Performs aggregation by event and/or epoch
#'  \item Calculate steps, if option selected
#'  \item Calculate non-wear coverage and rest coverage of events and/or epochs
#'  \item Assign bout classifications to events only
#'  \item Reports aggregated daily activity and sleep measures
#'  \item Consolidates daily epochs or bouts outputs into a single file
#' }
#'
#' @param data_folder Folder that contains raw data bin files to process or path to single bin file.
#' @param control Named list of optional settings for geneacore functions.
#' @details
#' Control options (all optional):
#' \describe{
#'  \item{cut_time_24hr}{String. Default "15:00". Time in 24h to split days up by.}
#'  \item{output_epochs}{Logical. Default `FALSE`. Create epoch outputs.}
#'  \item{epoch_duration}{Integer. Default 1. Specify duration of fixed epochs.}
#'  \item{output_events}{Logical. Default `TRUE`. Create event outputs.}
#'  \item{output_steps}{Logical. Default `FALSE`. Calculate step counts and stepping rate during epoch processing.
#'  Steps are always calculated during events processing.}
#'  \item{output_csv}{Logical. Default `FALSE`. Allows CSV output to be saved during epoch and event processing.}
#'  \item{timer}{Logical. Default `FALSE`. Print elapsed times of each processing step.}
#'  \item{multisession}{Integer vector of length 2. Default `c(1,1)`.
#'  Controls day split when running function across multiple R sessions.
#'  The first value is the current session number and the second is the total number of sessions.
#'  The default processes all days in a single session.}
#'  \item{required_processing_hours}{Integer. Default 0. Hours of wear time in a 24-hour day to be considered a valid day to be processed.}
#' }
#'
#' @examples
#' \dontrun{
#' controls <- list(output_csv = TRUE, required_processing_hours = 4, timer = TRUE)
#' geneacore("path/to/folder", controls)
#' }
#'
#' @export
geneacore <- function(data_folder = data_folder, control = list()) {
  defaults <- list(
    cut_time_24hr = "15:00",
    output_epochs = FALSE,
    epoch_duration = 1,
    output_events = TRUE,
    output_steps = FALSE,
    output_csv = FALSE,
    timer = FALSE,
    multisession = c(1, 1),
    required_processing_hours = 0
  )
  control <- merge_control(defaults, control)

  geneacore_part1(data_folder = data_folder, control = control)
  geneacore_part2(data_folder = data_folder, control = control)
  geneacore_part3(data_folder = data_folder, control = control)
}
