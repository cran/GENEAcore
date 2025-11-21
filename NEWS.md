# GENEAcore 1.1.2

## New features
* Added overloads for `get_UniqueBinFileIdentifier()` to enable it to also read a filename or a directory path

# GENEAcore 1.1.1

## New features
* The StepDiff measure is now included in outputs as default when `output_steps = TRUE`
* Create a bin file summary from `geneacore()` by passing parameter `summary = TRUE`
* `geneacore()` can now take either a path to a directory of bin files or a path to a single bin file
* MPI file_history now records parameters passed during function calls
* MPI now records the GENEAcore package version used during execution
* A new MPI is created if package version is not listed in the existing MPI
* New DateTimeUTC column has been added to the events and epochs CSV files
* Button press (event marker) timestamps are now recorded in the MPI
* A new MPI is created when a version number does 

## Minor improvements
* Removed warning message when MPI already exists
* Warning messages added for deprecated functions

## Bug fixes
* `geneacore()` steps error resolved when there are not enough events in the day
* `geneacore()` events are created when a day has only a single transition (when non-wear spans across multiple days)

# GENEAcore 1.1.0

## New features
* `detect_nonmovement()` uses a new sd_threshold value
*	`detect_transitions()` uses new default changepoint penalty values
* `apply_all()` function applies AGSA, ENMO, UpDown and Degrees measures in a single function
* `detect_transitions()` runs changepoint only if a different 24-hour cut time is set 
* `sample_binfile()` has new parameter option to allow raw sample RDS to be saved 
* `detect_transitions()` removes short changeover transition between days 
* Partial epochs are removed from aggregation output
* Aggregated events and epochs output have added day index column and AGSA.sd and ENMO.sd columns have been removed
* Standardised format of start time and duration for still bouts and non-wear bouts
* Certainty field removed from non-wear bouts
* MPI reports first and last timestamps as integer values

## Minor improvements
* `sample_binfile()` outputs an improved message with time zone offset when reading subset of file 

## Bug fixes
* `detect_transitions()` now applies a minimum 5 seconds of data for a valid day
* Event start time and transitions timestamps now match

## Breaking changes
* Updated naming conventions for MPI outputs, parameters and functions, see breaking-changes.html

# GENEAcore 1.0.1

Released to CRAN
