## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE---------------------------------------------------------------
library(knitr)

function_changes <- data.frame(
  Old = c(
    "aggregateEpochs",
    "aggregateEvents",
    "aggregatePeriods",
    "createEventMapping",
    "stepCounter"
  ),
  New = c(
    "aggregate_epochs",
    "aggregate_events",
    "aggregate_periods",
    "create_event_mapping",
    "step_counter"
  )
)

colnames(function_changes) <- c("v1.0.1", "v1.1.0")

kable(function_changes)

## ----echo = FALSE-------------------------------------------------------------
parameter_changes <- data.frame(
  Function = c(
    "geneacore",
    "detect_transitions",
    "check_time_format",
    "step_counter",
    "step_counter",
    "step_counter"
  ),
  Old = c(
    "CutTime24Hr",
    "CutTime24Hr",
    "CutTime24Hr",
    "StepData",
    "samplefreq",
    "filterorder"
  ),
  New = c(
    "cut_time_24hr",
    "cut_time_24hr",
    "cut_time_24hr",
    "step_data",
    "sample_frequency",
    "filter_order"
  )
)

colnames(parameter_changes) <- c("Function", "v1.0.1", "v1.1.0")

kable(parameter_changes)

## ----echo=FALSE---------------------------------------------------------------
changes <- data.frame(
  Section = c(
    rep("line_numbers", 8),
    rep("measurement_numbers", 5),
    rep("file_info", 7),
    rep("file_data", 5),
    rep("factory_calibration", 3),
    rep("still_bouts", 2),
    rep("non_wear", 1),
    rep("auto_calibration", 3),
    rep("transitions", 2)
  ),
  Old = c(
    "extractinfo", "subjectinfo", "calibrationdata", "memorystatus",
    "firstpage", "lastline", "lastpage", "lastpage",
    "firstsecond", "firstminute", "firsthour", "firstUTCday", "firstlocalday",
    "pagecount", "decimalseparator", "firsttimestamp", "lasttimestamp",
    "halfsecondstart", "numbermeasurements", "numbermeasurements",
    "ConfigTime", "ExtractTime", "FirstLocalMidnightTime", "MeasurementEndTime", "MeasurementStartTime",
    "temperatureoffset", "lightdenominator", "lightnumerator",
    "end_time", "num_events",
    "end_time",
    "temperatureoffset", "lightdenominator", "lightnumerator",
    "timestamp", "day"
  ),
  New = c(
    "extract_info", "subject_info", "calibration_data", "memory_status",
    "first_page", "last_line", "last_page", "last_page",
    "first_second", "first_minute", "first_hour", "first_UTC_day", "first_local_day",
    "page_count", "decimal_separator", "first_time_UTC", "last_time_UTC",
    "half_second_start", "number_measurements", "number_measurements",
    "ConfigTimeUTC", "ExtractTimeUTC", "FirstLocalMidnightTimeUTC", "MeasurementEndTimeUTC", "MeasurementStartTimeUTC",
    "temperature_offset", "light_denominator", "light_numerator",
    "removed", "number_events",
    "removed",
    "temperature_offset", "light_denominator", "light_numerator",
    "time_UTC", "day_number"
  )
)

changes$New <- ifelse(
  changes$New == "removed",
  "<span style='color:red;'>removed</span>",
  changes$New
)

colnames(changes) <- c("Section", "v1.0.1", "v1.1.0")

knitr::kable(changes, escape = FALSE, format = "html")

