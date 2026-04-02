#' Non-wear and Rest Coverage for Epochs/Events
#'
#' @param MPI Measurement Period Information.
#' @param aggregated_data Aggregated epochs or events.
#' @param start_time Time stamp to start the coverage assignment, defaults to first time stamp of aggregated data object passed.
#' @param end_time Time stamp to end the coverage assignment, defaults to end time stamp of aggregated data object passed.
#' @param nonwear_day Data frame of non-wear periods and their durations.
#' @keywords internal
#' @export
nonwear_rest_coverage <- function(MPI,
                                  aggregated_data,
                                  start_time = NULL,
                                  end_time = NULL,
                                  nonwear_day) {
  if (is.null(start_time) || is.null(end_time)) {
    start_time <- aggregated_data$TimeUTC[1]
    end_time <- aggregated_data$TimeUTC[nrow(aggregated_data)] + aggregated_data$Duration[nrow(aggregated_data)]
  }

  transitions <- data.frame(time_UTC = c(aggregated_data$TimeUTC, aggregated_data$TimeUTC[nrow(aggregated_data)] + aggregated_data$Duration[nrow(aggregated_data)]),
                            index = c(1, cumsum(aggregated_data$Duration) + 1))

  # create non-wear time by bout
  nonwear <- nonwear_day[nonwear_day$nonwear_start < end_time &
    nonwear_day$nonwear_end > start_time, ]

  nonwear$nonwear_start <- pmax(nonwear$nonwear_start, start_time)
  nonwear$nonwear_end   <- pmin(nonwear$nonwear_end,   end_time)

  nonwear$MPI_nonwear_duration <-
    nonwear$nonwear_end - nonwear$nonwear_start

  if (nrow(nonwear) != 0) {
    one_sequence <- data.frame(
      start_time = nonwear$nonwear_start,
      lengths = nonwear$MPI_nonwear_duration,
      values = 1
    )

    wear_start <- append(
      start_time,
      (
        nonwear$nonwear_start + nonwear$MPI_nonwear_duration
      )
    )
    wear_end <- append(
      nonwear$nonwear_start,
      end_time
    )

    zero_sequence <- data.frame(
      start_time = wear_start,
      lengths = wear_end - wear_start,
      values = 0
    )
    non_wear_sequence <- rbind(zero_sequence, one_sequence)
    non_wear_sequence <- non_wear_sequence[order(non_wear_sequence$start_time), ]
    non_wear_sequence <- subset(non_wear_sequence, select = -start_time)
    non_wear_sequence <- inverse.rle(non_wear_sequence)
  } else {
    non_wear_sequence <- rep(
      0,
      end_time - start_time
    )
  }

  rest_intervals <- MPI$non_movement$rest_intervals[MPI$non_movement$rest_intervals$start_time >= start_time & MPI$non_movement$rest_intervals$start_time + MPI$non_movement$rest_intervals$duration <= end_time, ]

  if (nrow(rest_intervals) != 0) {
    one_sequence <- data.frame(
      start_time = rest_intervals$start_time,
      lengths = rest_intervals$duration,
      values = 1
    )
    move_start <- append(
      start_time,
      (
        rest_intervals$start_time + rest_intervals$duration
      )
    )
    move_end <- append(
      rest_intervals$start_time,
      end_time
    )
    zero_sequence <- data.frame(
      start_time = move_start,
      lengths = move_end - move_start,
      values = 0
    )
    rest_sequence <- rbind(zero_sequence, one_sequence)
    rest_sequence <- rest_sequence[order(rest_sequence$start_time), ]
    rest_sequence <- subset(rest_sequence, select = -start_time)
    rest_sequence <- inverse.rle(rest_sequence)
  } else {
    rest_sequence <- rep(
      0,
      end_time - start_time
    )
  }

  # allocate non-wear & rest interval time to aggregated data
  bout_sequence <- data.frame(
    lengths = diff(transitions$time_UTC),
    values = seq(1, length(diff(
      transitions$time_UTC
    )))
  )

  transitions <- transitions[-nrow(transitions), ]

  if (nrow(transitions) > 1) {
    bout_sequence <- rbind(
      bout_sequence,
      c(
        (end_time - start_time - sum(bout_sequence$lengths)),
        nrow(transitions)
      )
    )

    bout_sequence <- inverse.rle(bout_sequence)
  } else {
    bout_sequence <- rep(
      1,
      end_time - start_time
    )
  }


  bout_sequence <- data.frame(bout_sequence, non_wear_sequence, rest_sequence)
  bout_non_wear <- aggregate(
    data = bout_sequence,
    non_wear_sequence ~ bout_sequence,
    FUN = sum
  )

  bout_rest <- aggregate(
    data = bout_sequence,
    rest_sequence ~ bout_sequence,
    FUN = sum
  )

  aggregated_data$nonwear.time <- bout_non_wear$non_wear_sequence
  aggregated_data$rest.time <- bout_rest$rest_sequence

  # for partial epochs
  aggregated_data$nonwear.time <-  pmin(aggregated_data$nonwear.time, aggregated_data$Duration)
  aggregated_data$rest.time <-  pmin(aggregated_data$rest.time, aggregated_data$Duration)

  return(aggregated_data)
}

#' Determine Daily Recommended Primary Rest Interval
#' @details Twice the expansion before than after.
#' @param start_expansion_percent The percentage expansion, based on duration, of a still period's start time to identify overlaps with near-adjacent still periods.
#' @param end_expansion_percent The percentage expansion, based on duration, of a still period's end time to identify overlaps with near-adjacent still periods.
#' @param cut_time_24hr Time in 24h to split days up by.
#' @param MPI Measurement Period Information.
#' @keywords internal
#' @export
find_rest_intervals <- function(start_expansion_percent, end_expansion_percent, cut_time_24hr, MPI) {
  still_bouts <- MPI[["non_movement"]][["still_bouts"]]
  non_wear <- MPI[["non_movement"]][["non_wear"]]
  non_wear$end <- non_wear$start + non_wear$duration

  still_bouts$end_time <- still_bouts$start_time + still_bouts$duration
  still_bouts$start_adj <- still_bouts$start_time - floor(still_bouts$duration * start_expansion_percent / 100)
  still_bouts$end_adj <- still_bouts$start_time + still_bouts$duration + floor(still_bouts$duration * end_expansion_percent / 100)

  result <- data.frame()
  cut_times <- get_cut_times(cut_time_24hr, MPI)

  is_in_non_wear <- function(start_time, end_time, non_wear) {
    any(
      (start_time >= non_wear$start & start_time <= non_wear$end) |
        (end_time >= non_wear$start & end_time <= non_wear$end) |
        (start_time <= non_wear$start & end_time >= non_wear$end)
    )
  }

  for (day_number in 1:(length(cut_times) - 1)) {
    day_start <- cut_times[day_number]
    day_end <- cut_times[day_number + 1]

    day_still_bouts <- still_bouts[(still_bouts$start_time >= day_start &
      still_bouts$end_time < day_end), ]
    day_still_bouts$StartTime <- as.POSIXct(day_still_bouts$start_time, origin = "1970-01-01")
    day_still_bouts$EndTime <- as.POSIXct(day_still_bouts$end_time, origin = "1970-01-01")
    day_still_bouts$StartAdj <- as.POSIXct(day_still_bouts$start_adj, origin = "1970-01-01")
    day_still_bouts$EndAdj <- as.POSIXct(day_still_bouts$end_adj, origin = "1970-01-01")
    if (nrow(day_still_bouts) > 0) {
      day_still_bouts <- day_still_bouts[order(day_still_bouts$start_time), ]
      day_still_bouts$group <- 1
      if (nrow(day_still_bouts) > 1) {
        for (row in 2:nrow(day_still_bouts)) {
          current_start <- day_still_bouts$start_adj[row]
          current_end <- day_still_bouts$end_adj[row]
          prev_end <- day_still_bouts$end_adj[row - 1]

          # Check if current interval touches a non_wear period
          if (is_in_non_wear(current_start, current_end, non_wear)) {
            # print(paste(
            #   MPI$file_data$BinfileName,
            #   "Day",
            #   day_number,
            #   "Row",
            #   row
            # ))
            # Start a new group due to overlap with non_wear
            day_still_bouts$group[row] <- day_still_bouts$group[row - 1] + 1
          } else if (current_start <= prev_end) {
            # Overlaps with previous bout and no non_wear, place in same group
            day_still_bouts$group[row] <- day_still_bouts$group[row - 1]
            day_still_bouts$end_adj[row] <- max(current_end, prev_end)
          } else {
            # No overlap, place in new group
            day_still_bouts$group[row] <- day_still_bouts$group[row - 1] + 1
          }
        }
      }

      day_still_bouts$end_time <- day_still_bouts$start_time + day_still_bouts$duration
      group_starts <- aggregate(start_time ~ group, data = day_still_bouts, FUN = min)
      group_ends <- aggregate(end_time ~ group, data = day_still_bouts, FUN = max)

      rest_bouts <- data.frame(
        start = group_starts$start_time,
        end = group_ends$end_time,
        duration = group_ends$end_time - group_starts$start_time
      )

      if (nrow(rest_bouts) > 0) {
        primary <- rest_bouts[which.max(rest_bouts$duration), ]
        result <- rbind(
          result,
          data.frame(
            start_time = primary$start,
            duration = primary$duration,
            day_number = day_number
          )
        )
      }
    }
  }

  MPI$non_movement[["rest_intervals"]] <- result
  return(MPI)
}

#' Bouts Decision Tree
#'
#' @param bouts Aggregated events after non-wear and rest coverage have been assigned.
#' @param SDduration_threshold Threshold for sleep classification. Uses the average x,y,z standard deviation over duration metric.
#' @param AGSA_threshold Threshold for active classification.
#' @returns Classified bouts
#' @keywords internal
#' @export
bouts_decision_tree <- function(bouts,
                                SDduration_threshold = 5.7e-5,
                                AGSA_threshold = 0.0625,
                                running_threshold = 0.407) {
  bouts$sd_over_dur <- (bouts$xSD + bouts$ySD + bouts$zSD) / (3 * bouts$Duration)

  # decision tree thresholds
  bouts$non_wear <- ifelse(bouts$nonwear.time > bouts$Duration / 2, TRUE, FALSE)
  bouts$rest <- ifelse(bouts$rest.time > bouts$Duration / 2, TRUE, FALSE)
  bouts$active <- ifelse(bouts$AGSAMean > AGSA_threshold, TRUE, FALSE)
  bouts$ambulatory <- ifelse((bouts$Duration > 9) & (bouts$StepMean > 45), TRUE, FALSE)
  bouts$vigourous <- ifelse(bouts$AGSAMean > running_threshold, TRUE, FALSE)
  bouts$fastwalk <- ifelse(bouts$StepMean > 70, TRUE, FALSE)
  bouts$sleep <- ifelse(bouts$sd_over_dur < SDduration_threshold, TRUE, FALSE)

  # bout classification
  bouts$classification <- ifelse(bouts$non_wear, "non_wear",
    ifelse(bouts$active,
      ifelse(bouts$ambulatory,
        ifelse(bouts$vigourous, "run",
          ifelse(bouts$fastwalk, "fast_walk",
            "slow_walk"
          )
        ),
        "active"
      ),
      ifelse(bouts$sleep, "sleep", "sedentary")
    )
  )

  new_names <- c(
    nonwear.time = "NonWearTime",
    rest.time = "RestTime",
    sd_over_dur = "SDOverDuration",
    non_wear = "NonWear",
    rest = "Rest",
    active = "Active",
    ambulatory = "Ambulatory",
    vigourous = "Vigorous",
    fastwalk = "FastWalk",
    sleep = "Sleep",
    classification = "Classification"
  )

  names(bouts)[names(bouts) %in% names(new_names)] <-
    new_names[names(bouts)[names(bouts) %in% names(new_names)]]

  return(bouts)
}
