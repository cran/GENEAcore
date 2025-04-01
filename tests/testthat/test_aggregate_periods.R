library(testthat)
# source( "../functions/aggregateEpochs.R")
# Expected values calculated within expectedCalculations.xlsx
local({
  # test with a decimal frequency
  timestamp <- c(
    100, 100.300003, 100.600006, 100.900009, 101.200012,
    101.500015, 101.800018, 102.100021, 102.400024, 102.700027,
    103.00003, 103.300033, 103.600036, 103.900039, 104.200042,
    104.500045, 104.800048, 105.100051, 105.400054, 105.700057,
    106.00006, 106.300063, 106.600066, 106.900069, 107.200072,
    107.500075, 107.800078, 108.100081, 108.400084, 108.700087,
    109.00009, 109.300093, 109.600096, 109.900099, 110.200102,
    110.500105, 110.800108, 111.100111, 111.400114, 111.700117
  )
  value <- c(
    2.796221556, 2.943475465, 0.078141391, 1.477123618, 0.433804919,
    0.14267638, 1.258884305, 0.733577225, 2.470809082, 1.471389038,
    2.55269315, 1.049893434, 0.028923353, 0.248485573, 0.094099557,
    2.081051666, 2.802255044, 0.375865493, 2.897413932, 2.778240804,
    0.605294183, 1.737795484, 0.446345263, 2.086556598, 2.660049359,
    1.771442781, 1.894041324, 2.574302217, 1.535151599, 2.651439127,
    2.685176996, 1.671938897, 2.967045045, 1.489973675, 1.836135594,
    2.139518605, 1.260516305, 1.754813504, 0.157902535, 2.295043666
  )
  expected_sum <- c(
    9.130327634, 8.555770856, 11.02892649, 11.20152499,
    15.57502756, 9.443930208
  )
  duration <- rep(c(2.100021, 2.100021, 1.800018), 2)
  expected <- data.frame(
    timestamp = timestamp[c(1, 8, 15, 21, 28, 35)],
    epoch_number = c(1:length(expected_sum)),
    value = expected_sum,
    duration = duration
  )
  aggregated_decimal_frequency <- aggregateEpochs(data.frame(timestamp = timestamp, value = value),
    duration = 2,
    measure = "value",
    sample_frequency = 3.3333,
    first_epoch_timestamp = 100,
    time = "timestamp",
    fun = sum
  )
  test_that("Sum with decimal frequency", {
    expect_equal(aggregated_decimal_frequency,
      expected,
      tolerance = 1e-5
    )
  })

  timestamp <- c(
    1619424000, 1619424001, 1619424002, 1619424003, 1619424004, 1619424005,
    1619424006, 1619424007, 1619424008, 1619424009, 1619424010, 1619424011,
    1619424012, 1619424013, 1619424014, 1619424015, 1619424016, 1619424017,
    1619424018, 1619424019, 1619424020, 1619424021, 1619424022, 1619424023,
    1619424024, 1619424025, 1619424026, 1619424027, 1619424028, 1619424029,
    1619424030, 1619424031, 1619424032
  )
  value <- c(
    0.729614366, 1.729115871, 0.804973546, 2.510181118, 2.23764038, 0.613203747,
    0.681953275, 0.089566943, 0.021042388, 2.4780338, 2.437488989, 2.632635727,
    2.686591681, 2.524958159, 1.821487006, 0.922968348, 1.658441493, 1.246719925,
    1.586168967, 2.365448368, 1.182042439, 1.735395123, 1.75151822, 1.581427968,
    2.55581665, 2.315657647, 1.736543656, 1.446465419, 2.474537822, 0.056360359,
    2.806630703, 1.31775029, 2.733245822
  )
  value2 <- c(
    0.022063192, 0.433119837, 0.886914128, 0.34247643, 0.93005937,
    0.838909727, 0.353414786, 0.028023234, 0.847933623, 0.615646043,
    0.593610734, 0.123124169, 0.411839584, 0.041831889, 0.744427145,
    0.40424696, 0.057025588, 0.207240283, 0.842408053, 0.011284519,
    0.943834754, 0.430070371, 0.673758579, 0.780404774, 0.45119477,
    0.009577175, 0.12071847, 0.199551839, 0.705354735, 0.170748985,
    0.296666811, 0.640369777, 0.867175777
  )
  epoch <- c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 6, 6, 6)
  data <- data.frame(timestamp, value)

  epoch <- c(1, 2, 3, 4, 5, 6)
  expected_mean <- c(0.776760031, 2.420632312, 1.55594942, 1.76124008, 1.605912981, 2.285875605)
  expected_times <- timestamp[c(6, 11, 16, 21, 26, 31)]
  expected <- data.frame(
    timestamp = expected_times, epoch_number = epoch,
    value = expected_mean, duration = c(rep(5, 5), 3)
  )
  rownames(expected) <- seq(2, 7) # The first row is droped as it is before the start time

  aggregated <- aggregateEpochs(data,
    duration = 5,
    measure = "value",
    sample_frequency = 1,
    first_epoch_timestamp = 1619424005,
    time = "timestamp"
  )

  test_that("aggregateEpochs calculation", {
    expect_equal(aggregated, expected)
    expect_equal(colnames(aggregated), c("timestamp", "epoch_number", "value", "duration"))
  })

  ## test when epochs start at index 1
  epoch2 <- c(1:11)
  data2 <- data.frame(timestamp, value, value2)

  expected_mean <- c(
    1.087901261, 1.787008415, 0.264187535, 2.516052839, 2.344345615,
    1.276043255, 1.711219925, 1.689447104, 2.202672651, 1.325787867,
    2.285875605
  )

  expected_mean_df <- data.frame(
    timestamp = timestamp[seq(from = 1, to = 31, by = 3)],
    epoch_number = epoch2,
    value = expected_mean,
    duration = rep(3, 11)
  )
  test_that("aggregateEpochs starting at index 1", {
    expect_equal(
      aggregateEpochs(data2,
        duration = 3,
        measure = "value",
        sample_frequency = 1,
        first_epoch_timestamp = 1619424000,
        time = "timestamp"
      ),
      expected_mean_df
    )
  })
  # Test with sum function
  expected_sum <- c(
    3.263703783, 5.361025245, 0.792562606, 7.548158516, 7.033036846,
    3.828129766, 5.133659774, 5.068341312, 6.608017953, 3.9773636,
    6.857626815
  )
  expected_sum_df <- data.frame(
    timestamp = timestamp[seq(from = 1, to = 31, by = 3)],
    epoch_number = epoch2,
    value = expected_sum,
    duration = rep(3, 11)
  )
  test_that("aggregateEpochs by sum starting at index 1", {
    expect_equal(
      aggregateEpochs(data2,
        duration = 3,
        measure = "value",
        sample_frequency = 1,
        first_epoch_timestamp = 1619424000,
        time = "timestamp",
        fun = sum
      ),
      expected_sum_df
    )
    expect_equal(
      aggregateEpochs(data2,
        duration = 3,
        measure = "value",
        sample_frequency = 1,
        time = "timestamp",
        fun = sum
      ),
      expected_sum_df
    )
  })

  expected_mean_value2 <- c(
    0.447365719, 0.703815176, 0.409790548, 0.444126982,
    0.399366206, 0.22283761, 0.599175775, 0.628077908,
    0.193830138, 0.358551853, 0.601404122
  )
  expected_sum_value2 <- c(
    1.342097157, 2.111445527, 1.229371643, 1.332380946,
    1.198098617, 0.668512831, 1.797527326, 1.884233724,
    0.581490415, 1.075655559, 1.804212365
  )
  expected_multicolumn_df <- data.frame(
    timestamp = timestamp[seq(from = 1, to = 31, by = 3)],
    epoch_number = epoch2,
    value = expected_sum,
    value2 = expected_sum_value2,
    duration = rep(3, 11)
  )
  expected_multifunction_df <- data.frame(
    timestamp = timestamp[seq(from = 1, to = 31, by = 3)],
    epoch_number = epoch2,
    value.sum = expected_sum,
    value.mean = expected_mean,
    duration = rep(3, 11)
  )
  expected_multicolumn_multifunction_df <- data.frame(
    timestamp = timestamp[seq(from = 1, to = 31, by = 3)],
    epoch_number = epoch2,
    value.sum = expected_sum,
    value.mean = expected_mean,
    value2.sum = expected_sum_value2,
    value2.mean = expected_mean_value2,
    duration = rep(3, 11)
  )
  test_that("AggregateEpochs multiple columns", {
    expect_equal(
      aggregateEpochs(data2,
        duration = 3,
        measure = c("value", "value2"),
        sample_frequency = 1,
        first_epoch_timestamp = 1619424000,
        time = "timestamp",
        fun = sum
      ),
      expected_multicolumn_df
    )
  })
  test_that("AggregateEpochs muliple functions", {
    expect_equal(
      aggregateEpochs(data2,
        duration = 3,
        measure = "value",
        sample_frequency = 1,
        first_epoch_timestamp = 1619424000,
        time = "timestamp",
        fun = function(x) c(sum = sum(x), mean = mean(x))
      ),
      expected_multifunction_df
    )
  })
  test_that("AggregateEpochs multiple columns and muliple functions", {
    expect_equal(
      aggregateEpochs(data2,
        duration = 3,
        measure = c("value", "value2"),
        sample_frequency = 1,
        first_epoch_timestamp = 1619424000,
        time = "timestamp",
        fun = function(x) c(sum = sum(x), mean = mean(x))
      ),
      expected_multicolumn_multifunction_df
    )
  })


  # Test aggregating events with differing durations
  event_start <- c(4, 18, 28, 32)
  event_end <- c(9, 25, 29, 32)
  series_length <- 33

  expected_event_count <- c(6, 8, 2, 1)
  expected_event_mean <- c(1.025597975, 1.750567208, 1.960501621, 1.31775029)
  expected_event_sum <- c(6.153587852, 14.00453766, 3.921003241, 1.31775029)
  expected_event_max <- c(2.510181118, 2.55581665, 2.474537822, 1.31775029)

  events <- data.frame(start = event_start, end = event_end)

  # tests for createEventMapping
  expected_mapping <- c(
    rep(0, 3), rep(1, 6),
    rep(0, 8), rep(2, 8),
    rep(0, 2), rep(3, 2),
    rep(0, 2), rep(4, 1), 0
  )
  event_mapping <- createEventMapping(events,
    start_time = "start",
    end_time = "end",
    series_length
  )

  event2_start <- c(4, 18, 28, 32)
  event2_end <- c(9, 25, 29, 33)
  expected_event2_mapping <- c(
    rep(0, 3), rep(1, 6),
    rep(0, 8), rep(2, 8),
    rep(0, 2), rep(3, 2),
    rep(0, 2), rep(4, 2)
  )
  expected_event2_count <- c(6, 8, 2, 2)
  event2_mapping <- createEventMapping(data.frame(start = event2_start, end = event2_end),
    start_time = "start",
    end_time = "end",
    series_length
  )
  event3_start <- c(1, 15, 25, 29)
  event3_end <- c(6, 22, 26, 30)
  expected_event3_mapping <- c(
    rep(1, 6),
    rep(0, 8), rep(2, 8),
    rep(0, 2), rep(3, 2),
    rep(0, 2), rep(4, 2), rep(0, 3)
  )
  event3_mapping <- createEventMapping(data.frame(start = event3_start, end = event3_end),
    start_time = "start",
    end_time = "end",
    series_length
  )
  expected_event3_count <- c(6, 8, 2, 2)

  event4_start <- c(1)
  event4_end <- c(33)
  expected_event4_mapping <- rep(1, 33)
  event4_mapping <- createEventMapping(data.frame(start = event4_start, end = event4_end),
    start_time = "start",
    end_time = "end",
    series_length
  )
  expected_event4_count <- c(33)

  test_that("testing createEventMapping", {
    expect_equal(event_mapping, expected_mapping)
    expect_equal(event2_mapping, expected_event2_mapping)
    expect_equal(event3_mapping, expected_event3_mapping)
    expect_equal(event4_mapping, expected_event4_mapping)
  })

  # test Mean function for events
  aggregated_events <- aggregateEvents(data2,
    events = events,
    measure = "value",
    time = "timestamp",
    start_time = "start",
    end_time = "end",
    sample_frequency = 1,
    fun = mean
  )


  expected_event_mean_df <- data.frame(
    timestamp = timestamp[event_start],
    event_number = c(1:4),
    value = expected_event_mean,
    duration = expected_event_count
  )
  rownames(expected_event_mean_df) <- seq(2, 5) # The first row is droped as it is event 0 (the interevent times)

  aggregated_events2 <- aggregateEvents(data2,
    events = data.frame(start = event2_start, end = event2_end),
    measure = "value",
    time = "timestamp",
    start_time = "start",
    end_time = "end",
    sample_frequency = 1,
    fun = sum
  )
  aggregated_events3 <- aggregateEvents(data2,
    events = data.frame(start = event3_start, end = event3_end),
    measure = "value",
    time = "timestamp",
    start_time = "start",
    end_time = "end",
    sample_frequency = 1,
    fun = sum
  )
  test_that("aggregateEvents by Mean", {
    expect_equal(aggregated_events, expected_event_mean_df)
  })

  # Test Sum function for events
  expected_event_sum_df <- data.frame(
    timestamp = timestamp[event_start],
    event_number = c(1:4),
    value = expected_event_sum,
    duration = expected_event_count
  )
  rownames(expected_event_sum_df) <- seq(2, 5) # The first row is droped as it is event 0 (the interevent times)

  expected_event2_sum <- c(6.153587852, 14.00453766, 3.921003241, 4.050996112)
  expected_event2_sum_df <- data.frame(
    timestamp = timestamp[event2_start],
    event_number = c(1:4),
    value = expected_event2_sum,
    duration = expected_event2_count
  )
  rownames(expected_event2_sum_df) <- seq(2, 5) # The first row is droped as it is event 0 (the interevent times)

  expected_event3_sum <- c(8.624729028, 12.51867167, 4.871474297, 2.530898181)
  expected_event3_sum_df <- data.frame(
    timestamp = timestamp[event3_start],
    event_number = c(1:4),
    value = expected_event3_sum,
    duration = expected_event3_count
  )
  rownames(expected_event3_sum_df) <- seq(2, 5) # The first row is droped as it is event 0 (the interevent times)

  test_that("aggregateEvents by Sum", {
    expect_equal(
      aggregateEvents(data2,
        events = events,
        measure = "value",
        time = "timestamp",
        start_time = "start",
        end_time = "end",
        sample_frequency = 1,
        fun = sum
      ),
      expected_event_sum_df
    )
    expect_equal(aggregated_events2, expected_event2_sum_df)
    expect_equal(aggregated_events3, expected_event3_sum_df)
  })

  # test Max function for events
  expected_event_max_df <- data.frame(
    timestamp = timestamp[event_start],
    event_number = c(1:4),
    value = expected_event_max,
    duration = expected_event_count
  )
  rownames(expected_event_max_df) <- seq(2, 5) # The first row is droped as it is event 0 (the interevent times)
  test_that("aggregateEvents by Max", {
    expect_equal(
      aggregateEvents(data2,
        events = events,
        measure = "value",
        time = "timestamp",
        start_time = "start",
        end_time = "end",
        sample_frequency = 1,
        fun = max
      ),
      expected_event_max_df
    )
  })


  # Test events starting with index 1
  event_start <- c(1, 41, 101, 161)
  event_end <- c(40, 100, 160, 240)

  timestamp <- c(
    rep(1619424000, 10), rep(1619424001, 10), rep(1619424002, 10), rep(1619424003, 10), rep(1619424004, 10), rep(1619424005, 10),
    rep(1619424006, 10), rep(1619424007, 10), rep(1619424008, 10), rep(1619424009, 10), rep(1619424010, 10), rep(1619424011, 10),
    rep(1619424012, 10), rep(1619424013, 10), rep(1619424014, 10), rep(1619424015, 10), rep(1619424016, 10), rep(1619424017, 10),
    rep(1619424018, 10), rep(1619424019, 10), rep(1619424020, 10), rep(1619424021, 10), rep(1619424022, 10), rep(1619424023, 10)
  )

  value <- c(
    0.14268, 0.21757, 0.52900, 0.36383, 0.77226, 0.40324, 0.89915, 0.78311, 0.78458, 0.19789, 0.43919, 0.32208, 0.64713, 0.78197, 0.26385,
    0.27295, 0.29220, 0.79510, 0.17902, 0.22625, 0.54374, 0.35098, 0.18611, 0.19456, 0.85896, 0.81587, 0.58503, 0.40719, 0.27722, 0.20296,
    0.35092, 0.27459, 0.45962, 0.41295, 0.86575, 0.66672, 0.11755, 0.52654, 0.86727, 0.56019, 0.35358, 0.74724, 0.63962, 0.69328, 0.11020,
    0.25596, 0.50829, 0.53244, 0.45811, 0.69914, 0.27914, 0.66251, 0.77620, 0.18535, 0.25512, 0.69509, 0.82728, 0.52895, 0.48566, 0.49792,
    0.77935, 0.57280, 0.11087, 0.47106, 0.55435, 0.23953, 0.27808, 0.16935, 0.35139, 0.76321, 0.53472, 0.81649, 0.10416, 0.39537, 0.61199,
    0.88902, 0.64204, 0.37887, 0.43116, 0.23647, 0.32718, 0.41245, 0.44991, 0.48804, 0.72741, 0.45446, 0.16543, 0.33086, 0.44340, 0.85309,
    0.83398, 0.47028, 0.54156, 0.56521, 0.23599, 0.80990, 0.60332, 0.29213, 0.73660, 0.19181, 0.41653, 0.44419, 0.66183, 0.32812, 0.14328,
    0.61385, 0.33689, 0.19097, 0.45845, 0.76945, 0.85266, 0.38556, 0.28407, 0.38389, 0.37137, 0.46842, 0.51447, 0.60401, 0.49446, 0.19990,
    0.47928, 0.13632, 0.18083, 0.66348, 0.29237, 0.20725, 0.54102, 0.41511, 0.50977, 0.50303, 0.47440, 0.59781, 0.14512, 0.86859, 0.23449,
    0.18951, 0.38779, 0.71779, 0.88790, 0.86471, 0.71627, 0.28933, 0.65501, 0.19281, 0.28026, 0.64888, 0.34871, 0.22607, 0.18295, 0.52064,
    0.26045, 0.84477, 0.20004, 0.74697, 0.86129, 0.23484, 0.66180, 0.89427, 0.67509, 0.51427, 0.18083, 0.68074, 0.81665, 0.81137, 0.40167,
    0.30615, 0.74574, 0.61289, 0.59765, 0.13865, 0.62766, 0.13603, 0.33061, 0.17562, 0.38540, 0.51627, 0.12964, 0.67909, 0.68736, 0.84879,
    0.22582, 0.70068, 0.16320, 0.88372, 0.17064, 0.27045, 0.23782, 0.82649, 0.68039, 0.21208, 0.33623, 0.31262, 0.11348, 0.86296, 0.36118,
    0.49150, 0.41350, 0.47975, 0.38317, 0.68639, 0.78622, 0.24043, 0.42220, 0.41897, 0.70417, 0.45142, 0.16767, 0.55614, 0.85018, 0.22937,
    0.25659, 0.16008, 0.41959, 0.55806, 0.62432, 0.82646, 0.71658, 0.59699, 0.34690, 0.59615, 0.34873, 0.31040, 0.46002, 0.42651, 0.17205,
    0.29124, 0.55947, 0.79534, 0.13187, 0.64579, 0.34105, 0.23312, 0.31534, 0.64369, 0.31584, 0.76384, 0.47115, 0.63265, 0.16314, 0.58023
  )

  data3 <- data.frame(timestamp, value)
  events <- data.frame(start = event_start, end = event_end)

  expected_event_mean <- c(0.47094425, 0.49090617, 0.4695610, 0.4643855)
  expected_event_count <- c(4, 6, 6, 8)

  aggregated_events <- aggregateEvents(data3,
    events = events,
    measure = "value",
    time = "timestamp",
    start_time = "start",
    end_time = "end",
    sample_frequency = 10,
    fun = mean
  )


  expected_event_mean_df2 <- data.frame(
    timestamp = timestamp[event_start],
    event_number = c(1:4),
    value = expected_event_mean,
    duration = expected_event_count
  )

  test_that("aggregateEvents by Mean", {
    expect_equal(aggregated_events, expected_event_mean_df2)
  })
})
