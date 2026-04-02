local({

  AGSA <- c(10,11,12,13,14,15,16,17,18,19,20)
  Duration <- c(5,4,3,2,1,5,4,3,2,1,5)
  Idx <- c(T,T,F,F,F,T,F,T,F,T,F)

  test_that("Weighted_sum calculation is as expected", {
    expect_equal(weighted_sum(AGSA, Duration, Idx), 239)
  })

  test_that("Weighted_mean calculation is as expected", {
    expect_equal(round(weighted_mean(AGSA, Duration, Idx), 1), 13.3)
  })

  TimeUTC <- c(10,11,13,16,20,25,31,32,34,37,41)
  AGSAMean <- c(5.1,4.2,3.3,2.2,1,5,4,3,2,1.6,5.8)
  Duration <- c(1,2,3,4,5,6,1,2,3,4,5)

  test_bouts <- data.frame(TimeUTC = TimeUTC,
                      AGSAMean = AGSAMean,
                      Duration = Duration)

  window <- 3

  M3 <- mx_lx_window(test_bouts, 3)

  test_that("mx_lx calculation is as expected", {
    expect_equal(M3$M_intensity, 5.8)
    expect_equal(M3$M_time, 41)
    expect_equal(M3$L_intensity, 1)
    expect_equal(M3$L_time, 20)
  })

  C80 <- cx_percentile(test_bouts, 80, "AGSAMean", "Duration")

  test_that("cx_percentile calculation is as expected", {
    expect_equal(C80, 5)
  })

  F3_5 <- fx_count(test_bouts, 3, 5)

  test_that("fx_count calculation is as expected", {
    expect_equal(F3_5$M_count, 5)
    expect_equal(F3_5$M_time, 25)
    expect_equal(F3_5$L_count, 1)
    expect_equal(F3_5$L_time, 20)
  })
})
