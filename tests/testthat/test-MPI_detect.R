local({
  changepoints1 <- data.frame(
    time = c(1677855218, 1677855598, 1677855661, 1677855679),
    index = c(86019, 86399, 62, 80),
    day = c(7, 7, 8, 8)
  )

  changepoints2 <- data.frame(
    time = c(1677855218, 1677855590, 1677855602, 1677855679),
    index = c(86019, 86391, 3, 80),
    day = c(7, 7, 8, 8)
  )

  changepoints3 <- data.frame(
    time = c(1677855218, 1677855590, 1677855598, 1677855602, 1677855679),
    index = c(86019, 86391, 86399, 3, 80),
    day = c(7, 7, 7, 8, 8)
  )

  cut_time <- data.frame(
    time = c(1677855600),
    index = c(1),
    day = c(8)
  )

  expected_transitions1 <- data.frame(
    time = c(1677855218, 1677855600, 1677855661, 1677855679),
    index = c(86019, 1, 62, 80),
    day = c(7, 8, 8, 8)
  )

  expected_transitions2 <- data.frame(
    time = c(1677855218, 1677855590, 1677855600, 1677855679),
    index = c(86019, 86391, 1, 80),
    day = c(7, 7, 8, 8)
  )

  expected_transitions3 <- data.frame(
    time = c(1677855218, 1677855590, 1677855600, 1677855679),
    index = c(86019, 86391, 1, 80),
    day = c(7, 7, 8, 8)
  )

  test_that("Short transition before index 1 is removed", {
    expect_equal(remove_short_transitions(changepoints1, cut_time, 5), expected_transitions1)
  })

  test_that("Short transition after index 1 is removed", {
    expect_equal(remove_short_transitions(changepoints2, cut_time, 5), expected_transitions2)
  })

  test_that("Short transition after index 1 is removed", {
    expect_equal(remove_short_transitions(changepoints3, cut_time, 5), expected_transitions3)
  })
})
