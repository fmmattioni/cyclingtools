test_that("critical speed functions work", {
  expect_s3_class(
    object = critical_speed(
      .data = demo_critical_speed,
      distance_column = "Distance",
      time_to_exhaustion_column = "TTE",
      plot = FALSE
    ),
    class = "tbl_df"
  )
})
