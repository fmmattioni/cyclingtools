test_that("critical power functions work", {
  expect_s3_class(object = critical_power(.data = demo_data,
                                          power_output_column = "PO",
                                          time_to_exhaustion_column = "TTE",
                                          plot = FALSE),
                  class = "tbl_df"
  )
})
