## code to prepare `demo_critical_speed` dataset goes here

demo_critical_speed <- tibble::tribble(
  ~Distance, ~TTE,
  956L,      180L,
  1500L,      300L,
  3399L,      720L
)

usethis::use_data(demo_critical_speed, overwrite = TRUE)
