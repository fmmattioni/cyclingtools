## code to prepare `demo_critical_power` dataset goes here

demo_critical_power <- tibble::tribble(
  ~PO, ~TTE,
  446L,      100L,
  385L,      172L,
  324L,      434L,
  290L,      857L,
  280L,     1361L
)

usethis::use_data(demo_critical_power, overwrite = TRUE)
