# Placeholder dataset for Wales LTLA secondary care data
wales_ltla_secondary_care_placeholder <- tibble(
  variable = c(
    "Bed availability \n(Mar 2022)",
    "Referral to treatment \nwaiting times (Jul 22 - Sep 22)"
  ),
  scaled_1_1 = c(
    NA,
    NA
  ),
  label = c(
    NA,
    NA
  )
)

usethis::use_data(wales_ltla_secondary_care_placeholder, overwrite = TRUE)
