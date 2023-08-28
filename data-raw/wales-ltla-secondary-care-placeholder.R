# Placeholder dataset for Wales LTLA secondary care data
wales_ltla_secondary_care_placeholder <- tibble(
  area_name = rep(
    c(
      "Isle of Anglesey", "Gwynedd", "Conwy", "Denbighshire", "Flintshire",
      "Wrexham", "Ceredigion", "Pembrokeshire", "Carmarthenshire", "Swansea",
      "Neath Port Talbot", "Bridgend", "Vale of Glamorgan", "Cardiff",
      "Rhondda Cynon Taf", "Caerphilly", "Blaenau Gwent", "Torfaen",
      "Monmouthshire", "Newport", "Powys", "Merthyr Tydfil"
    ),
    2
  ),
  variable = rep(
    c(
      "Bed availability \n(Mar 2022)",
      "Referral to treatment \nwaiting times (Jul 22 - Sep 22)"
    ),
    22
  ),
  scaled_1_1 = NA,
  label = NA
)

usethis::use_data(wales_ltla_secondary_care_placeholder, overwrite = TRUE)
