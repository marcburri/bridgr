## code to prepare `gdp` dataset goes here
# Real GDP data for Switzerland, calendar and seasonally adjusted
gdp <- read.csv("https://www.seco.admin.ch/dam/seco/en/dokumente/Wirtschaft/Wirtschaftslage/BIP_Daten/ch_seco_gdp_csv.csv.download.csv/ch_seco_gdp.csv")

gdp <- gdp %>%
  dplyr::as_tibble() %>%
  dplyr::filter(
    structure == "gdp",
    type == "real",
    seas_adj == "csa"
    ) %>%
  dplyr::mutate(
    time = as.Date(.[[4]]),
    values = .[[5]]
  ) %>%
  dplyr::select(time, values) %>%
  dplyr::filter(time >= as.Date("2004-01-01") & time <= as.Date("2022-12-31"))

usethis::use_data(gdp, overwrite = TRUE)
