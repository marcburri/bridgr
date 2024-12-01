## code to prepare `baro` dataset goes here

baro <- read.csv("https://datenservice.kof.ethz.ch/api/v1/public/collections/ogd_ch.kof.barometer?mime=csv")

baro <- baro %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(
    time = as.Date(paste0(.[[1]], "-01")),
    values = .[[2]]
  ) %>%
  dplyr::select(time, values) %>%
  dplyr::filter(time >= as.Date("2004-01-01") & time <= as.Date("2022-12-31"))

usethis::use_data(baro, overwrite = TRUE)
