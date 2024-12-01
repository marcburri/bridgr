## code to prepare `wea` dataset goes here
# The WEA is a weekly business cycle indicator for Switzerland referncing yoy GDP growth.
wea <- read.csv("https://www.seco.admin.ch/dam/seco/en/dokumente/Wirtschaft/Wirtschaftslage/indikatoren/wwa.csv.download.csv/wwa.csv")

wea <- wea %>%
  dplyr::as_tibble() %>%
  dplyr::filter(.[[1]] == "seco_wwa") %>%
  dplyr::mutate(
    time = as.Date(.[[4]]),
    values = .[[5]]
    ) %>%
  dplyr::select(time, values) %>%
  dplyr::filter(time >= as.Date("2005-01-01") & time <= as.Date("2022-12-31"))

usethis::use_data(wea, overwrite = TRUE)
