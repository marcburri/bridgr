## code to prepare `fcurve` dataset goes here
# The fcurve is a daily business cycle indicator for Switzerland.
fcurve <- read.csv("https://raw.githubusercontent.com/dankaufmann/f-curve/refs/heads/master/Results/f-curve-data.csv")

fcurve <- fcurve %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(
    time = as.Date(.[[1]]),
    values = -.[[2]]
    ) %>%
  dplyr::select(time, values) %>%
  dplyr::filter(time >= as.Date("2004-01-01") & time <= as.Date("2022-12-31"))

usethis::use_data(fcurve, overwrite = TRUE)
