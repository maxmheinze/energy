
# Header ------------------------------------------------------------------

pacman::p_load(
  tidyverse,
  countrycode,
  lubridate
)



# Read Data ---------------------------------------------------------------

temperature <- read_csv("/Users/heinzemax/Downloads/temperature.csv")

temp_data <- temperature %>%
  dplyr::filter(LEVL_CODE == 0) %>%
  dplyr::select(NUTS_ID, climate_variable, date, value) %>%
  rename(country_code = NUTS_ID) %>%
  mutate(country_name = countrycode(country_code, "eurostat", "country.name")) %>%
  dplyr::filter(climate_variable == "tmp") %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  filter(year %in% 2015:2022) %>%
  group_by(country_code, country_name, year, month) %>%
  summarize(temperature = mean(value))


write_csv(temp_data, "/Users/heinzemax/Documents/GitHub/energy/data/temp_data.csv")
