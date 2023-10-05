
# reading in energy data --------------------------------------------------

pacman::p_load(
  tidyverse,
  lubridate
)


gen_energy <- read_csv("monthly_full_release_long_format-4.csv")

gen_energy_europe <- gen_energy %>% 
  subset(Continent == "Europe")

# wrangling ---------------------------------------------------------------

price_energy_europe_1 <- gen_energy_europe %>% 
  rename("Country" = "Area") %>%
  select(Country, "Country code", Date, Category, Subcategory, Variable, Unit, Value) %>%
  filter(!Country == "Ukraine")

price_energy_europe_share_nuclear <- price_energy_europe_1 %>% 
  filter(Category  %in% ("Electricity generation")) %>%
  filter(Variable == "Nuclear") %>%
  filter(Unit == "%") %>%
  select("Country", "Country code", "Date", "Value") %>%
  rename(share_nuclear = Value)

#saving electricity demand as a control variable
price_energy_europe_demand <- price_energy_europe_1 %>% 
  filter(Category %in% "Electricity demand") %>%
  select("Country", "Country code", "Date", "Value") %>%
  rename(demand = Value) %>% 
  mutate(year = year(Date)) %>%
  mutate(month = month(Date))

gen_energy_panel <- left_join(price_energy_europe_demand, price_energy_europe_share_nuclear, by = c("Country", "Country code", "Date"))

#setting irish nuclear generating capacity to zero
gen_energy_panel[is.na(gen_energy_panel)] <- 0

#write.csv(gen_energy_panel, "gen_energy_demand_share.csv")




# loading in price data ---------------------------------------------------
price_data <- read_csv("price_data.csv")

#calculate mean
price_data_mean <- price_data %>% 
  mutate(year = year(Date)) %>%
  mutate(month = month(Date)) %>%
  group_by(Country, year, month) %>%
  rename("price" = "Price (EUR/MWhe)")

price_data_mean_sd <- price_data_mean %>% 
  summarize(mean_price = mean(price), 
            sd_price = sd(price))
  





# loading in heating degree days ------------------------------------------
hdd_cdd <- read_csv("hdd_cdd.csv")

temp_hdd <- hdd_cdd %>%
  select(geo, TIME_PERIOD, indic_nrg, OBS_VALUE) %>%
  group_by(geo, TIME_PERIOD) %>%
  summarize(temp = sum(OBS_VALUE)) %>%
  mutate(TIME_PERIOD = ym(TIME_PERIOD)) %>%
  mutate(year = year(TIME_PERIOD)) %>%
  mutate(month = month(TIME_PERIOD))


temp_hdd <- temp_hdd %>%
  select(geo, year, month, temp) %>%
  rename("Country" = "geo") 


# merging stuff -----------------------------------------------------------
final_panel_1 <- left_join(price_data_mean_sd, gen_energy_panel)

final_panel <- left_join(final_panel_1, temp_hdd)

summary(lm(log(mean_price) ~ as.factor(Date) + as.factor(`Country code`) + share_nuclear + demand + temp, final_panel))

summary(lm((log(sd_price^2)) ~ as.factor(Date) + as.factor(`Country code`) + share_nuclear + demand + temp, final_panel))


