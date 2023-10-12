
# reading in energy data --------------------------------------------------

pacman::p_load(
  tidyverse,
  lubridate, 
  plm, 
  sandwich, 
  fixest, 
  quantreg, 
  modelsummary
)

#ADJUST DIRECTORY
gen_energy <- read_csv("data/monthly_full_release_long_format-4.csv")

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

#saving electricity generation as a control variable
price_energy_europe_generation <- price_energy_europe_1 %>% 
  filter(Category %in% "Electricity generation") %>%
  filter(Subcategory %in% "Total") %>%
  select("Country", "Country code", "Date", "Value") %>%
  rename(generation = Value) %>% 
  mutate(year = year(Date)) %>%
  mutate(month = month(Date))

#Renewables 
price_energy_europe_renewables <- price_energy_europe_1 %>% 
  filter(Category %in% "Electricity generation") %>%
  filter(Variable == "Renewables") %>%
  filter(Unit == "%") %>%
  select("Country", "Country code", "Date", "Value") %>%
  rename(share_renewables = Value) %>% 
  mutate(year = year(Date)) %>%
  mutate(month = month(Date))


gen_energy_panel_1 <- left_join(price_energy_europe_demand, price_energy_europe_share_nuclear, by = c("Country", "Country code", "Date"))

gen_energy_panel_2 <- left_join(price_energy_europe_generation, gen_energy_panel_1, by = c("Country", "Country code", "Date", "year", "month"))

gen_energy_panel <- left_join(gen_energy_panel_2, price_energy_europe_renewables, by = c("Country", "Country code", "Date", "year", "month"))

#setting irish nuclear generating capacity to zero
gen_energy_panel[is.na(gen_energy_panel)] <- 0





# loading in price data ---------------------------------------------------

#ADJUST DIRECTORY
price_data <- read_csv("data/price_data.csv")

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

#ADJUST DIRECTORY
hdd_cdd <- read_csv("data/hdd_cdd.csv")

temp_hdd <- hdd_cdd %>%
  select(geo, TIME_PERIOD, indic_nrg, OBS_VALUE) %>%
  group_by(geo, TIME_PERIOD) %>%
  summarize(hdd = sum(OBS_VALUE)) %>%
  mutate(TIME_PERIOD = ym(TIME_PERIOD)) %>%
  mutate(year = year(TIME_PERIOD)) %>%
  mutate(month = month(TIME_PERIOD))


temp_hdd <- temp_hdd %>%
  select(geo, year, month, hdd) %>%
  rename("Country" = "geo") 






# Loading in natural gas price data ---------------------------------------
nat_gas <- read_csv("data/nat_gas_prices.csv")

nat_gas_1 <- nat_gas %>%
  rename("Country" = "country_clean") %>%
  select(year, month, Country, gas_ppi)





# Loading in oil price data -----------------------------------------------

oil_prices <- read_csv("data/oil_prices.csv")

oil <- oil_prices %>%
  select(geo, TIME_PERIOD, indic_nrg, OBS_VALUE) %>%
  mutate(TIME_PERIOD = ym(TIME_PERIOD)) %>%
  mutate(year = year(TIME_PERIOD)) %>%
  mutate(month = month(TIME_PERIOD)) %>%
  select(geo, year, month, OBS_VALUE) %>%
  rename("Country" = "geo") %>%
  rename("oil" = "OBS_VALUE")
  


# Loading in oil price data -----------------------------------------------

oil_prices <- read_csv("data/oil_prices.csv")

oil <- oil_prices %>%
  select(geo, TIME_PERIOD, indic_nrg, OBS_VALUE) %>%
  mutate(TIME_PERIOD = ym(TIME_PERIOD)) %>%
  mutate(year = year(TIME_PERIOD)) %>%
  mutate(month = month(TIME_PERIOD)) %>%
  select(geo, year, month, OBS_VALUE) %>%
  rename("Country" = "geo") %>%
  rename("oil" = "OBS_VALUE")



# merging stuff -----------------------------------------------------------
final_panel_1 <- left_join(price_data_mean_sd, gen_energy_panel)

# temperature data --------------------------------------------------------
temp <- read_csv("data/temp_data.csv")

temp <- temp %>%
  select(!country_code) %>%
  rename("Country" = "country_name")

final_panel_2 <- left_join(final_panel_1, oil)

final_panel_3 <- left_join(final_panel_2, nat_gas_1)

final_panel_4 <- left_join(final_panel_3, temp)

final_panel <- left_join(final_panel_4, temp_hdd)


# imputation of oil price data  -------------------------------------------
final_panel_imputed_2 <- final_panel %>% 
  group_by(Date) %>%
  mutate(oil_imputed = ifelse(is.na(oil), mean(oil, na.rm = TRUE), oil))


#PREPARING DATA FOR ENERGY CRISIS regression
final_panel_imputed_1 <- final_panel_imputed_2 %>% 
  group_by(Date, Country) %>%
  select(!oil) %>%
  mutate(energycrisis = ifelse(Date > as.Date("2021-09-01"), 1, 0))


#PREPARING DATA FOR ENERGY CRISIS regression
final_panel_imputed <- final_panel_imputed_1 %>% 
  mutate(nuclear_bin = case_when(
    share_nuclear > 0.4 ~ "high",
    share_nuclear > 0.2 ~ "mid",
    share_nuclear < 0.2 ~ "low")) %>%
  relevel(share_nuclear, ref = "low")

#DATA TRANSFORMATION FINSIHED

final_panel_imputed$nuclear_bin <- relevel(final_panel_imputed$nuclear_bin, ref = "no")



# REGRESSION --------------------------------------------------------

#summary(lm(log(mean_price) ~ as.factor(Date) + as.factor(`Country code`) + share_nuclear + demand + temp + oil_imputed, final_panel_imputed))

#summary(lm((log(sd_price^2)) ~ as.factor(Date) + as.factor(`Country code`) + share_nuclear + demand + temp + oil_imputed, final_panel_imputed))









#DIFFERENTIAL IMPACT WITH ENERGY DEMAND




#trying lag
plm <- plm(log(mean_price) ~ lag(log(mean_price)) + share_nuclear + log(demand) + temp + oil_imputed, 
           index = c("Country", "Date"),
           model = "within", 
           effect = "twoways",
           data = final_panel_imputed)

plm_sd <- plm(log(sd_price^2) ~ lag(log(mean_price)) + share_nuclear + log(demand) + temp + oil_imputed, 
              index = c("Country", "Date"),
              model = "within", 
              effect = "twoways",
              data = final_panel_imputed)


#linear time trends
plm <- lm(log(sd_price^2) ~  as.factor(Country) + as.numeric(Date) + as.factor(Country)*as.numeric(Date) + share_nuclear + log(demand) + temperature + oil_imputed, 
          data = final_panel_imputed)




#QUANTILE MODELS
quantile_level <- c(seq(0.25, 0.5, 0.75 , by = 0.25))

quantile_model_mean <- rq(log(mean_price) ~ as.factor(Country) + as.factor(Date) + share_nuclear + (demand) + temperature + I(temperature^2) + log(gas_ppi), 
                     data = final_panel_imputed,
                     tau = quantile_level)

quantile_model_sd <- rq(log(sd_price^2) ~ as.factor(Country) + as.factor(Date) + share_nuclear + (demand) + temperature + I(temperature^2) + log(gas_ppi), 
                     data = final_panel_imputed,
                     tau = quantile_level)


summary(quantile_model_sd)
#for (tau in quantiles) {
#  model <- rq(log(sd_price^2) ~ as.factor(Country) + as.factor(Date) + share_nuclear + log(demand) + temp + oil_imputed, 
#              tau = tau, data = final_panel_imputed)
#  clustered_se <- vcovHC(model, type = "arellano", cluster = "group", 
#                         adjust = TRUE)
#  print(coeftest(model, vcov = clustered_se))
#}





#ADDING RENEWABLES

model_feols_mean_renew <- feols(log(mean_price) ~ (share_nuclear) + share_renewables + temperature + I(temperature^2) + (demand) + log(gas_ppi) | Country + Date,
                          vcov = "cluster", 
                          data = final_panel)

model_feols_sd_renew <- feols((log(sd_price^2)) ~ share_nuclear + share_renewables + temperature + I(temperature^2) + (demand) + log(gas_ppi)  | Country + Date,
                        vcov = "cluster",
                        data = final_panel)




#BINNED REGRESSION
model_feols_mean <- feols(log(mean_price) ~ nuclear_bin + (temperature) + I(temperature^2)  + log(demand) + log(gas_ppi) | Country + Date,
                          vcov = "cluster", 
                          data = final_panel_imputed)


model_feols_sd <- feols((log(sd_price^2)) ~ nuclear_bin + temperature + I(temperature^2) + log(demand) + log(gas_ppi)  | Country + Date,
                        vcov = "cluster",
                        data = final_panel_imputed)




#REGRESSION TABLES

#regression_1
model_feols_mean <- feols(log(mean_price) ~ share_nuclear + (temperature) + I(temperature^2)  + (demand) + log(gas_ppi) | Country + Date,
                          vcov = "cluster", 
                          data = final_panel_imputed)


model_feols_sd <- feols((log(sd_price^2)) ~ share_nuclear + temperature + I(temperature^2) + (demand) + log(gas_ppi)  | Country + Date,
                        vcov = "cluster",
                        data = final_panel_imputed)

etable(model_feols_mean, model_feols_sd, 
       tex = TRUE, 
       dict = c("log(sd_price^2)" = "log(variance)",
                "log(gas_ppi)" = "log(nat_gas)"))



#REGRESSION 2 RENEWABLES
model_feols_mean_renew <- feols(log(mean_price) ~ share_renewables + temperature + I(temperature^2) + (demand) + log(gas_ppi) | Country + Date,
                                vcov = "cluster", 
                                data = final_panel)

model_feols_sd_renew <- feols((log(sd_price^2)) ~ share_renewables + temperature + I(temperature^2) + (demand) + log(gas_ppi)  | Country + Date,
                              vcov = "cluster",
                              data = final_panel)

model_feols_mean_renew_nuclear <- feols(log(mean_price) ~ share_nuclear + share_renewables + temperature + I(temperature^2) + (demand) + log(gas_ppi) | Country + Date,
                                vcov = "cluster", 
                                data = final_panel)

model_feols_sd_renew_nuclear <- feols((log(sd_price^2)) ~ share_nuclear + share_renewables + temperature + I(temperature^2) + (demand) + log(gas_ppi)  | Country + Date,
                              vcov = "cluster",
                              data = final_panel)

etable(model_feols_mean_renew, model_feols_sd_renew, model_feols_mean_renew_nuclear, model_feols_sd_renew_nuclear,
       tex = TRUE, 
       dict = c("log(sd_price^2)" = "log(variance)",
                "log(gas_ppi)" = "log(nat_gas)"))


#3 REGRESSION FOR DIFFERENTIAL IMPACT DURING ENERGY CRISIS?
model_feols_mean_diff_1 <- feols((log(mean_price)) ~ share_nuclear +  temperature + I(temperature^2)  + (demand) + log(gas_ppi) + energycrisis + I(energycrisis*share_nuclear) | Country,
                               vcov = "cluster",
                               data = final_panel_imputed)

model_feols_sd_diff_1 <- feols((log(sd_price^2)) ~ share_nuclear + temperature + I(temperature^2) + (demand) + log(gas_ppi) + energycrisis + I(energycrisis*share_nuclear) | Country,
                             vcov = "cluster",
                             data = final_panel_imputed)

model_feols_mean_diff_2 <- feols((log(mean_price)) ~ share_nuclear +  I(energycrisis*share_nuclear) + temperature + I(temperature^2)  + (demand) + log(gas_ppi) + energycrisis  | Country + Date,
                               vcov = "cluster",
                               data = final_panel_imputed)

model_feols_sd_diff_2 <- feols((log(sd_price^2)) ~ share_nuclear + I(energycrisis*share_nuclear) + temperature + I(temperature^2) + (demand) + log(gas_ppi) + energycrisis | Country + Date,
                             vcov = "cluster",
                             data = final_panel_imputed)

etable(model_feols_mean_diff_1, model_feols_sd_diff_1, model_feols_mean_diff_2, model_feols_sd_diff_2,
       tex = TRUE,
       digits = 2,
       dict = c("log(sd_price^2)" = "log(variance)",
                "log(gas_ppi)" = "log(nat_gas)", 
                "I(energycrisis * share_nuclear)" = "energy_crisis*share_nuclear"))



#regression_only in countries where there is nuclear
model_feols_mean_generation <- feols((log(mean_price)) ~ (I(share_nuclear*generation)) + temperature + I(temperature^2)  + demand + log(gas_ppi) | Country + Date,
                                     vcov = "cluster",
                                     data = final_panel_imputed %>% filter(share_nuclear>0))

model_feols_sd_generation <- feols(log(sd_price^2) ~ (I(share_nuclear*generation)) + temperature + I(temperature^2) + demand + log(gas_ppi) | Country + Date,
                                   vcov = "cluster", 
                                   data = final_panel_imputed %>% filter(share_nuclear>0))

etable(model_feols_mean_generation, model_feols_sd_generation, 
         tex = TRUE, 
         dict = c("log(sd_price^2)" = "log(variance)",
                  "log(gas_ppi)" = "log(nat_gas)", 
                  "(I(share_nuclear*generation))" = "nuclear_generation"))



etable(model_feols_mean, model_feols_sd, 
       tex = TRUE, 
       dict = c("log(sd_price^2)" = "log(variance)",
                "log(gas_ppi)" = "log(nat_gas)"))

modelsummary(model_feols_mean)



#5 REGRESSION FOR DIFFERENTIAL IMPACT ON DEMAND LEVEL?
model_feols_mean_diff <- feols((log(mean_price)) ~ share_nuclear + I(generation*share_nuclear) + temperature + I(temperature^2) + (demand) + log(gas_ppi) | Country + Date,
                               vcov = "cluster",
                               data = final_panel_imputed)

model_feols_sd_diff <- feols((log(sd_price^2)) ~ share_nuclear + I(generation*share_nuclear)  + temperature + I(temperature^2) + (demand) + log(gas_ppi) | Country + Date,
                             vcov = "cluster",
                             data = final_panel_imputed)

etable(model_feols_mean_diff, model_feols_sd_diff, 
         tex = TRUE, 
         digits = 4,
         dict = c("log(sd_price^2)" = "log(variance)",
                  "log(gas_ppi)" = "log(nat_gas)", 
                  "I(generation*share_nuclear)" = "nuclear_generation"))


