
# Header ------------------------------------------------------------------

# development version needed
devtools::install_github("thomasp85/transformr")

pacman::p_load(
  tidyverse,
  lubridate,
  countrycode,
  gganimate,
  RColorBrewer,
  sf,
  rnaturalearth,
  rnaturalearthdata,
  magrittr,
  gifski,
  transformr
)

source("code/regression.R")


# Animations need gganimate 1.0.7 and transformr 0.1.3, newer versions will
# throw an error (bug) -- so install old versions while removing current versions
# if necessary!

# Use this code:
# remove.packages("gganimate")
# install.packages("https://cran.r-project.org/src/contrib/Archive/gganimate/gganimate_1.0.7.tar.gz", repos = NULL, type = "source")
# remove.packages("transformr")
# install.packages("https://cran.r-project.org/src/contrib/Archive/transformr/transformr_0.1.3.tar.gz", repos = NULL, type = "source")


# Animated Scatter Plot ---------------------------------------------------

anim <- plot_data %>%
  ungroup() %>%
  select(Date, year, Country, share_nuclear, mean_price) %>%
  drop_na() %>%
  filter(year < 2021) %>%
  ggplot() +
  geom_point(aes(x = share_nuclear, y = mean_price, color = Country), size = 20) +
  theme_bw() +
  theme(text = element_text(family = "Fira Sans", size = 50)) +
  labs(title = "Share of Nuclear and Energy Price, {(frame_time)}",
       x = "Share of Nuclear Energy (%)",
       y = "Energy Price (EUR/MWh)") +
  scale_size_continuous(range = c(15, 30)) +
  transition_time(Date) 

animate(anim, width = 2400, height = 1200, start_pause = 10, end_pause = 10, nframes = 300, duration = 20, renderer = gifski_renderer())
anim_save("output/anim_scatterplot/anim_scatterplot.gif")


# Animated Bar Chart ------------------------------------------------------

anim <- plot_data %>%
  ungroup() %>%
  mutate(ccode = countrycode(Country, "country.name", "iso2c")) %>%
  select(Date, ccode, share_nuclear) %>%
  drop_na() %>%
  filter(share_nuclear > 0) %>%
  ggplot() +
  geom_bar(aes(x = reorder(ccode, -share_nuclear), y = share_nuclear, fill = reorder(ccode, -share_nuclear)), stat = "identity") +
  theme_bw() +
  theme(text = element_text(family = "Fira Sans", size = 50)) +
  labs(title = "Share of Nuclear, {(frame_time)}",
       x = "",
       y = "Share of Nuclear Energy (%)") +
  guides(fill="none") +
  transition_time(Date) 

animate(anim, width = 2400, height = 1200, start_pause = 10, end_pause = 10, nframes = 300, duration = 20, renderer = gifski_renderer())
anim_save("output/anim_nuclear/anim_nuclear.gif")


# Average Price -----------------------------------------------------------

plot_data %>%
  ungroup() %>%
  select(Date, Country, mean_price, sd_price) %>%
  group_by(Date) %>%
  summarize(mean_price = mean(mean_price),
            sd_price = mean(sd_price)) %>%
  drop_na() %>%
  ggplot() +
  geom_line(aes(x = Date, y = mean_price), color = "#1c3068", linewidth = 1) +
  theme_bw() +
  theme(text = element_text(family = "Fira Sans", size = 12)) +
  labs(title = "Monthly Mean Wholesale Electricity Price",
       subtitle = "averaged over countries",
       x = "",
       y = "Wholesale Electricity Price (EUR/MWh)")

ggsave("output/avg_price.png", plot = last_plot(), width = 2400, height = 1200, units = "px")



# Volatility --------------------------------------------------------------

plot_data %>%
  ungroup() %>%
  select(Date, Country, mean_price, sd_price) %>%
  group_by(Date) %>%
  summarize(mean_price = mean(mean_price),
            sd_price = mean(sd_price)) %>%
  drop_na() %>%
  ggplot() +
  geom_line(aes(x = Date, y = sd_price), color = "#4a6dcf", linewidth = 1) +
  theme_bw() +
  theme(text = element_text(family = "Fira Sans", size = 12)) +
  labs(title = "Wholesale Electricity Price Volatility",
       subtitle = "averaged over countries",
       x = "",
       y = "SD of Wholesale Electricity Price")

ggsave("output/vol_price.png", plot = last_plot(), width = 2400, height = 1200, units = "px")
