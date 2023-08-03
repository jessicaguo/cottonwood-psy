# Plot each timeseries

fn_appended <- list.files("data_appended")

psy_list <- list()

for(i in 1:length(fn_appended)) {
  ins <- str_remove(fn_appended[i], ".CSV")
  temp <- read_csv(here::here("data_appended", fn_appended[i]),
                   locale = locale(tz = "America/Phoenix")) %>%
    mutate(instrument = ins)
  

  psy_list[[i]] <- temp 
}

psy_df <- do.call(rbind, psy_list)

psy_df %>%
  # filter(date >= as.Date("2023-07-21")) %>%
  ggplot(aes(x = dt, y = corrected_water_potential_m_pa,
             color = instrument)) +
  geom_point() +
  scale_x_datetime(date_labels = "%m-%d") +
  facet_wrap(~instrument,  ncol = 2) +
  guides(color = "none") +
  theme_bw()
