# Plot each timeseries

fn_appended <- list.files("data_appended")

psy_list <- list()

for(i in 1:length(fn_appended)) {
  ins <- str_remove(fn_appended[i], ".CSV")
  temp <- read_csv(here::here("data_appended", fn_appended[i])) %>%
    mutate(instrument = ins)
  

  psy_list[[i]] <- temp 
}

psy_df <- do.call(rbind, psy_list)

psy_df %>%
  ggplot(aes(x = dt, y = corrected_water_potential_m_pa,
             color = instrument)) +
  geom_point() +
  facet_wrap(~instrument) +
  guides(color = "none") +
  theme_bw()
