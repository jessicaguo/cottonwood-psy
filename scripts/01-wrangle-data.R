library(googlesheets4)
library(tidyverse)
library(janitor)

# Create function for negate %in%
`%nin%` = Negate(`%in%`)

# Create directories if does not already exist
if(!dir.exists("data_raw")) {
  dir.create("data_raw")
}

if(!dir.exists("data_appended")) {
  dir.create("data_appended")
}


# get files
fn <- list.files("data_raw")
# Set url, get file names
folder_url <- "https://drive.google.com/drive/folders/1wet5afQm-8N9plDQ9qCECaeIqbVhJfID"
folder <- googledrive::drive_get(googledrive::as_id(folder_url))
all_files <- googledrive::drive_ls(folder, type = "csv")

# Only download new files on Google Drive
csv_files <- all_files %>%
  filter(name %nin% fn)

# Download new files to data_raw folder
map2(.x = csv_files$id, .y = csv_files$name,
     ~googledrive::drive_download(googledrive::as_id(.x), path = paste0("data_raw/", .y), overwrite = TRUE))


###### Read in files by latest date #####
fn <- list.files("data_raw")
# only extract latest date
dl <- max(as.numeric(unique(str_extract(fn, "^[0-9]{6}"))))

files <- paste0("data_raw/", grep(dl, fn, value = TRUE))

for(i in 1:length(files)) {
  # Check skipping
  skip_read_15 <- read_csv(files[i], 
                        skip = 15, n_max = 3,
                        locale=locale(encoding="latin1", tz = "America/Phoenix"))
  skip_read_32 <- read_csv(files[i], 
                        skip = 32, n_max = 3,
                        locale=locale(encoding="latin1", tz = "America/Phoenix"))
  skip_read_49 <- read_csv(files[i], 
                           skip = 49, n_max = 3,
                           locale=locale(encoding="latin1", tz = "America/Phoenix"))
  
  if(colnames(skip_read_15)[1] == "Date" & any(class(skip_read_15$Time) == "difftime")) {
    df_temp <- read_csv(files[i], 
                        skip = 15, 
                        locale=locale(encoding="latin1", tz = "America/Phoenix"),
                        col_types = cols(Date = col_character(), # col_date("%d/%m/%Y")
                                         Time = col_time(format = "%H:%M:%S"),
                                         `Chamber Temperature (°C)` = col_double(),
                                         `dT (µV)` = col_double(),
                                         `Wet Bulb Depression (µV)` = col_double(),
                                         `Corrected Water Potential (MPa)` = col_double(),
                                         Intercept = col_double(),
                                         Slope = col_double(),
                                         EDBO = col_double(),
                                         `Correction for dT (MPa)` = col_double(),
                                         `Internal Battery Voltage (V)` = col_double(),
                                         `Internal Battery Temperature (°C)` = col_double(),
                                         `External Power Supply Present` = col_character(),
                                         `External Power Supply Voltage (V)` = col_double(),
                                         `External Power Supply Current (mA)` = col_double(),
                                         `Diagnostic Comment` = col_character())) %>%
      mutate(Date = lubridate::dmy(Date, tz = "America/Phoenix"),
             dt = ymd_hms(paste(Date, Time), tz = "America/Phoenix")) 
  } else if(colnames(skip_read_32)[1] == "Date" & any(class(skip_read_32$Time) == "difftime")) {
    df_temp <- read_csv(files[i], 
                        skip = 32, 
                        locale=locale(encoding="latin1", tz = "America/Phoenix"),
                        col_types = cols(Date = col_character(), # col_date("%d/%m/%Y")
                                         Time = col_time(format = "%H:%M:%S"),
                                         `Chamber Temperature (°C)` = col_double(),
                                         `dT (µV)` = col_double(),
                                         `Wet Bulb Depression (µV)` = col_double(),
                                         `Corrected Water Potential (MPa)` = col_double(),
                                         Intercept = col_double(),
                                         Slope = col_double(),
                                         EDBO = col_double(),
                                         `Correction for dT (MPa)` = col_double(),
                                         `Internal Battery Voltage (V)` = col_double(),
                                         `Internal Battery Temperature (°C)` = col_double(),
                                         `External Power Supply Present` = col_character(),
                                         `External Power Supply Voltage (V)` = col_double(),
                                         `External Power Supply Current (mA)` = col_double(),
                                         `Diagnostic Comment` = col_character())) %>%
      mutate(Date = lubridate::dmy(Date, tz = "America/Phoenix"),
             dt = ymd_hms(paste(Date, Time), tz = "America/Phoenix")) 
  } else if(colnames(skip_read_49)[1] == "Date" & any(class(skip_read_49$Time) == "difftime")) {
    df_temp <- read_csv(files[i], 
                        skip = 49, 
                        locale=locale(encoding="latin1", tz = "America/Phoenix"),
                        col_types = cols(Date = col_character(), # col_date("%d/%m/%Y")
                                         Time = col_time(format = "%H:%M:%S"),
                                         `Chamber Temperature (°C)` = col_double(),
                                         `dT (µV)` = col_double(),
                                         `Wet Bulb Depression (µV)` = col_double(),
                                         `Corrected Water Potential (MPa)` = col_double(),
                                         Intercept = col_double(),
                                         Slope = col_double(),
                                         EDBO = col_double(),
                                         `Correction for dT (MPa)` = col_double(),
                                         `Internal Battery Voltage (V)` = col_double(),
                                         `Internal Battery Temperature (°C)` = col_double(),
                                         `External Power Supply Present` = col_character(),
                                         `External Power Supply Voltage (V)` = col_double(),
                                         `External Power Supply Current (mA)` = col_double(),
                                         `Diagnostic Comment` = col_character())) %>%
      mutate(Date = lubridate::dmy(Date, tz = "America/Phoenix"),
             dt = ymd_hms(paste(Date, Time), tz = "America/Phoenix"))
  }
  
  # clean up and write out
  df <- df_temp %>%
    janitor::clean_names(replace = janitor:::mu_to_u)
  
  f_lab <- str_remove(files[i], "data_raw/[0-9]{6}\\_")
  
  write_csv(df, file = paste0("data_appended/", f_lab))

}
  
# Read in data to check
fn_appended <- list.files("data_appended")

dat_sum <- data.frame(filename = character(),
                      start = Date(),
                      end = Date(),
                      length_non_NA = numeric())

for(i in 1:length(fn_appended)) {
  dat <- read_csv(here::here("data_appended", fn_appended[i]))
  
  dat_sum[i,1] <- fn_appended[i]
  dat_sum[i,2] <- min(dat$date, na.rm = TRUE)
  dat_sum[i,3] <- max(dat$date, na.rm = TRUE)
  dat_sum[i,4] <- nrow(dat)
  
}

# Plot frequency of measurements
ggplot(dat_sum) +
  geom_errorbarh(aes(xmin = start, xmax = end,
                     y = filename),
                 height = 0)
  
