library(tidyverse)
library(stringr)

data_dir <- "data/"
out_dir <- "outputs/"

date_pattern <- "^U(\\d{6})-"
start_date <- as.Date("2007-01-01")
end_date <- as.Date("2018-12-31")

proj_sweref99 <- "+init=epsg:3006 +proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
proj_rt90 <- "+init=epsg:3021 +proj=tmerc +lat_0=0 +lon_0=15.80827777777778 +k=1 +x_0=1500000 +y_0=0 +ellps=bessel +towgs84=414.1,41.3,603.1,-0.855,2.141,-7.023,0 +units=m +no_defs"
proj_wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Extract date from UppdragID and filter out rows outside desired date range
vlt_data <-
  read.csv2(
    str_c(data_dir, "4.2007-2018.csv"),
    dec = ",",
    strip.white = TRUE,
    na.strings = c("", " ")
  ) %>%
  mutate(Datum = as.Date(str_match(UppdragID, date_pattern)[seq.int(n()), 2], format = "%y%m%d")) %>%
  filter(between(Datum, start_date, end_date))

# Select relevant columns
vlt_relevant <- vlt_data %>%
  select(
    UppdragID,
    Datum,
    Insäntmaterial,
    Län:Gisy,
    Diagnoser,
    Diagnoser...Engelska,
    Djurslag,
    Family:Class
  )

counties <- read.csv2(
  str_c(data_dir, 'Lan_Sweref99TM.csv'),
  na.strings = '',
  sep = ';',
  dec = ','
)

time_series <- vlt_relevant %>%
  group_by(date = Datum) %>%
  summarise(n = n()) %>%
  complete(date = seq.Date(start_date, end_date, by = "day")) %>%
  mutate(
    n = ifelse(is.na(n), 0, n),
    i = seq.int(n()),
    y = as.factor(strftime(date, format = "%Y")),
    m = as.factor(strftime(date, format = "%m")),
    d = as.numeric(strftime(date, format = "%j"))
  )

time_series %>%
  filter(y == 2018) %>%
  ggplot(aes(date, n)) +
  geom_line()


