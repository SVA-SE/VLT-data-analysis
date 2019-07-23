library(tidyverse)
library(stringr)

data_dir <- "data/"

date_pattern <- "^U(\\d{6})-"
start_date <- as.Date("2007-01-01")
end_date <- as.Date("2011-12-31")

# Extract date from UppdragID and filter out rows outside desired date range
vlt_data <- read.csv2(str_c(data_dir, "4.2007-2018.csv")) %>%
  mutate(Datum = as.Date(str_match(UppdragID, date_pattern)[seq.int(n()), 2], format = "%y%m%d")) %>%
  filter(between(Datum, start_date, end_date))

vlt_relevant <- vlt_data %>%
  select(UppdragID, Datum, Insäntmaterial, Län, Gisx, Gisy, Diagnoser, Djurslag, Family, Order, Class)


