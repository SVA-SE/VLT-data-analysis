library(tidyverse)
library(stringr)
library(sp)
library(rgdal)

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

geotagged <- vlt_relevant %>%
  filter(!(is.na(Gisx) | is.na(Gisy)))

sweref99 <- geotagged %>%
  filter(Gisy > 90 & Gisy < 1083427.2970)
points_sweref99 <- SpatialPoints(cbind(sweref99$Gisx, sweref99$Gisy))
proj4string(points_sweref99) <- proj_sweref99
sweref_2_wgs84 <- spTransform(points_sweref99, proj_wgs84)

rt90 <- geotagged %>%
  filter(Gisy >= 1083427.2970)
points_rt90 <- SpatialPoints(cbind(rt90$Gisx, rt90$Gisy))
proj4string(points_rt90) <- proj_rt90
rt90_2_wgs84 <- spTransform(points_rt90, proj_wgs84)

spdf <- SpatialPointsDataFrame(cbind(geotagged$Gisx, geotagged$Gisy), geotagged)

proj4string(spdf) <- "+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
# proj4string(spdf) <- "+init=epsg:3021 +proj=tmerc +lat_0=0 +lon_0=15.80827777777778 +k=1 +x_0=1500000 +y_0=0 +ellps=bessel +towgs84=414.1,41.3,603.1,-0.855,2.141,-7.023,0"

spdf <- spTransform(spdf, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Load diagnosis definition table
# diagnoses <-
#   read.csv2(
#     str_c(data_dir, "diagnoser.csv"),
#     dec = ',',
#     strip.white = TRUE,
#     na.strings = c("", " ")
#   )
#
#
# names_en <- diagnoses$Namngb
# codes <- diagnoses$Kod
# codenames_se <- assign_hash(names_se, codes, new.env(hash = TRUE))
# codenames_en <- assign_hash(names_en, codes, new.env(hash = TRUE))
#
#
# vlt_diag_en <- vlt_relevant$Diagnoser...Engelska
#
# names_se <- diagnoses$Namn
# vlt_diag_se <- vlt_relevant$Diagnoser
# diags = list()
#
# for (entry in vlt_diag_se) {
#   entry <- entry %>% str_split(pattern = ', ')
#   entry_diags = vector(mode = "character")
#   i <- 0
#   while (i < length(entry[[1]])) {
#     i <- i + 1
#     part <- entry[[1]][i]
#     while (!(part %in% names_se) && i < length(entry[[1]])) {
#       i <- i + 1
#       part <- str_c(part, entry[[1]][i], sep = ', ')
#     }
#     entry_diags <- append(entry_diags, part)
#   }
#   print(entry_diags)
#   diags <- append(diags, entry_diags)
# }
#
# # Find entries whose diagnosis exists exactly in definition table
# diagnosed <- vlt_relevant %>%
#   filter(
#     str_to_lower(Diagnoser) %in% names_se |
#       str_to_lower(Diagnoser...Engelska) %in% names_en
#   )
#
# # Save those whose do not (should mostly be entries with multiple diagnoses)
# undiagnosed <- setdiff(vlt_relevant, diagnosed)
#
# # Sort out entries that do not have the same number of diagnoses in Swedish as in English (for some reason...)
# problematic <- undiagnosed %>%
#   filter(lengths(str_split(Diagnoser, ', ')) != lengths(str_split(Diagnoser...Engelska, ', ')))
#
# # Save those that do and split the diagnoses into separate rows
# unproblematic <- setdiff(undiagnosed, problematic) %>%
#   separate_rows(Diagnoser, Diagnoser...Engelska, sep = ', ')
#
# unproblematic_diagnosed <- unproblematic %>%
#   filter(
#     str_to_lower(Diagnoser) %in% names_se |
#       str_to_lower(Diagnoser...Engelska) %in% names_en
#   )
#
# unproblematic_undiagnosed <-
#   setdiff(unproblematic, unproblematic_diagnosed)
