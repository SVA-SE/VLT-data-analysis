Sys.setlocale("LC_ALL", "swedish")

library(data.table)
library(stringr)
library(lubridate)

# manual edits have been done in csv copies of the original data files, including:
#   -removing unnecessary (and adding necessary) whitespace
#   -repairing broken rows with missing columns
#   -swapping class and order columns (they were named wrong in some tables)
#   -fixing incorrect classification fields:
#       *renaming swedish fields to latin
#       *filling in some blanks
#       *correcting spelling errors
#   -fixing date values of some rows (the format was wrong)
#   -removing random extra header rows in the middle of the data sets

# import all the wildlife data, select relevant columns (rename for inter-dataset consistency), and fix/extract the dates
# what the dates represent may differ between datasets
# wild_1986_2006 dates are autopsy dates, while wild_1992_2006 dates are arrival dates.

data.path <- "I:/ESS/SVA3D/VLT-data-analysis/data/"

wild.1 <- fread(
  paste0(data.path, "csv/1948-1981.csv"),
  sep = ";",
  dec = ",",
  na.strings = c("", ".", "Not given", "Not relevant", "Undefined", "?", "-", "Ej angivet"),
  strip.white = TRUE,
  stringsAsFactors = TRUE
) %>%
  .[, date := as.Date(as.character(Datum), format = "%Y%m%d")] %>%
  .[,
    .(
      id = `V-nr`,
      animal = Djurslag,
      family = Family,
      order = Order,
      class = Class,
      region = `Län`,
      place = Ort,
      date,
      y = as.factor(year(date)),
      m = as.factor(month(date)),
      w = as.factor(week(date)),
      doy = as.numeric(strftime(date, format = "%j"))
    )] %>%
  .[, ym := as.factor(paste(y, m, sep = '-'))]

wild.2 <- fread(
  paste0(data.path, "csv/1982-1985.csv"),
  sep = ";",
  dec = ",",
  na.strings = c("", ".", "Not relevant", "Undefined", "?", "Not given", "-", "Ej angivet"),
  strip.white = TRUE,
  stringsAsFactors = TRUE
) %>%
  .[, date := as.Date(as.character(Datum), format = "%Y%m%d")] %>%
  .[,
    .(
      id = `V-nr`,
      animal = Djurslag,
      family = Family,
      order = Order,
      class = Class,
      region = `Län`,
      place = Ort,
      gisx = `X-koordinat`,
      gisy = `Y-koordinat`,
      date,
      y = as.factor(year(date)),
      m = as.factor(month(date)),
      w = as.factor(week(date)),
      doy = as.numeric(strftime(date, format = "%j"))
    )] %>%
  .[, ym := as.factor(paste(y, m, sep = '-'))]

wild.3 <- fread(
  paste0(data.path, "csv/1986-2006.csv"),
  sep = ";",
  dec = ",",
  na.strings = c("", ".", "Not given", "Not relevant", "Undefined", "?", "-", "Ej angivet"),
  strip.white = TRUE,
  stringsAsFactors = TRUE,
  drop = 1
) %>%
  .[, date := as.Date(str_pad(Obduktionsdatum, 6, "left", "0"), format = "%y%m%d")] %>%
  .[,
    .(
      id = `V-nr`,
      animal = DJURSLAG,
      family = Family,
      order = Order,
      class = Class,
      region = `Län`,
      place = Fyndort,
      gisx = `X-koordinat`,
      gisy = `Y-koordinat`,
      date,
      y = as.factor(year(date)),
      m = as.factor(month(date)),
      w = as.factor(week(date)),
      doy = as.numeric(strftime(date, format = "%j"))
    )] %>%
  .[, ym := as.factor(paste(y, m, sep = '-'))]

wild.4 <- fread(
  paste0(data.path, "csv/1992-2006.csv"),
  sep = ";",
  dec = ",",
  na.strings = c("", ".", "?", "Not relevant", "Not given", "Undefined", "-", "Ej angivet"),
  strip.white = TRUE,
  stringsAsFactors = TRUE
) %>%
  .[, date := as.Date(ANKOMSTDATUM, format = "%Y-%m-%d")] %>%
  .[,
    .(
      id = UPPDRAGSID,
      animal = DJURSLAG,
      family = Family,
      order = Order,
      class = Class,
      place = INSPOSTADRESS,
      gisx = X_KOORDINAT,
      gisy = Y_KOORDINAT,
      date,
      y = as.factor(year(date)),
      m = as.factor(month(date)),
      w = as.factor(week(date)),
      doy = as.numeric(strftime(date, format = "%j"))
    )] %>%
  .[, ym := as.factor(paste(y, m, sep = '-'))]

wild.5 <- fread(
  paste0(data.path, "csv/2007-2018.csv"),
  sep = ";",
  dec = ",",
  na.strings = c("", ".", "?", "#SAKNAS!", "Not given", "Undefined", "-", "Ej angivet"),
  strip.white = TRUE,
  stringsAsFactors = TRUE
) %>%
  .[, date := as.Date(str_match(UppdragID, "^U(\\d{6})-")[seq.int(.N), 2], format = "%y%m%d")] %>%
  .[,
    .(
      id = UppdragID,
      animal = Djurslag,
      family = Family,
      order = Order,
      class = Class,
      region = `Län`,
      place = Postort,
      gisx = Gisx,
      gisy = Gisy,
      date,
      y = as.factor(year(date)),
      m = as.factor(month(date)),
      w = as.factor(week(date)),
      doy = as.numeric(strftime(date, format = "%j"))
    )] %>%
  .[, ym := as.factor(paste(y, m, sep = '-'))]

# load regional (län) codes
regions <- fread(
  paste0(data.path, "geo/county_codes.csv"),
  sep = ";",
  dec = ",",
  na.strings = "",
  colClasses = rep("factor", 5)
)

regions.codeletter <- regions[, `New Code`[1], by = "Numbercode"] %>%
  setnames(c("Numbercode", "V1"), c("c.code", "c.letter"))

urban.areas <- fread(
  paste0(data.path, "geo/urban_areas.csv"),
  sep = ";",
  dec = ",",
  na.strings = "",
  colClasses = c(
    "factor",
    "factor",
    "numeric",
    "factor",
    "character",
    "character",
    "numeric",
    "numeric"
  ),
  strip.white = TRUE,
  encoding = "UTF-8"
) %>%
  .[, .(
    c.code = as.factor(str_pad(
      `County code`, 2, side = "left", pad = "0"
    )),
    c.name = `County name`,
    m.code = `Municipality code`,
    m.name = `Municipality name`,
    p.code = `Place Code`,
    p.name = `Place name`,
    gisx,
    gisy
  )] %>%
  merge(regions.codeletter,
        by = "c.code",
        all.x = TRUE)
  

postal.codes <- fread(
  paste0(data.path, "geo/postalcodes.csv"),
  sep = ";",
  dec = ",",
  na.strings = "",
  encoding = "UTF-8"
) %>%
  na.omit() %>%
  .[, names(sort(table(str_split(c.code, ",")[[1]][1]), decreasing = T))[1], by = "postal.name"] %>%
  setnames("V1", "c.code") %>%
  merge(regions.codeletter,
        by = "c.code")

wild.all <- rbindlist(list(wild.1,
                           wild.2,
                           wild.3,
                           wild.4,
                           wild.5),
                      fill = TRUE,
                      idcol = "origin") %>%
  .[, place := str_to_sentence(str_replace_all(place, "[[:punct:]]", ""))] %>%
  .[place == "Ej angivet", place := NA] %>%
  .[, international := region %in% c("NORGE", "TYSKLAND", "DK", "ÅLAND", "SF", "ESTLAND") |
      place %in% c("Oslo")] %>%
  .[grepl("(^|[^\\.])\\.{2}($|[^\\.])", place), place := NA] %>%
  .[is.na(date), y := str_split(id, "-")[[1]][1]] %>%
  setkey() %>%
  unique()

wild.found.region <- merge(wild.all[region %in% regions$Code],
                           regions[, .(`New Code`, Code)],
                           by.x = "region",
                           by.y = "Code",
                           all.x = TRUE) %>%
  .[, region := `New Code`] %>%
  .[, -"New Code"] %>%
  setcolorder(names(wild.unfound)) %>%
  setkey()


wild.found.munic <- merge(wild.all[place %in% urban.areas$m.name],
                          urban.areas[, names(sort(table(c.letter), decreasing = T))[1], by = "m.name"],
                          by.x = "place",
                          by.y = "m.name",
                          all.x = TRUE) %>%
  .[, region := V1] %>%
  .[, -"V1"] %>%
setcolorder(names(wild.all)) %>%
  setkey()

wild.found.urban <- merge(
  wild.all[place %in% urban.areas$p.name],
  urban.areas[, names(sort(table(c.letter), decreasing = T))[1], by = "p.name"],
  by.x = "place",
  by.y = "p.name",
  all.x = TRUE
) %>%
  .[, region := V1] %>%
  .[,-"V1"] %>%
  setcolorder(names(wild.all)) %>%
  setkey()

wild.found.postal <-
  merge(
    wild.all[place %in% postal.codes$postal.name],
    postal.codes[, .(postal.name,
                     c.letter)],
    by.x = "place",
    by.y = "postal.name",
    all.x = TRUE
  ) %>%
  .[, region := c.letter] %>%
  .[,-"c.letter"] %>%
  setcolorder(names(wild.all)) %>%
  setkey()

wild.with.region <- rbindlist(list(
  wild.found.region,
  wild.found.munic,
  wild.found.urban,
  wild.found.postal
)) %>%
  unique(by = setdiff(names(wild.all), "region")) %>%
  merge(regions.codeletter,
        by.x = "region",
        by.y = "c.letter",
        all.x = TRUE) %>%
  .[, region.code := c.code] %>%
  .[, -"c.code"] %>%
  .[order(date)]


