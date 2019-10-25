library(data.table)
library(sf)
library(stringr)
library(plotly)
library(lubridate)



data.path <- "I:/ESS/SVA3D/VLT-data-analysis/data/"

load(paste0(data.path, "output/wildlife.data.Rdata"))

sweden.regions <-
  st_read(paste0(data.path, "geo/Lan_Sweref99TM_region.shp"))

dates <- seq.Date(as.Date("1948-01-01"), as.Date("2018-12-31"), by = "day")

date.range <- data.table(
  date = dates,
  y = as.factor(year(dates)),
  m = as.factor(month(dates)),
  w = as.factor(isoweek(dates)),
  doy = strftime(dates, format = "%j")
) %>%
  .[, c("ym", "yw") := list(as.factor(str_c(y, m, sep = "-")),
                            as.factor(str_c(y, w, sep = "-")))]
rm(dates)

wild.by.region <- wild.with.region[, .N, by = c("region", "y")] %>%
  merge(regions.codeletter,
        by.x = "region",
        by.y = "c.letter",
        all.x = T) %>%
  setnames("c.code", "LnKod") %>%
  setorderv(c("y", "LnKod"))

wild.by.date <- wild.all[!is.na(date), .N, by = "date"] %>%
  merge(date.range,
        by = "date",
        all.x = T)
