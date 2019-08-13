Sys.setlocale("LC_ALL", "swedish")

library(tidyverse)
library(stringr)
library(lubridate)
library(sf)

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


wild_1948_1981 <- read.csv2(
  "data/csv/1.1.1948-1981.csv",
  sep = ";",
  dec = ",",
  quote = "\"",
  na.strings = c("", " ", ".", "Not given", "Not relevant", "Undefined", "?", "-"),
  strip.white = TRUE
) %>%
  select(
    ID = V.nr,
    Animal = Djurslag,
    Family,
    Order,
    Class,
    Region = Län,
    Place = Ort,
    Date = Datum
  ) %>%
  mutate(
    ID = as.factor(ID),
    Region = as.factor(str_to_upper(Region)),
    Date = as.Date(as.character(Date), format = "%Y%m%d"),
    Year = as.factor(year(Date)),
    Month = as.factor(month(Date)),
    Week = as.factor(week(Date)),
    yearmonth = as.factor(paste(Year, Month, sep = '-'))
  )

unique(wild_1948_1981$Class)
unique(wild_1948_1981$Order)
unique(wild_1948_1981$Family)

wild_1982_1985 <- read.csv2(
  "data/csv/1.2.1982-1985.csv",
  sep = ";",
  dec = ",",
  quote = "\"",
  na.strings = c("", " ", ".", "Not relevant", "Undefined", "?", "Not given", "-"),
  strip.white = TRUE
) %>%
  select(
    ID = V.nr,
    Animal = Djurslag,
    Family,
    Order,
    Class,
    Region = Län,
    Place = Ort,
    Date = Datum
  ) %>%
  mutate(
    ID = as.factor(ID),
    Region = as.factor(str_to_upper(Region)),
    Date = as.Date(as.character(Date), format = "%Y%m%d"),
    Year = as.factor(year(Date)),
    Month = as.factor(month(Date)),
    Week = as.factor(week(Date)),
    yearmonth = as.factor(paste(Year, Month, sep = '-'))
  )

unique(wild_1982_1985$Class)
unique(wild_1982_1985$Order)
unique(wild_1982_1985$Family)

wild_1986_2006 <- read.csv2(
  "data/csv/2.1986-2006.csv",
  sep = ";",
  dec = ",",
  quote = "\"",
  na.strings = c("", " ", ".", "Not given", "Not relevant", "Undefined", "?", "-"),
  strip.white = TRUE
) %>%
  select(
    ID = V.nr,
    Animal = DJURSLAG,
    Family,
    Order,
    Class,
    Region = Län,
    Place = Fyndort,
    Gisx = X.koordinat,
    Gisy = Y.koordinat,
    Date = Obduktionsdatum.1
  ) %>%
  mutate(
    ID = as.factor(ID),
    Region = as.factor(str_to_upper(Region)),
    Gisx = as.numeric(Gisx),
    Gisy = as.numeric(Gisy),
    Date = as.Date(ifelse(
      length(as.character(Date)) == 5,
      paste('0', as.character(Date)),
      as.character(Date)
    ), format = "%y%m%d"),
    Year = as.factor(year(Date)),
    Month = as.factor(month(Date)),
    Week = as.factor(week(Date)),
    yearmonth = as.factor(paste(Year, Month, sep = '-'))
  )

unique(wild_1986_2006$Class)
unique(wild_1986_2006$Order)
unique(wild_1986_2006$Family)

wild_1992_2006 <- read.csv2(
  "data/csv/3.1992-2006.csv",
  sep = ";",
  dec = ",",
  quote = "\"",
  na.strings = c("", " ", ".", "?", "Not relevant", "Not given", "Undefined", "-"),
  strip.white = TRUE
) %>%
  select(
    ID = UPPDRAGSID,
    Animal = DJURSLAG,
    Family,
    Order,
    Class,
    Place = INSPOSTADRESS,
    Gisx = X_KOORDINAT,
    Gisy = Y_KOORDINAT,
    Date = ANKOMSTDATUM
  ) %>%
  mutate(
    ID = as.factor(ID),
    Gisx = as.numeric(Gisx),
    Gisy = as.numeric(Gisy),
    Date = as.Date(as.character(Date), format = "%Y-%m-%d"),
    Year = as.factor(year(Date)),
    Month = as.factor(month(Date)),
    Week = as.factor(week(Date)),
    yearmonth = as.factor(paste(Year, Month, sep = '-'))
  )

unique(wild_1992_2006$Class)
unique(wild_1992_2006$Order)
unique(wild_1992_2006$Family)

wild_2007_2018 <- read.csv2(
  "data/csv/4.2007-2018.csv",
  sep = ";",
  dec = ",",
  quote = "\"",
  na.strings = c("", " ", ".", "?", "#SAKNAS!", "Not given", "Undefined", "-"),
  strip.white = TRUE
) %>%
  select(
    ID = UppdragID,
    Animal = Djurslag,
    Family,
    Order,
    Class,
    Region = Län,
    Place = Postort,
    Gisx,
    Gisy,
  ) %>%
  mutate(
    ID = as.factor(ID),
    Region = as.factor(str_to_upper(Region)),
    Gisx = as.numeric(Gisx),
    Gisy = as.numeric(Gisy),
    Date = as.Date(str_match(ID, "^U(\\d{6})-")[seq.int(n()), 2], format = "%y%m%d"),
    Year = as.factor(year(Date)),
    Month = as.factor(month(Date)),
    Week = as.factor(week(Date)),
    yearmonth = as.factor(paste(Year, Month, sep = '-'))
  )

unique(wild_2007_2018$Class)
unique(wild_2007_2018$Order)
unique(wild_2007_2018$Family)

#merge together and make into one dataset
wild_all <-
  bind_rows(
    wild_1948_1981,
    wild_1982_1985,
    wild_1986_2006,
    wild_1992_2006,
    wild_2007_2018,
    .id = "origin"
  ) %>%
  mutate(
    origin = as.numeric(origin),
    ID = as.factor(ID),
    Year = as.factor(Year),
    Month = as.factor(Month),
    week = as.factor(Week),
    yearmonth = as.factor(yearmonth)
  )

# load regional (län) codes
regions <-
  read.csv2(
    "data/geo/county_codes.csv",
    sep = ";",
    dec = ",",
    na.strings = "",
    colClasses = rep("factor", 5)
  )

# add column for domestic/international
wild_all <- wild_all %>%
  mutate(International = Region %in% c("NORGE", "TYSKLAND", "DK", "ÅLAND", "SF", "ESTLAND"))
  
wild_all %>%
  filter(!(is.na(Region) | Region %in% regions$Code | International)) %>%
  select(origin, ID, Region, Place) %>%
  arrange(ID)

wild_all <- wild_all %>%
  mutate(Region = ifelse(!(
    is.na(Region) | Region %in% regions$Code | International
  ),
  NA,
  Region))

# visualize the trend and distribution 

# library(ggplot2)
# library(RColorBrewer)
# 
# All_species_time_geo$Year <- as.factor(All_species_time_geo$Year)
# colourCount = length(unique(All_species_time_geo$Year))
# getPalette = colorRampPalette(brewer.pal(73, "PuBuGn"))
# 
# annual_trend <- ggplot(All_species_time_geo) + geom_histogram(aes(factor(Year)), fill=getPalette(colourCount), stat="count") + theme_classic() + theme(axis.text.x = element_text(size= 6, angle = 45))                   
# annual_trend_animal2 <- ggplot(All_species_time_geo, aes(x=Year, fill=Class)) + geom_bar(width = 0.6, position = "stack") + scale_fill_brewer(palette="BrBG") + theme_classic() + theme(axis.text.x = element_text(size= 6, angle = 45))

#subsetting for specific classes of interest (mammalia and aves), and plot

mammals <- wild_all %>% filter(Class =="Mammalia")

(annual_trend_mammalia <-
  ggplot(mammals, aes(fill = Order, x = Year)) +
  geom_bar(position = "stack", stat = "count") +
  theme_classic() + theme(axis.text.x = element_text(size = 6, angle = 45)))

birds <- wild_all %>% filter(Class =="Aves")
sort(unique(birds$Order))

(annual_trend_aves <-
  ggplot(birds, aes(fill = Order, x = factor(Year))) +
  geom_bar(position = "stack", stat = "count") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 6, angle = 45)))

#plot to check if there is any seasonality 

(
  seasonal_trend  <- ggplot(wild_all) +
    geom_histogram(aes(Month), stat = "count") +
    theme_classic()
)

(
  seasonal_trend_animal <-
    ggplot(wild_all, aes(x = Month, fill = Class)) +
    geom_bar(width = 0.6, position = "stack") +
    scale_fill_brewer(palette = "BrBG") +
    theme_classic()
)

(
  seasonal_trend_mammalia <-
    ggplot(mammals, aes(fill = Order, x = Month)) +
    geom_bar(position = "stack", stat = "count") +
    theme_classic()
)
(
  seasonal_trend_aves <- ggplot(birds, aes(fill = Order, x = Month)) +
    geom_bar(position = "stack", stat = "count") +
    theme_classic()
)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

map <- st_read('data/geo/Lan_Sweref99TM_region.shp')

reg_cnt <- wild_all %>%
  filter(!International & !is.na(Region)) %>%
  group_by(Region) %>%
  summarise(n = n(), mca = Mode(Animal))

regs_w_cnt <- regions %>%
  left_join(reg_cnt, by = c("Code" = "Region"))

tot_cnt <- regs_w_cnt %>%
  group_by(New.Code) %>%
  summarise(n = sum(n),)

regs_w_cnt <- regs_w_cnt %>%
  select(-n) %>%
  inner_join(tot_cnt, by = c("Code" = "New.Code")) %>%
  mutate(Code = as.factor(Code))

map %>%
  left_join(
    regs_w_cnt %>% select(Numbercode, n, Lettercode = New.Code, mca),
    by = c("LnKod" = "Numbercode")
  ) %>%
  ggplot() +
  geom_sf(aes(fill = n)) +
  geom_sf_label(aes(label = mca, alpha = 0.5), show.legend = F) +
  labs(title = "Total number of wildlife cases\n(all time)")
ggsave("data/output/case_count_mca.png")
