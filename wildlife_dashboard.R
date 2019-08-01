Sys.setlocale("LC_ALL", "swedish")

library(tidyverse)
library(stringr)
library(lubridate)

# manual edits have been done in csv copies of the original data files. These include:
#   -removing unnecessary (and adding necessary) whitespace
#   -repairing broken rows with missing columns
#   -swapping class and order columns (they were named wrong in some tables)
#   -fixing incorrect classification fields:
#       *renaming swedish fields to latin
#       *filling in some blanks
#       *correcting spelling errors

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
  select(ID = V.nr,
         Animal = Djurslag,
         Family,
         Order,
         Class,
         Region = Län,
         Place = Ort,
         Date = Datum) %>%
  mutate(
    Region = as.factor(str_to_upper(Region)),
    Date = as.Date(as.character(Date), format = "%Y%m%d"),
    Year = year(Date),
    Month = month(Date),
    Week = week(Date),
    yearmonth = paste(Year, Month, sep = '-')
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
  select(ID = V.nr,
         Animal = Djurslag,
         Family,
         Order,
         Class,
         Region = Län,
         Place = Ort,
         Date = Datum) %>%
  mutate(
    Region = as.factor(str_to_upper(Region)),
    Date = as.Date(as.character(Date), format = "%Y%m%d"),
    Year = year(Date),
    Month = month(Date),
    Week = week(Date),
    yearmonth = paste(Year, Month, sep = '-')
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
  Region = as.factor(str_to_upper(Region)),
  Gisx = as.numeric(Gisx),
  Gisy = as.numeric(Gisy),
  Date = as.Date(ifelse(
    length(as.character(Date)) == 5,
    paste('0', as.character(Date)),
    as.character(Date)
  ), format = "%y%m%d"),
  Year = year(Date),
  Month = month(Date),
  Week = week(Date),
  yearmonth = paste(Year, Month, sep = '-')
)

unique(wild_1986_2006$Class)
unique(wild_1986_2006$Order)
unique(wild_1986_2006$Family)

variable3 <- c("UPPDRAGSID", "DJURSLAG", "Family", "Order", "Class", "INSPOSTADRESS", "X_KOORDINAT", "Y_KOORDINAT", "Date", "Year", "Month", "Week", "yearmonth")
W4_species_time_geo <- wild_1992_2006[variable3]
colnames(W4_species_time_geo)[1] <- "ID"
colnames(W4_species_time_geo)[2] <- "Djurslag"
colnames(W4_species_time_geo)[6] <- "Ort"
colnames(W4_species_time_geo)[7] <- "Gisy"
colnames(W4_species_time_geo)[8] <- "Gisx"

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
    Gisx = as.numeric(Gisx),
    Gisy = as.numeric(Gisy),
    Date = as.Date(as.character(Date), format = "%Y-%m-%d"),
    Year = year(Date),
    Month = month(Date),
    Week = week(Date),
    yearmonth = paste(Year, Month, sep = '-')
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
    Region = as.factor(str_to_upper(Region)),
    Gisx = as.numeric(Gisx),
    Gisy = as.numeric(Gisy),
    Date = as.Date(str_match(ID, "^U(\\d{6})-")[seq.int(n()), 2], format = "%y%m%d"),
    Year = year(Date),
    Month = month(Date),
    Week = week(Date),
    yearmonth = paste(Year, Month, sep = '-')
  )

unique(wild_2007_2018$Class)
unique(wild_2007_2018$Order)
unique(wild_2007_2018$Family)

# 2019-08-01 ends here

#make a dataset for animal species, time and geography
##1992_2006 dataset doesn't have a län column and for 'ort' information there is 'postadress' and 'inpostadress', used 'inpostadress' for now but needs checking.

#W4_species_time_geo$Län <- NA
#W4_species_time_geo <- W4_species_time_geo[c("ID", "Djurslag", "Family","Order", "Class", "Län","Ort", "Gisx", "Gisy", "Date", "Year", "Month", "Week", "yearmonth")]



#merge together and make into one dataset

library(dplyr)
All_species_time_geo <- bind_rows(W1_species_time_geo, W2_species_time_geo, W3_species_time_geo, W4_species_time_geo, W5_species_time_geo)

#to visualize the trend and distribution 
##checked the years and found that there is year 1945 (ID 1950-507- could it be data entry error?) and NAs (ID 1998-0045, 1998-0338, 2000-0649, 2000-0772, 1970-3670) and possibly with wrong date(2004-0911 -with date as 110217)
##also some of the 1995 data were imported as 2035 when imported, so manually changed them back to 1995 after making temporal columns
length(unique(All_species_time_geo$Year))

library(ggplot2)
library(RColorBrewer)

All_species_time_geo$Year <- as.factor(All_species_time_geo$Year)
colourCount = length(unique(All_species_time_geo$Year))
getPalette = colorRampPalette(brewer.pal(73, "PuBuGn"))

annual_trend <- ggplot(All_species_time_geo) + geom_histogram(aes(factor(Year)), fill=getPalette(colourCount), stat="count") + theme_classic() + theme(axis.text.x = element_text(size= 6, angle = 45))                   
annual_trend_animal2 <- ggplot(All_species_time_geo, aes(x=Year, fill=Class)) + geom_bar(width = 0.6, position = "stack") + scale_fill_brewer(palette="BrBG") + theme_classic() + theme(axis.text.x = element_text(size= 6, angle = 45))

#subsetting for specific classes of interest (mammalia and aves), checking/fixing the data, and plot

library(tidyverse)
mammalia_time_geo <- All_species_time_geo %>% filter(All_species_time_geo$Class =="Mammalia")
sort(unique(mammalia_species_time_geo$Order))

All_species_time_geo$Order[All_species_time_geo$Order=="Primattes"] <- "Primates"
All_species_time_geo$Order[All_species_time_geo$Order=="Undefined"] <- NA

annual_trend_mammalia <- ggplot(mammalia_time_geo, aes(fill=Order, x= factor(Year))) + geom_bar(position = "stack", stat="count") + theme_classic() + theme(axis.text.x = element_text(size= 6, angle = 45))

Aves_time_geo <- All_species_time_geo %>% filter(All_species_time_geo$Class =="Aves")
sort(unique(Aves_time_geo$Order))

annual_trend_aves <- ggplot(Aves_time_geo, aes(fill=Order, x= factor(Year))) + geom_bar(position = "stack", stat="count") + theme_classic() + theme(axis.text.x = element_text(size= 6, angle = 45))

#plot to check if there is any seasonality 

colourCount2 = length(unique(All_species_time_geo$Month))
seasonal_trend  <- ggplot(All_species_time_geo) + geom_histogram(aes(factor(Month)), stat="count") + theme_classic()                  
seasonal_trend_animal <- ggplot(All_species_time_geo, aes(x=Month, fill=Class)) + geom_bar(width = 0.6, position = "stack") + scale_fill_brewer(palette="BrBG") + theme_classic() 

seasonal_trend_mammalia <- ggplot(mammalia_time_geo, aes(fill=Order, x= Month)) + geom_bar(position = "stack", stat="count") + theme_classic() 
seasonal_trend_aves <- ggplot(Aves_time_geo, aes(fill=Order, x= Month)) + geom_bar(position = "stack", stat="count") + theme_classic() 
