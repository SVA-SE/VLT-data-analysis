Sys.setlocale("LC_ALL", "swedish")

library(tidyverse)
library(stringr)
library(lubridate)

# import all the wildlife data and fix/extract the dates
# what the dates represent may differ between datasets 
# wild_1986_2006 dates are autopsy dates, while wild_1992_2006 dates are arrival dates. 

wild_1948_1981 <- read.csv2(
  "data/csv/1.1.1948-1981.csv",
  sep = ";",
  dec = ",",
  quote = "\"",
  na.strings = c("", " ", ".", "Not given", "Not relevant", "Undefined", "?"),
  strip.white = TRUE
) %>%
  mutate(
    Date = as.Date(as.character(Datum), format = "%Y%m%d"),
    Year = year(Date),
    Month = month(Date),
    Week = week(Date),
    yearmonth = paste(Year, Month, sep = '-')
  )

unique(wild_1948_1981$Class)

wild_1982_1985 <- read.csv2(
  "data/csv/1.2.1982-1985.csv",
  sep = ";",
  dec = ",",
  quote = "\"",
  na.strings = c("", " ", ".", "Not relevant", "Undefined", "?"),
  strip.white = TRUE
) %>%
  mutate(
    Date = as.Date(as.character(Datum), format = "%Y%m%d"),
    Year = year(Date),
    Month = month(Date),
    Week = week(Date),
    yearmonth = paste(Year, Month, sep = '-')
  )

unique(wild_1982_1985$Class)

wild_1986_2006 <- read.csv2(
  "data/csv/2.1986-2006.csv",
  sep = ";",
  dec = ",",
  quote = "\"",
  na.strings = c("", " ", ".", "Not given", "Not relevant", "Undefined", "?"),
  strip.white = TRUE
) %>%
  mutate(
    Date = as.Date(ifelse(length(Obduktionsdatum.1) == 5, paste('0', as.character(Obduktionsdatum.1)), as.character(Obduktionsdatum.1)), format = "%y%m%d"),
    Year = year(Date),
    Month = month(Date),
    Week = week(Date),
    yearmonth = paste(Year, Month, sep = '-')
  )

unique(wild_1986_2006$Class)

wild_1992_2006 <- read.csv2(
  "data/csv/3.1992-2006.csv",
  sep = ";",
  dec = ",",
  quote = "\"",
  na.strings = c("", " ", ".", "?", "Not relevant", "Not given"),
  strip.white = TRUE
) %>%
  mutate(
    Date = as.Date(as.character(ANKOMSTDATUM), format = "%Y-%m-%d"),
    Year = year(Date),
    Month = month(Date),
    Week = week(Date),
    yearmonth = paste(Year, Month, sep = '-')
    # Class = replace(Class, Class == 0, NA)
  ) 

unique(wild_1992_2006$Class)

wild_2007_2018 <- read.csv2(
  "data/csv/4.2007-2018.csv",
  sep = ";",
  dec = ",",
  quote = "\"",
  na.strings = c("", " ", ".", "?", "#SAKNAS!", "Not given"),
  strip.white = TRUE
) %>%
  mutate(
    Date = as.Date(str_match(UppdragID, "^U(\\d{6})-")[seq.int(n()), 2], format = "%y%m%d"),
    Year = year(Date),
    Month = month(Date),
    Week = week(Date),
    yearmonth = paste(Year, Month, sep = '-')
  )

unique(wild_2007_2018$Class)

# 2019-07-31 ended here

unique(wild_1948_1981$Order)
unique(wild_1982_1985$Order)
unique(wild_1986_2006$Order)
unique(wild_1992_2006$Order)
unique(wild_2007_2018$Order)

unique(wild_1948_1981$Family)
unique(wild_1982_1985$Family)
unique(wild_1986_2006$Family)
unique(wild_1992_2006$Family)
unique(wild_2007_2018$Family)

## fixed some columns (e.g., "0" for order and class, 'class' and 'order' swapped) on the excel file copies in my H 
## and also corrected for the different forms of NAs as below

wild_1982_1985$Class[wild_1982_1985$Class=="Not relevant"] <- NA
wild_1982_1985$Class[wild_1982_1985$Class=="Not given"] <- NA

wild_1986_2006$Class[wild_1986_2006$Class=="Not given"] <- NA
wild_1986_2006$Class[wild_1986_2006$Class=="Not relevant"] <- NA
wild_1986_2006$Class[wild_1986_2006$Class=="Undefined"] <- NA

wild_1992_2006$Class[wild_1992_2006$Class=="Not given"] <- NA
wild_1992_2006$Class[wild_1992_2006$Class=="Not relevant"] <- NA

wild_2007_2018$Class[wild_2007_2018$Class=="Not given"] <- NA

#make a dataset for animal species, time and geography
##1992_2006 dataset doesn't have a län column and for 'ort' information there is 'postadress' and 'inpostadress', used 'inpostadress' for now but needs checking.

variable1 <- c("V-nr", "Djurslag", "Family","Order", "Class", "L?n","Ort", "Date", "Year", "Month", "Week", "yearmonth" )
W1_species_time_geo <- wild_1948_1981[variable1]
W2_species_time_geo <- wild_1982_1985[variable1]
colnames(W1_species_time_geo)[1] <- "ID"
colnames(W2_species_time_geo)[1] <- "ID"

variable2 <- c("V-nr", "DJURSLAG", "Family","Order", "Class", "L?n__1", "Fyndort", "Y-koordinat", "X-koordinat", "Date", "Year", "Month", "Week", "yearmonth" )
W3_species_time_geo <- wild_1986_2006[variable2]
colnames(W3_species_time_geo)[1] <- "ID"
colnames(W3_species_time_geo)[2] <- "Djurslag"
colnames(W3_species_time_geo)[6] <- "Län"
colnames(W3_species_time_geo)[7] <- "Ort"
colnames(W3_species_time_geo)[8] <- "Gisy"
colnames(W3_species_time_geo)[9] <- "Gisx"

W3_species_time_geo$Gisx <- as.numeric(W3_species_time_geo$Gisx)
W3_species_time_geo$Gisy <- as.numeric(W3_species_time_geo$Gisy)

variable3 <- c("UPPDRAGSID", "DJURSLAG", "Family", "Order", "Class", "INSPOSTADRESS", "X_KOORDINAT", "Y_KOORDINAT", "Date", "Year", "Month", "Week", "yearmonth")
W4_species_time_geo <- wild_1992_2006[variable3]
colnames(W4_species_time_geo)[1] <- "ID"
colnames(W4_species_time_geo)[2] <- "Djurslag"
colnames(W4_species_time_geo)[6] <- "Ort"
colnames(W4_species_time_geo)[7] <- "Gisy"
colnames(W4_species_time_geo)[8] <- "Gisx"

#W4_species_time_geo$Län <- NA
#W4_species_time_geo <- W4_species_time_geo[c("ID", "Djurslag", "Family","Order", "Class", "Län","Ort", "Gisx", "Gisy", "Date", "Year", "Month", "Week", "yearmonth")]

variable4 <- c("UppdragID", "Djurslag", "Family","Order", "Class", "L?n","Postort", "Gisx", "Gisy", "Date", "Year", "Month", "Week", "yearmonth" )
W5_species_time_geo <- wild_2007_2018[variable4]
colnames(W5_species_time_geo)[1] <- "ID"
colnames(W5_species_time_geo)[7] <- "Ort"

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
