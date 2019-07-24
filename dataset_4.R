library(tidyverse)
library(stringr)

data_dir <- "data/"

date_pattern <- "^U(\\d{6})-"
start_date <- as.Date("2007-01-01")
end_date <- as.Date("2011-12-31")

escape_regex <- function(string) {
  gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", string)
}

# Extract date from UppdragID and filter out rows outside desired date range
vlt_data <-
  read.csv2(str_c(data_dir, "4.2007-2018.csv"),
            dec = ",",
            na.strings = c("", " ")) %>%
  mutate(Datum = as.Date(str_match(UppdragID, date_pattern)[seq.int(n()), 2], format = "%y%m%d")) %>%
  filter(between(Datum, start_date, end_date)) %>%
  mutate_if(is.character, str_trim)

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

# Load diagnosis definition table
diagnoses <-
  read.csv2(str_c(data_dir, "diagnoser.csv"), dec = ',') %>%
  mutate_if(is.character, str_trim)

names_se <- paste(str_c('(', escape_regex(trimws(str_to_lower(diagnoses$Namn, locale = "sv"))), ')'), collapse = '|')
names_en <- str_to_lower(diagnoses$Namngb, locale = "en")

# Find entries whose diagnosis exists exactly in definition table
diagnosed <- vlt_relevant %>%
  filter(
    str_to_lower(Diagnoser) %in% names_se |
      str_to_lower(Diagnoser...Engelska) %in% names_en
  )

# Save those whose do not (should mostly be entries with multiple diagnoses)
undiagnosed <- setdiff(vlt_relevant, diagnosed)

# Sort out entries that do not have the same number of diagnoses in Swedish as in English (for some reason...)
problematic <- undiagnosed %>%
  filter(lengths(str_split(Diagnoser, ', ')) != lengths(str_split(Diagnoser...Engelska, ', ')))

# Save those that do and split the diagnoses into separate rows
unproblematic <- setdiff(undiagnosed, problematic) %>%
  separate_rows(Diagnoser, Diagnoser...Engelska, sep = ', ')

unproblematic_diagnosed <- unproblematic %>%
  filter(
    str_to_lower(Diagnoser) %in% names_se |
      str_to_lower(Diagnoser...Engelska) %in% names_en
  )

unproblematic_undiagnosed <-
  setdiff(unproblematic, unproblematic_diagnosed)
