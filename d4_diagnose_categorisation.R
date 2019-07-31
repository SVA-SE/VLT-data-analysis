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