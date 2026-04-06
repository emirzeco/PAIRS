# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# R code for                                                                              # 
# Means-Tested Benefits and Relationship Satisfaction among Low-Income Couples in Germany #
# Author: Emir Zecovic                                                                    #
# Last Update: 06.04.2026                                                                 #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # # # # # #
# DATA PREPERATION PAIRFAM  #
# # # # # # # # # # # # # # # 






# Variables ----



vars_needed <- c(
  # Education
  "sd27",                     # Höchster allgemeinbildender Schulabschluss
  "sd28",                     # Berufsausbildung oder Studium abgeschlossen
  
  # Partnership
  "sd3",                      # Aktueller Partnerschaftsstatus: Beziehung vorhanden (W1)
  "sd10",                     # Aktueller Familienstand                             (W1)
  "sd7e1",                    # Aktuelle Kohabitation mit aktuellem Partner         (W1)
  "sd11",                     # Mit aktuellem Partner verheiratet                   (W1)
  
  # Partnership Duration (W2-W13)
  "reldur",
  "sd5ezbm",                  # Beginn Beziehung mit aktuellem Partner: Monat (NOT WORKING)
  "sd5ezby",                  # Beginn Beziehung mit aktuellem Partner: Jahr  (NOT WORKING)
  "sd8e1bm",                  # Beginn aktuelle Kohabitation mit aktuellem Partner: Monat
  "sd8e1by",                  # Beginn aktuelle Kohabitation mit aktuellem Partner: Jahr
  
  # Children
  #"ehc10kx",                  # Kohabitation mit Kind x (Not Working!)
  
  # HH-income
  "inc13",                    # Monatliches Haushaltseinkommen Netto
  "inc23",                    # Monatliches Haushaltseinkommen Kategorie
  #"inc18",                   # Anzahl Personen, die zum Haushaltseinkommen beitragen


)

# Helper: read one file, keep only available vars, add missing vars as NA, add wave/source
read_anchor_subset <- function(file, wave_num, vars_needed) {
  dat <- haven::read_dta(file)
  
  # pairfam files are usually already lowercase, but this makes it safer
  names(dat) <- tolower(names(dat))
  vars_needed <- tolower(vars_needed)
  
  # Keep only available variables
  keep <- intersect(names(dat), vars_needed)
  dat <- dat[, keep, drop = FALSE]
  
  # Add missing variables as NA so bind_rows works consistently
  missing_vars <- setdiff(vars_needed, names(dat))
  for (v in missing_vars) dat[[v]] <- NA
  
  # Reorder columns consistently
  dat <- dat[, vars_needed, drop = FALSE]
  
  # Add wave and source metadata
  dat$wave <- wave_num
  dat$source_file <- basename(file)
  
  dat
}

# Waves 1-11: regular anchor files
waves_1_11 <- lapply(1:11, function(w) {
  f <- file.path(path, paste0("anchor", w, ".dta"))
  read_anchor_subset(f, w, vars_needed)
})

# Waves 12-14: mode-specific anchor files (capi/cati/cawi/papi)
read_mode_wave <- function(wave_num, path, vars_needed) {
  files <- list.files(
    path = path,
    pattern = paste0("^anchor", wave_num, "_.*\\.dta$"),
    full.names = TRUE
  )
  
  # Keep interview mode files only
  files <- files[grepl("_(capi|cati|cawi|papi)\\.dta$", basename(files), ignore.case = TRUE)]
  
  if (length(files) == 0) return(NULL)
  
  dat_list <- lapply(files, function(f) read_anchor_subset(f, wave_num, vars_needed))
  dplyr::bind_rows(dat_list)
}

waves_12_14 <- lapply(12:14, function(w) read_mode_wave(w, path, vars_needed))

# Combine all waves
pairfam_long <- dplyr::bind_rows(waves_1_11, waves_12_14)

# Final column order (wave first is convenient)
pairfam_long <- pairfam_long %>%
  dplyr::relocate(id, wave, source_file)

# Quick checks
dplyr::glimpse(pairfam_long)
table(pairfam_long$wave, useNA = "ifany")


pairfam_long %>%
  dplyr::count(id, wave) %>%
  dplyr::count(n)





# Save ----
# haven::write_dta(pairfam_long, "pairfam_long.dta")
# saveRDS(pairfam_long, "pairfam_long.rds")