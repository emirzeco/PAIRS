# # # # # # # # # # #
# DATA PREPERATION  #
# # # # # # # # # # #

# Setup ----
## Packages ----
# if ("convenience" %in% rownames(installed.packages()) ==F) {
#   devtools::install_github("ratsupaltuf/convenience", force=T)
# }

packages <- c("tidyverse", "haven", "pastecs", "datawizard", #"convenience",
              "ggplot2", "ggrepel", "sjPlot", "lme4", "knitr", "kableExtra", 
              "stringr", "flextable", "officer", "sf", "plm", "stargazer",
              "patchwork", "tidytext", "sjlabelled")
install.packages(setdiff(packages, rownames(installed.packages())))
suppressMessages(lapply(packages, library, character.only = TRUE, quietly=T))
rm(packages)
options(max.print=10000)






# Folder path
path <- "/posit_share/home/zecovic-e/data"
#path <- "C:/Users/Emir  PC/Desktop/PhD/freda/data/pairfam"

# Variables you want (excluding wave, since we add it ourselves)
vars_needed <- c(
  # Admin
  "id", "wave", "original_sex", "original_doby",
  #"hc1pxi2",                  # Bundesland Wohnort x (W1) (NOT WORKING)
  "sd4g",                     # Geschlecht Partner   (W1)
  
  # Education
  "sd27",                     # Höchster allgemeinbildender Schulabschluss
  "sd28",                     # Berufsausbildung oder Studium abgeschlossen
  
  # Job Status
  #"sd23_",                    # Aktuelle (Aus-)Bildung / Erwerbstätigkeit (Status Quo)
  "sd23i1",
  "sd23i2",
  "sd23i3",
  "sd23i4",
  "sd23i5",
  "sd23i6",
  "sd23i7",
  "sd23i8",
  "sd23i9",
  "sd23i9o",
  "sd23i10",
  "sd23i11",
  "sd23i12",
  "sd23i13",
  "sd23i14",
  "sd23i15",
  "sd23i16",
  "sd23i16o",
  "sd23i17",
  "sd23i18",
  "sd23i19",
  "sd23i20",
  "sd23i21",
  "sd23i22",
  
  #"ehc19_",                   # Aktuelle (Aus-)Bildung / Erwerbstätigkeit (EHC)
  "ehc19i1",
  "ehc19i2",
  "ehc19i3",
  "ehc19i4",
  "ehc19i5",
  "ehc19i6",
  "ehc19i7",
  "ehc19i8",
  "ehc19i9",
  "ehc19i9o",
  "ehc19i10",
  "ehc19i11",
  "ehc19i12",
  "ehc19i13",
  "ehc19i14",
  "ehc19i15",
  "ehc19i16",
  "ehc19i16o",
  "ehc19i17",
  "ehc19i18",
  "ehc19i19",
  "ehc19i20",
  "ehc19i21",
  "ehc19i22",
  "ehc19i22o",
  "ehc19i23",
  
  # Partnership
  "sd3",                      # Aktueller Partnerschaftsstatus: Beziehung vorhanden
  "sd10",                     # Aktueller Familienstand
  "sd7e1",                    # Aktuelle Kohabitation mit aktuellem Partner
  "sd11",                     # Mit aktuellem Partner verheiratet
  
  # _______________
  #"ehc2px",                   # Beziehung mit Partner x jetzt
  #"ehc3px",                   # Zusammenleben mit Partner x jetzt
  #"ehc4px",                   # Ehe mit Partner x jetzt
  
  # Partnership Duration (W2-W13)
  #"sd5ezbm",                  # Beginn Beziehung mit aktuellem Partner: Monat (NOT WORKING)
  #"sd5ezby",                  # Beginn Beziehung mit aktuellem Partner: Jahr  (NOT WORKING)
  "sd8e1bm",                   # Beginn aktuelle Kohabitation mit aktuellem Partner: Monat
  "sd8e1by",                   # Beginn aktuelle Kohabitation mit aktuellem Partner: Jahr
  
  # Life satisfaction
  "sat6",
  
  # Health Status
  "hlt1",                     # Gesundheitszustand letzte 4 Wochen
  
  # Children
  #"ehc10kx",                  # Kohabitation mit Kind x (Not Working!)
  
  # HH-income
  "inc13",                    # Monatliches Haushaltseinkommen Netto
  "inc23",                    # Monatliches Haushaltseinkommen Kategorie
  #"inc18",                    # Anzahl Personen, die zum Haushaltseinkommen beitragen
  "inc24",                    # Was ist Ihr Anteil am Haushaltseinkommen (in Prozent) (W2-W3)
  "inc28",                    # Zufriedenheit mit finanzieller Situation des Haushalts
  
  "inc27i2",                  # HH: Wir müssen häufig verzichten, wegen finanzieller Einschränkungen (W2-W14)
  "inc27i3"                   # HH: Bei uns ist das Geld meistens knapp (W2-W14)
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