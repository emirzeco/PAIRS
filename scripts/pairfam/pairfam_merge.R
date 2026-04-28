# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# R code for                                                                              # 
# Means-Tested Benefits and Relationship Satisfaction among Low-Income Couples in Germany #
# Author: Emir Zecovic                                                                    #
# Last Update: 07.04.2026                                                                 #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # # # 
# DATA MERGE PAIRFAM  #
# # # # # # # # # # # #

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


# Load data ----

## Mode merge ----
### Wave 12 ----
w12_capi <- haven::read_dta("/posit_share/home/zecovic-e/PAIRS/data/pairfam/anchor12_capi.dta")
w12_cati <- haven::read_dta("/posit_share/home/zecovic-e/PAIRS/data/pairfam/anchor12_cati.dta")
# w12_capi <- haven::read_dta("C:/Users/Emir  PC/Desktop/PhD/Paper1/PAIRS/data/pairfam/anchor12_capi.dta")
# w12_cati <- haven::read_dta("C:/Users/Emir  PC/Desktop/PhD/Paper1/PAIRS/data/pairfam/anchor12_cati.dta")
w12 <- rbind(w12_capi, w12_cati) %>%
  arrange(id)

### Wave 13 ----
w13_capi <- haven::read_dta("/posit_share/home/zecovic-e/PAIRS/data/pairfam/anchor13_capi.dta")
w13_cati <- haven::read_dta("/posit_share/home/zecovic-e/PAIRS/data/pairfam/anchor13_cati.dta")
# w13_capi <- haven::read_dta("C:/Users/Emir  PC/Desktop/PhD/Paper1/PAIRS/data/pairfam/anchor13_capi.dta")
# w13_cati <- haven::read_dta("C:/Users/Emir  PC/Desktop/PhD/Paper1/PAIRS/data/pairfam/anchor13_cati.dta")
w13 <- rbind(w13_capi, w13_cati) %>%
  arrange(id)

### Wave 14 ----
w14_capi <- haven::read_dta("/posit_share/home/zecovic-e/PAIRS/data/pairfam/anchor14_capi.dta")
w14_cawi <- haven::read_dta("/posit_share/home/zecovic-e/PAIRS/data/pairfam/anchor14_cawi.dta")
w14_papi <- haven::read_dta("/posit_share/home/zecovic-e/PAIRS/data/pairfam/anchor14_papi.dta")
# w14_capi <- haven::read_dta("C:/Users/Emir  PC/Desktop/PhD/Paper1/PAIRS/data/pairfam/anchor14_capi.dta")
# w14_cawi <- haven::read_dta("C:/Users/Emir  PC/Desktop/PhD/Paper1/PAIRS/data/pairfam/anchor14_cawi.dta")
# w14_papi <- haven::read_dta("C:/Users/Emir  PC/Desktop/PhD/Paper1/PAIRS/data/pairfam/anchor14_papi.dta")
w14 <- bind_rows(w14_capi, w14_cawi, w14_papi) %>%
  arrange(id)

### Save ----
# haven::write_dta(w12, "C:/Users/Emir  PC/Desktop/PhD/Paper1/PAIRS/data/pairfam/anchor12.dta")
# haven::write_dta(w13, "C:/Users/Emir  PC/Desktop/PhD/Paper1/PAIRS/data/pairfam/anchor13.dta")
# haven::write_dta(w14, "C:/Users/Emir  PC/Desktop/PhD/Paper1/PAIRS/data/pairfam/anchor14.dta")




# Long format ----
#path <- "/posit_share/home/zecovic-e/data/pairfam"
path <- "C:/Users/Emir  PC/Desktop/PhD/Paper1/PAIRS/data/pairfam"
#path <- "/posit_share/home/zecovic-e/PAIRS/data/pairfam/"
files <- paste0("anchor", 1:14, ".dta")
files



## Variables
vars <- c(
  "id", "pid", "wave", "sample",        # ID, PID, Wave, Sample
  "sex_gen", "psex_geg",                # Sex (+partner)
  "age", "page",                        # Age/Partner Age
  "ykage", "k1age", "k2age", "k3age",   # Children Age
  "cohort", "migstatus", "pmigstatus",  # Birth cohort/ Mig.status/ Partner Mig.status
  "east",                               # East Germany
  "homosex", "homosex_new",             # Anchor's sexual orientation
  
  "crn20i1", "crn20i2", "crn20i3", "crn20i4", 
  
  "sat3",                               # Relationship satisfaction
  "sat4",                               # Relationship satisfaction (partner)
  
  "relstat", "marstat", "pmarstat",     # Relationship status/ Marital status (anchor, partner)
  "np", "ncoh",                         # Number of previous (+cohabited) partners
  "meetdur",                            # Months since anchor and current partner got to know each other  
  "reldur", "cohabdur", "mardur",       # Duration of current relationship, cohabitation and marriage
  
  "nkidsliv", "nkids", "childmrd",      # Number of all kids living with anchor / Number of children living in household
  "hhcomp", "hhsizemrd",                # Household composition                 / Household size
  "pmrd",                               # Partner lives in household

  "enrol", "penrol",                    # Enrollment in school or vocational qualification at time of interview (anchor, partner)
  "school", "pschool",                  # Highest school degree (+partner)
  "isced",
  "vocat", "pvocat",                    # Highest vocational degree (+partner)
  "yeduc", "pyeduc",                    # Years of schooling (+partner)
  "lfs", "plfs",                        # Labor force status (anchor, partner)
  
  "hhincgcee", "hhincoecd",             # HH-Income (GCEE, OECD)
  "incnet", "hhincnet",                 # Net personal, Net HH
  "inc28",                              # Zufriedenheit mit finanzieller Situation des Haushalts
  "inc26i2",
  "inc26i3",
  "inc27i2",                            # HH: Wir müssen häufig verzichten, wegen finanzieller Einschränkungen (W2-W14)
  "inc27i3",                            # HH: Bei uns ist das Geld meistens knapp                              (W2-W14)
  
  #"inc10i1",                           # Kindergeld
  #"inc10i2",                           # Lohnfortzahlung im Mutterschutz
  #"inc10i3",                           # Elterngeld
  "inc10i4",                            # Wohngeld oder Lastenzuschuss
  #"inc10i5",                           # Leistungen der Pflegeversicherung
  "inc10i7",                            # Sozialhilfe
  "inc10i8",                            # Arbeitslosengeld I (ALG I)
  "inc10i9",                            # Arbeitslosengeld II einschließlich Sozialgeld
  "inc10i10",                           # Grundsicherung im Alter und bei Erwerbsminderung
  "inc10i11",                           # Krankengeld
  
  "casprim", "cassec",                  # Current primary (anchor, partner)
  "pcasprim", "pcassec",                # Secondary activity status
  
  "sat6",                               # Life satisfaction 
  "pcs", "mcs",                         # Summary score physical and mental health
  "hlt1",                               # Gesundheitszustand letzte 4 Wochen
  "per1i6"
  )


## Unbalanced ----
pairfam_long <- lapply(files, function(f) {
  read_dta(file.path(path, f)) %>%
    select(any_of(vars))
}) %>%
  bind_rows() %>%
  arrange(id, wave)

## Balanced ----
pairfam_balanced <- pairfam_long %>%
  group_by(id) %>%
  filter(n_distinct(wave) == 14) %>%
  ungroup()


# Save ----
haven::write_dta(pairfam_long, "pairfam_long.dta")
haven::write_dta(pairfam_balanced, "pairfam_balanced.dta")
# saveRDS(pairfam_long, "pairfam_long.rds")
# saveRDS(pairfam_long, "pairfam_balanced.rds")