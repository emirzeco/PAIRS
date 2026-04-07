# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# R code for                                                                              #
# FReDA Dissertation Paper Emir Zecovic (11.2025)                                         #
# Means-Tested Benefits and Relationship Satisfaction among Low-Income Couples in Germany #                                                                
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # #
# DATA PREPERATION  #
# # # # # # # # # # #

# Setup ----
## Packages ----
if ("convenience" %in% rownames(installed.packages()) ==F) {
  devtools::install_github("ratsupaltuf/convenience", force=T)
}

packages <- c("tidyverse", "haven", "pastecs", "datawizard", "convenience",
              "ggplot2", "ggrepel", "sjPlot", "lme4", "knitr", "kableExtra", 
              "stringr", "flextable", "officer", "sf", "plm", "stargazer",
              "patchwork", "tidytext", "sjlabelled")
install.packages(setdiff(packages, rownames(installed.packages())))
suppressMessages(lapply(packages, library, character.only = TRUE, quietly=T))
rm(packages)
options(max.print=10000)


## Load ----
#f_long<-haven::read_dta("FREDAanchor_long.dta")
f <-haven::read_dta("FREDAanchor_balanced.dta")


## Rename ----
f <- select(f,
            "id", "pid", "welle", "sample",
            "sex", "sex_reg", "age", "age_reg",
            
            "school", "educy", "voctrain", # Wave 1, 4, 5, 6, 7 (included in balanced panel)
            
            "migback", "cohort", "east",
            "samesex",                     # Wave 2, 4, 5, 6, 7
            
            # Income
            "hhincnet",
            hhinccat = "inc23",
            "hhincgcee",
            "hhincoecd",
            sub_fin_hh = "inc52",  # in all Waves
            
            # Un/employment
            "lfstat",                 # Erwerbsstatus, Anker               (Wave 2, 4, 5, 6, 7 (NOT IN WAVE 3))
            "sd55",                   # Derzeitige Situation, Anker        (Wave 1, 2, 3)
            "job40",                  # Erwerbstätigkeit Vollzeit/Teilzeit (Wave 2, 3, 5, 7)
            
            # Means-tested benefits
            grundsich   = "inc53i8",  # Wave 3, 4, 6
            wohngeld    = "inc53i24", # Wave 3, 4, 6
            aII         = "inc53i13", # Wave 3, 4, 6
            #krankgeld  = "inc53i10", # Wave 3, 4, 6
            #kindergeld = "inc53i22", # Wave 3, 4, 6
            #aI         = "inc53i12", # Wave 3, 4, 6
            
            # satisfaction relationship
            satrelship = "sat3",      # in all Waves
            lifesat    = "sat6",      # in all Waves
            
            # Dyadic Adjustment Scale
            #"pa18i1": "pa18i16",     # only in Wave 6

            # Relationship status for Wave 3 (W1B)
            "pstat",                  # Indikator aktuelle Beziehung    (all Waves)
            "separation",             # Trennung seit letzter Teilnahme (Wave 2, 3, 4, 5, 6 ,7)
            "sd3",                    # Beziehung vorhanden             (Wave 1, 2, 3)
            "bpa17",                  # Beziehung vorhanden             (Wave 4, 5, 6, 7)
            
            # Relationship status for all other waves
            cohab      = "bpa4",      # Wave 4, 5, 6, 7 (coding changed in wave 7)
            married    = "bpa7",      # Wave 4, 5, 6, 7
            "relstat"                 # Aktueller Beziehungsstatus, Anker (Wave 2, 4, 5, 6, 7)
            )


# Recoding ----
## relstat ----
#alabs(f$relstat) #get_labels(f$relstat, values = "as.name")
table(f$relstat, f$welle, useNA = "ifany")

f <- f %>%
  mutate(
    relstat3 = case_when(
      relstat %in% c(2, 7, 10) ~ "LAT",
      relstat %in% c(3, 8, 11) ~ "Cohabiting",
      relstat == 4             ~ "Married",
      TRUE                     ~ NA_character_
    ),
    relstat3 = factor(relstat3, 
                      levels = c("LAT", "Cohabiting", "Married"))
    )
table(f$relstat3, f$welle, useNA = "ifany")

### Fill W1B ----
f <- f %>%
  group_by(id) %>%
  mutate(relstat3_w2 = relstat3[welle == 2][1]) %>%  # relationship status in wave 2
  ungroup()

f <- f %>%
  mutate(
    relstat3_filled = case_when(
      
      # 1) Keep any observed relstat3 as is (all waves)
      !is.na(relstat3) ~ relstat3,
      
      # 2) SAFE FILLING for WAVE 3:
      #    - relstat3 missing in wave 3
      #    - wave 2 indicates LAT / Cohabiting / Married
      #    - wave 3 shows partner presence (pstat==1 or sd3==1)
      #    - AND no separation reported in wave 3
      is.na(relstat3) &
        welle == 3 &
        relstat3_w2 %in% c("LAT", "Cohabiting", "Married") &
        (pstat == 1 | sd3 == 1) &
        separation != 1 ~ relstat3_w2,
      
      # 3) OPTIONAL: if wave-3 partner indicators are missing but no separation,
      #    we can still carry over wave-2 status (more liberal but still plausible):
      is.na(relstat3) &
        welle == 3 &
        relstat3_w2 %in% c("LAT", "Cohabiting", "Married") &
        (is.na(pstat) & is.na(sd3)) &
        separation != 1 ~ relstat3_w2,
      
      # 4) CONTRADICTIONS → do NOT fill
      
      # Wave 3 explicitly says "no partner"
      is.na(relstat3) &
        welle == 3 &
        (pstat == 0 | sd3 == 2) ~ NA_character_,
      
      # Wave 3 explicitly reports separation
      is.na(relstat3) &
        welle == 3 &
        separation == 1 ~ NA_character_,
      
      # 5) Default: keep original relstat3 (NA here)
      TRUE ~ relstat3
    )
  )
table(f$relstat3_filled, f$welle)


## lfstat ----
table(f$lfstat, f$welle, useNA = "ifany")

f <- f %>%
  mutate(
    # Clean versions: set negative codes to NA
    sd55_clean  = ifelse(sd55  >= 1 & sd55  <= 12, sd55,  NA_real_),
    job40_clean = ifelse(job40 >= 1 & job40 <=  3, job40, NA_real_),
    
    lfstat_w3_rec = case_when(
      # only for wave 3
      welle != 3 ~ NA_real_,        
      
      sd55_clean == 1 ~ 1,          # In Ausbildung
      sd55_clean %in% c(9, 10) ~ 2, # Elternzeit / Mutterschutz
      sd55_clean == 8 ~ 3,          # Hausmann/Hausfrau
      sd55_clean == 5 ~ 4,          # Arbeitslos
      sd55_clean == 7 ~ 5,          # Freiwilligendienst
      sd55_clean %in% c(6, 11) ~ 6, # Ruhestand / arbeitsunfähig
      
      # Angestellt (split by job40)
      sd55_clean == 2 & job40_clean == 1 ~ 7,  # Vollzeit
      sd55_clean == 2 & job40_clean == 2 ~ 8,  # Teilzeit
      sd55_clean == 2 & job40_clean == 3 ~ 9,  # Geringfügig
      # sd55 == 2 but job40 missing -> stays NA (conservative)
      sd55_clean == 3 ~ 10,                    # Selbstständig
      sd55_clean %in% c(4, 12) ~ 11,           # Mithelfendes Familienmitglied oder Anderes
      TRUE ~ NA_real_
    ),
    
    # Keep original where present, otherwise fill for wave 3
    lfstat_filled = dplyr::coalesce(
      lfstat,       # original lfstat in all other wave
      lfstat_w3_rec # Wave 3 only
      )
    )
table(f$lfstat_filled, f$welle)


# Missings ----
vars_main <- c(
  "age",
  #"sex", "", "school", "educy", "voctrain",
  "hhincgcee", 
  #"sub_fin_hh",
  #"hhincnet", "hhincoecd",
  #"lfstat_filled", #"lfstat",
  #"sd55", "job40",
  "grundsich", "wohngeld", "aII",
  "satrelship",
  "satlife"
  #"pstat", "separation", "sd3",
  #"relstat3_filled" #"relstat"
  )

f_reduc <- f
f_reduc[vars_main][f_reduc[vars_main] < 0] <- NA
summary(f_reduc[vars_main])

## Remove NAs ----
prop.table(table(complete.cases(f_reduc[vars_main])))
f_reduc <- f_reduc[complete.cases(f_reduc[vars_main]), ]
#rm(vars_main)







# Count ----
benefit_summary <- f_reduc %>%
  group_by(sample) %>%
  summarise(
    couples        = n(),
    grundsich      = sum(grundsich == 1),
    wohngeld       = sum(wohngeld == 1),
    aII            = sum(aII == 1)
  )
benefit_summary

f_reduc %>%
  filter(!is.na(relstat3_filled)) %>%
  group_by(welle) %>%
  summarise(
    grundsich = sum(grundsich == 1, na.rm = TRUE),
    wohngeld  = sum(wohngeld == 1,  na.rm = TRUE),
    aII       = sum(aII == 1,       na.rm = TRUE),
    .groups = "drop"
    )











# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Benefit ----
##  Define benefit variables
# benefit_vars <- c("grundsich",
#                   "wohngeld",
#                   "aII")
# 
# ## 2.2 Recode each benefit: 1 = receives, 0 = does not, NA = missing / filter
# f<- f %>%
#   mutate(
#     across(
#       all_of(benefit_vars),
#       ~ case_when(
#         .x == 1 ~ 1,
#         .x == 0 ~ 0,
#         TRUE    ~ NA_real_
#       ),
#       .names = "{.col}_rec"
#     ),
# 
#     ## 2.3 Intensity: how many different benefits (0–5)?
#     benefit_count   = rowSums(across(ends_with("_rec")), na.rm = TRUE),
# 
#     ## 2.4 How many benefit vars are non-missing?
#     benefit_nonmiss = rowSums(!is.na(across(ends_with("_rec")))),
# 
#     ## 2.5 Binary: any benefit vs none vs no info
#     any_benefit = case_when(
#       benefit_nonmiss == 0       ~ NA_real_,  # no info on any benefit
#       benefit_count > 0          ~ 1,
#       benefit_count == 0         ~ 0
#     )
#     )