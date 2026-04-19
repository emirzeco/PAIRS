# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# R code for                                                                              # 
# Means-Tested Benefits and Relationship Satisfaction among Low-Income Couples in Germany #
# Author: Emir Zecovic                                                                    #
# Last Update: 07.04.2026                                                                 #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # # # # # #
# DATA PREPERATION FReDA    #
# # # # # # # # # # # # # # # 

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
#f_long<-haven::read_dta("data/FREDAanchor_long.dta")
f <-haven::read_dta("data/FREDAanchor_balanced.dta")

## Rename ----
f <- select(f,
            "id", "pid", wave = "welle", "sample",
            "sex", "psexgen", "age", "migback",
            "cohort", "east",
            "samesex",                     # Wave 2, 4, 5, 6, 7
            "school", "educy", "voctrain", # Wave 1, 4, 5, 6, 7 (included in balanced panel)
            
            # Income
            #"hhincnet",
            #hhinccat = "inc23",
            "hhincgcee", 
            "hhincoecd",
            #sub_fin_hh = "inc52",  # in all Waves
            
            # Un/employment
            "lfstat",                 # Erwerbsstatus, Anker               (Wave 2, 4, 5, 6, 7 (NOT IN WAVE 3))
            "sd55",                   # Derzeitige Situation, Anker        (Wave 1, 2, 3)
            "job40",                  # Erwerbstätigkeit Vollzeit/Teilzeit (Wave 2, 3, 5, 7)
            
            # Means-tested benefits
            grundsich   = "inc53i8",  # Wave 3, 4, 6
            aII         = "inc53i13", # Wave 3, 4, 6
            wohngeld    = "inc53i24", # Wave 3, 4, 6
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
            "relstat",                # Aktueller Beziehungsstatus, Anker (Wave 2, 4, 5, 6, 7)
            "reldur",
            "nkids",
            )
# Fill W1B ----
## relstat ----
f <- f %>%
  group_by(id) %>%
  mutate(
    relstat_orig = relstat,
    relstat_w2 = relstat_orig[wave == 2][1]
  ) %>%
  ungroup()

f <- f %>%
  mutate(
    pstat_clean = case_when(
      pstat %in% c(0, 1) ~ pstat,
      TRUE ~ NA_real_
    ),
    sd3_clean = case_when(
      sd3 %in% c(1, 2) ~ sd3,
      TRUE ~ NA_real_
    ),
    separation_clean = case_when(
      separation %in% c(0, 1) ~ separation,
      TRUE ~ NA_real_
    ),
    
    relstat_filled = case_when(
      # Keep observed raw relstat as is
      !is.na(relstat_orig) ~ relstat_orig,
      
      # Fill wave 3 if wave 2 had a couple status,
      # wave 3 still indicates a partner,
      # and no separation is reported
      is.na(relstat_orig) &
        wave == 3 &
        relstat_w2 %in% c(3, 4, 8, 11) &
        (pstat_clean == 1 | sd3_clean == 1) &
        separation_clean != 1 ~ relstat_w2,
      
      # More liberal carry-forward:
      # partner indicators missing, but no separation reported
      is.na(relstat_orig) &
        wave == 3 &
        relstat_w2 %in% c(3, 4, 8, 11) &
        is.na(pstat_clean) &
        is.na(sd3_clean) &
        separation_clean != 1 ~ relstat_w2,
      
      # Explicit contradictions -> do not fill
      is.na(relstat_orig) &
        wave == 3 &
        (pstat_clean == 0 | sd3_clean == 2) ~ NA_real_,
      
      is.na(relstat_orig) &
        wave == 3 &
        separation_clean == 1 ~ NA_real_,
      
      TRUE ~ NA_real_
    )
  )


## lfstat ----
f <- f %>%
  mutate(
    # Preserve original raw lfstat
    lfstat_orig = lfstat,
    
    # Clean helper variables: keep only valid positive response codes
    sd55_clean = case_when(
      sd55 %in% 1:12 ~ sd55,
      TRUE ~ NA_real_
    ),
    job40_clean = case_when(
      job40 %in% 1:3 ~ job40,
      TRUE ~ NA_real_
    ),
    
    # Reconstruct raw lfstat coding for wave 3 only
    lfstat_w3_rec = case_when(
      wave != 3 ~ NA_real_,
      
      sd55_clean == 1 ~ 1,                 # In Ausbildung
      sd55_clean %in% c(9, 10) ~ 2,        # Mutterschutz / Elternzeit
      sd55_clean == 8 ~ 3,                 # Hausmann/Hausfrau
      sd55_clean == 5 ~ 4,                 # Arbeitslos
      sd55_clean == 7 ~ 5,                 # Freiwilligendienst
      sd55_clean %in% c(6, 11) ~ 6,        # Ruhestand / arbeitsunfähig
      
      sd55_clean == 2 & job40_clean == 1 ~ 7,  # Vollzeit
      sd55_clean == 2 & job40_clean == 2 ~ 8,  # Teilzeit
      sd55_clean == 2 & job40_clean == 3 ~ 9,  # Geringfügig
      
      sd55_clean == 3 ~ 10,                # Selbstständig
      sd55_clean %in% c(4, 12) ~ 11,       # Mithelfendes Familienmitglied / Anderes
      
      TRUE ~ NA_real_
    ),
    
    # Fill missing raw lfstat values with reconstructed wave-3 values
    lfstat_filled = coalesce(lfstat_orig, lfstat_w3_rec)
  )






# Recoding ----
## Sex ----
f <- f %>%
  mutate(
    sex = case_when(
      sex == 2 ~ 0,
      sex == 1 ~ 1,
      TRUE     ~ NA_real_
    ),
    sex = factor(
      sex,
      levels = c(0, 1),
      labels = c("Female", "Male")
    )
  )

## Relationship Status ----
#get_labels(f$relstat, values = "as.name")
f <- f %>%
  mutate(
    relstat2 = case_when(
      relstat_filled  %in% c(3, 8, 11) ~ "Cohabiting",
      relstat_filled  == 4             ~ "Married",
      TRUE                             ~ NA_character_
    ),
    relstat2 = factor(relstat2, 
                      levels = c("Cohabiting", "Married"))
  )

## Labor Force Status ----
f <- f %>%
  mutate(
    lfstat = case_when(
      lfstat_filled  == 7 ~ "Full-time employed",
      lfstat_filled  == 8 ~ "Part-time employed",
      lfstat_filled  == 9 ~ "Marginal employment",
      lfstat_filled  == 10 ~ "Self-employed",
      lfstat_filled  == 2 ~ "Parental leave",
      lfstat_filled  == 6 ~ "Retired",
      lfstat_filled  %in% c(3, 4, 5, 11) ~ "Unemployed",
      TRUE ~ NA_character_
    ),
    lfstat = factor(lfstat,
                    levels = c("Full-time employed",
                               "Part-time employed",
                               "Marginal employment", "Self-employed",
                               "Unemployed", "Parental leave", "Retired"))
    )

## Income ----
### Log ----
#### GCEE ----
f <- f %>%
  mutate(
    hhincgcee = case_when(
      hhincgcee < 0 ~ NA_real_,
      TRUE ~ hhincgcee
    ),
    log_hhincgcee = log1p(hhincgcee)      ## HH-Income (Nettoäquivalenzeinkommen, GCEE)
  )
#### OECD ----
# p <- p %>%
#   mutate(
#     hhincoecd = case_when(
#       hhincoecd < 0 ~ NA_real_,
#       TRUE ~ hhincgcee
#     ),
#     log_hhincoecd = log1p(hhincoecd)      ## HH-Income (Nettoäquivalenzeinkommen, OECD)
#   )





## Benefits ----
### Main ----
f <- f %>%
  mutate(
    wohngeld = dplyr::case_when(
      as.numeric(wohngeld) %in% c(0, 7) ~ 0,
      as.numeric(wohngeld) == 1         ~ 1,
      TRUE                              ~ NA_real_
    ),
    aII = dplyr::case_when(
      as.numeric(aII) %in% c(0, 7) ~ 0,
      as.numeric(aII) == 1         ~ 1,
      TRUE                         ~ NA_real_
    ),
    grundsich = dplyr::case_when(
      as.numeric(grundsich) %in% c(0, 7) ~ 0,
      as.numeric(grundsich) == 1         ~ 1,
      TRUE                               ~ NA_real_
    )
  )

### AGII + Grundischerung ----
f <- f %>%
  mutate(
    benefit_dummy = case_when(
      aII         %in% c(0, 7) &
        grundsich %in% c(0, 7) ~ 0,
      aII == 1 | grundsich == 1 ~ 1,
      TRUE ~ NA_real_
    )
  )



# Sample reduction ----
## Age ----
f <- f %>%
  filter(age >= 15) # Drop samples younger than 15

## Homosexual ----
# f <- f %>%
#   filter(samesex == 0)


# Negative values ----
vars_neg_na <- c(
  "satrelship",
  "reldur", "nkids",
  "age", "wave", "cohort",
  "lifesat"
  )

f <- f %>%
  mutate(
    across(
      all_of(vars_neg_na),
      ~ ifelse(. < 0, NA, .)
    )
  )
table(f$satrelship, f$wave, useNA = "ifany")


# Missings ----
missings <- c(
  "grundsich", "aII", "wohngeld",
  "benefit_dummy",
  "satrelship",
  
  "relstat2",
  #"reldur",
  
  #"age","cohort", "sex",
  #"lifesat",                       # Life satisfaction
  #"nkids",                         # children in HH 
  #"pmrd",                          # Partner lives in household
  #"lfstat", 
  
  "log_hhincgcee"
  #"log_hhincoecd",
  # "hhincnet",
  #"sub_fin_hh"
)

## Remove NAs ----
f_reduc <- f
prop.table(table(complete.cases(f_reduc[missings])))
f_reduc <- f_reduc[complete.cases(f_reduc[missings]), ]
rm(missings, vars_neg_na)