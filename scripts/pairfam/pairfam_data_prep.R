# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# R code for                                                                              # 
# Means-Tested Benefits and Relationship Satisfaction among Low-Income Couples in Germany #
# Author: Emir Zecovic                                                                    #
# Last Update: 07.04.2026                                                                 #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # # # # # #
# DATA PREPERATION PAIRFAM  #
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
p <-haven::read_dta("data/pairfam_long.dta")

## Rename ----
new_var_names <- c(sex           = "sex_gen",
                   satrelship    = "sat3",
                   P_satrelship  = "sat4",
                   lifesat       = "sat6",
                   
                   sub_fin_hh    = "inc28",
                   depriv_fin_hh = "inc27i2", # HH: Wir müssen häufig verzichten, wegen finanzieller Einschränkungen (W2-W14)
                   strain_fin_hh = "inc27i3", # HH: Bei uns ist das Geld meistens knapp                              (W2-W14)
                   
                   wohngeld      = "inc10i4",  # Wohngeld oder Lastenzuschuss
                   sozhilfe      = "inc10i7",  # Sozialhilfe
                   aI            = "inc10i8",  # Arbeitslosengeld I (ALG I)
                   aII           = "inc10i9",  # Arbeitslosengeld II einschließlich Sozialgeld
                   grundsich     = "inc10i10", # Grundsicherung im Alter und bei Erwerbsminderung
                   krankgeld     = "inc10i11"  # Krankengeld
                   )
p <- rename(p,
            all_of(new_var_names))


# Recoding ----
## Relationship Status ----
p <- p %>%
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
#table(p$relstat3, p$wave, useNA = "ifany")

## Labor Force Status ----
### Anchor ----
p <- p %>%
  mutate(
    lfstat = case_when(
      lfs == 2            ~ "Parental leave",
      lfs == 6            ~ "Retired",
      lfs %in% c(3, 4, 7) ~ "Unemployed",
      lfs == 9            ~ "Full-time employed",
      lfs == 10           ~ "Part-time employed",
      lfs == 8            ~ "Vocational training",
      lfs %in% c(11, 13)  ~ "Marginal employment",
      lfs == 12           ~ "Self-employed",
      TRUE                ~ NA_character_
    ),
    lfstat = factor(lfstat,
                    levels = c("Parental leave", "Retired", "Unemployed",
                               "Full-time employed", "Part-time employed",
                               "Vocational training", "Marginal employment", "Self-employed"))
  )
#table(p$lfstat, p$wave, useNA = "ifany")

### Partner ----
p <- p %>%
  mutate(
    p_lfstat = case_when(
      plfs == 2            ~ "Parental leave",
      plfs == 6            ~ "Retired",
      plfs %in% c(3, 4, 7) ~ "Unemployed",
      plfs == 9            ~ "Full-time employed",
      plfs == 10           ~ "Part-time employed",
      plfs == 8            ~ "Vocational training",
      plfs %in% c(11, 13)  ~ "Marginal employment",
      plfs == 12           ~ "Self-employed",
      TRUE                ~ NA_character_
    ),
    p_lfstat = factor(p_lfstat,
                    levels = c("Parental leave", "Retired", "Unemployed",
                               "Full-time employed", "Part-time employed",
                               "Vocational training", "Marginal employment", "Self-employed"))
  )
table(p$p_lfstat, p$wave, useNA = "ifany")

## Income ----
### Log ----



## Benefits ----
### Main ----
p <- p %>%
  dplyr::mutate(
    wohngeld = dplyr::case_when(
      as.numeric(wohngeld) %in% c(0, 7) ~ 0,
      as.numeric(wohngeld) == 1         ~ 1,
      TRUE                              ~ NA_real_
    ),
    sozhilfe = dplyr::case_when(
      as.numeric(sozhilfe) %in% c(0, 7) ~ 0,
      as.numeric(sozhilfe) == 1         ~ 1,
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

### AII + Soz.Hilfe ----
p <- p %>%
  mutate(
    aII_sozhilfe = case_when(
      aII %in% c(0, 7) & sozhilfe %in% c(0, 7) ~ 0,
      aII == 1 & sozhilfe == 1 ~ 2,
      aII == 1 & sozhilfe %in% c(0, 7) ~ 1,
      aII %in% c(0, 7) & sozhilfe == 1 ~ 1,
      TRUE ~ NA_real_
    )
)

### AGII + Soz.Hilfe + Grundischerung
p <- p %>%
  mutate(
    benefit_combined_3 = case_when(
      aII %in% c(0, 7) & grundsich %in% c(0, 7) & sozhilfe %in% c(0, 7) ~ 0,
      (aII == 1) + (grundsich == 1) + (sozhilfe == 1) == 1 ~ 1,
      (aII == 1) + (grundsich == 1) + (sozhilfe == 1) == 2 ~ 2,
      (aII == 1) + (grundsich == 1) + (sozhilfe == 1) == 3 ~ 3,
      TRUE ~ NA_real_
    )
  )

## Negative values ----
vars_neg_na <- c(
  "satrelship", "P_satrelship",
  "reldur",
  "age", "wave", "cohort",
  "lifesat",
  "nkidsliv",
  "hhincgcee", "hhincoecd", "hhincnet",
  "sub_fin_hh",
  "pcs",
  "mcs"
)

p <- p %>%
  mutate(
    across(
      all_of(vars_neg_na),
      ~ ifelse(. < 0, NA, .)
    )
  )
table(p$satrelship, useNA = "ifany")



# Sample reduction ----
## Age ----
p <- p %>%
  filter(age >= 15) # Drop all samples younger than 15

## enrol ----
p <- p %>%
  filter(enrol %in% c(0, 11)) # Drop all samples that are enrolled
                              # except 0 and 11 (not enrolled & vocational training)

## Drop Wave 14
p <- p %>%
  filter(wave != 14)


# Missings ----
## FE Missings ----
p_fe <- p

missings <- c(
"grundsich", "aII", "sozhilfe", "wohngeld",
"aII_sozhilfe", "benefit_combined_3",
"satrelship", "P_satrelship",                                      # Relationship satisfaction

"relstat3",
"reldur",

"age", "wave", "cohort",
"lifesat",       # Life satisfaction
"nkidsliv",      # children in HH 
#"pmrd",         # Partner lives in household
"lfs", "plfs",   # Labor force status (anchor, partner)

"hhincgcee",
"hhincnet",
"sub_fin_hh",

"pcs",          # Summary score physical health
"mcs",          # Summary score mental health
)


## Clean ----
# vars_main <- c(
#   "satrelship",
#   "grundsich", "sozhilfe", "aII", "wohngeld",
#   "sub_fin_hh", "lifesat", "log_hhinc", "inc_quartile", "low_income_q1"
# )
# p_reduc <- p


## Remove NAs ----
# prop.table(table(complete.cases(p_reduc[vars_main])))
# p_reduc <- p_reduc[complete.cases(p_reduc[vars_main]), ]
#rm(vars_main)























































## Income ----
# p$hhinc <- ifelse(p$hhinc >= 0, as.numeric(p$hhinc), NA)
# p$log_hhinc <- log1p(p$hhinc)
# 
# ### Quartiles ----
# qcuts <- quantile(p$hhinc, probs = c(.25, .50, .75), na.rm = TRUE)
# 
# p$inc_quartile <- with(p, dplyr::case_when(
#   is.na(hhinc) ~ NA_character_,
#   hhinc <= qcuts[1] ~ "Q1 (lowest)",
#   hhinc <= qcuts[2] ~ "Q2",
#   hhinc <= qcuts[3] ~ "Q3",
#   hhinc >  qcuts[3] ~ "Q4 (highest)"
# ))
# 
# p$inc_quartile <- factor(
#   p$inc_quartile,
#   levels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")
# )
# 
# table(p$inc_quartile, useNA = "ifany")
# 
# ### Low-income ----
# p <- p %>%
#   dplyr::mutate(
#     low_income_q1 = dplyr::case_when(
#       is.na(inc_quartile)                 ~ NA_real_,
#       inc_quartile == "Q1 (lowest)"       ~ 1,
#       TRUE                                ~ 0
#     )
#   )
# 
# table(p$low_income_q1, useNA = "ifany")
