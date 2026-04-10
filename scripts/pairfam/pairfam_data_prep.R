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
#p <-haven::read_dta("/posit_share/home/zecovic-e/PAIRS/data/pairfam_long.dta")

## Rename ----
new_var_names <- c(sex           = "sex_gen",
                   satrelship    = "sat3",
                   p_satrelship  = "sat4",
                   lifesat       = "sat6",
                   
                   sub_fin_hh    = "inc28",   # Overall, how satisfied are you with your household's financial situation?
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
## Sex ----
p <- p %>%
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
p <- p %>%
  mutate(
    relstat2 = case_when(
      relstat %in% c(3, 8, 11) ~ "Cohabiting",
      relstat == 4             ~ "Married",
      TRUE                     ~ NA_character_
    ),
    relstat2 = factor(relstat2, 
                      levels = c("Cohabiting", "Married"))
  )
#table(p$relstat2, p$wave, useNA = "ifany")

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
      lfs %in% c(11, 13)  ~ "Marginal employment",
      lfs == 12           ~ "Self-employed",
      TRUE                ~ NA_character_
    ),
    lfstat = factor(lfstat,
                    levels = c("Parental leave", "Retired", "Unemployed",
                               "Full-time employed", "Part-time employed",
                               "Marginal employment", "Self-employed"))
  )
table(p$lfstat, p$wave, useNA = "ifany")

### Partner ----
p <- p %>%
  mutate(
    p_lfstat = case_when(
      plfs == 2            ~ "Parental leave",
      plfs == 6            ~ "Retired",
      plfs %in% c(3, 4, 7) ~ "Unemployed",
      plfs == 9            ~ "Full-time employed",
      plfs == 10           ~ "Part-time employed",
      plfs %in% c(11, 13)  ~ "Marginal employment",
      plfs == 12           ~ "Self-employed",
      TRUE                ~ NA_character_
    ),
    p_lfstat = factor(p_lfstat,
                    levels = c("Parental leave", "Retired", "Unemployed",
                               "Full-time employed", "Part-time employed",
                               "Marginal employment", "Self-employed"))
  )
table(p$p_lfstat, p$wave, useNA = "ifany")

## Income ----
### Log ----
p <- p %>%
  mutate(
    hhincgcee = case_when(
      hhincgcee < 0 ~ NA_real_,
      TRUE ~ hhincgcee
    ),
    log_hhincgcee = log1p(hhincgcee)      ## HH-Income (Nettoäquivalenzeinkommen, GCEE)
  ) 



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

### AGII + Soz.Hilfe + Grundischerung
p <- p %>%
  mutate(
    benefit_dummy = case_when(
      aII         %in% c(0, 7) &
        grundsich %in% c(0, 7) &
        sozhilfe  %in% c(0, 7) ~ 0,
      aII == 1 | grundsich == 1 | sozhilfe == 1 ~ 1,
      TRUE ~ NA_real_
    )
  )

## Negative values ----
vars_neg_na <- c(
  "satrelship", "p_satrelship",
  "reldur",
  "age", "wave", "cohort",
  "lifesat",
  "nkidsliv",
  "sub_fin_hh",
  "hlt1"
)

p <- p %>%
  mutate(
    across(
      all_of(vars_neg_na),
      ~ ifelse(. < 0, NA, .)
    )
  )
table(p$satrelship, p$wave, useNA = "ifany")



# Sample reduction ----
## Age ----
p <- p %>%
  filter(age >= 15) # Drop samples younger than 15

## Homosexual ----
p <- p %>%
  filter(homosex == 0)

## Enrolled ----
p <- p %>%
  filter(enrol == 0) # Drop samples that are enrolled

## Wave 14 ----
p <- p %>%
  filter(wave != 14) # Relstat not available in W14


# Missings ----
## FE Missings ----
missings <- c(
"grundsich", "aII", "sozhilfe", "wohngeld",
"benefit_dummy",
"satrelship", "p_satrelship",   # Relationship satisfaction

"relstat2",
"reldur",

"age","cohort", "sex",
"lifesat",                      # Life satisfaction
"nkidsliv",                     # children in HH 
#"pmrd",                        # Partner lives in household
"lfstat", "p_lfstat",           # Labor force status (anchor, partner)

"log_hhincgcee",
# "hhincoecd",
# "hhincnet",
"sub_fin_hh",

"hlt1"

# "pcs",          # Summary score physical health
# "mcs"           # Summary score mental health
)

## Remove NAs ----
p_reduc <- p
prop.table(table(complete.cases(p_reduc[missings])))
p_reduc <- p_reduc[complete.cases(p_reduc[missings]), ]
rm(missings, new_var_names, vars_neg_na)
















































## Income
# p$hhinc <- ifelse(p$hhinc >= 0, as.numeric(p$hhinc), NA)
# p$log_hhinc <- log1p(p$hhinc)
# 
# ### Quartiles
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
# ### Low-income
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
