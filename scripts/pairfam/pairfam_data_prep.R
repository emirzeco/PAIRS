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
p <- p %>%
  mutate(
    lfstat = case_when(
      lfs == 2            ~ "Parental leave",
      lfs == 6            ~ "Retired",
      lfs %in% c(3, 4, 7) ~ "Unemployed",
      lfs == 9            ~ "Full-time employed",
      lfs == 10           ~ "Part-time employed",
      lfs == 8            ~ "Vocational training"
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


## Income ----









# Sample reduction ----
## Age ----
# < 15 = NA

## enrol ----
alabs(p$enrol)
# Drop all except 0 and 11 (vocational training (berufl. Ausbildung))






# Missings ----
## FE Missings ----
p_fe <- p

missings <- c(
"grundsich", "aII", "sozhilfe", "wohngeld",
"sat3",                               # Relationship satisfaction

"relstat",
"reldur",

"age", "wave",
"lifesat",     # Life satisfaction
"nkidsliv",    # children in HH 
"pmrd",        # Partner lives in household
"lfs", "plfs", # Labor force status (anchor, partner)

"hhincgcee",
"hhincnet",
"sub_fin_hh",

"pcs",         # Summary score physical health
"mcs",         # Summary score mental health
)














## satrelship ----
p$satrelship <- ifelse(p$satrelship %in% 0:10, as.numeric(p$satrelship), NA)

## Benefits ----
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

## Lifestat and finance_sat ----
p <- p %>%
  dplyr::mutate(
    lifestat   = ifelse(as.numeric(lifestat)   %in% 0:10, as.numeric(lifestat), NA),
    sub_fin_hh = ifelse(as.numeric(sub_fin_hh) %in% 0:10, as.numeric(sub_fin_hh), NA)
  )



## Income ----
p$hhinc <- ifelse(p$hhinc >= 0, as.numeric(p$hhinc), NA)
p$log_hhinc <- log1p(p$hhinc)

### Quartiles ----
qcuts <- quantile(p$hhinc, probs = c(.25, .50, .75), na.rm = TRUE)

p$inc_quartile <- with(p, dplyr::case_when(
  is.na(hhinc) ~ NA_character_,
  hhinc <= qcuts[1] ~ "Q1 (lowest)",
  hhinc <= qcuts[2] ~ "Q2",
  hhinc <= qcuts[3] ~ "Q3",
  hhinc >  qcuts[3] ~ "Q4 (highest)"
))

p$inc_quartile <- factor(
  p$inc_quartile,
  levels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")
)

table(p$inc_quartile, useNA = "ifany")

### Low-income ----
p <- p %>%
  dplyr::mutate(
    low_income_q1 = dplyr::case_when(
      is.na(inc_quartile)                 ~ NA_real_,
      inc_quartile == "Q1 (lowest)"       ~ 1,
      TRUE                                ~ 0
    )
  )

table(p$low_income_q1, useNA = "ifany")


table(p$wohngeld, useNA = "ifany")
table(p$aII, useNA = "ifany")
table(p$grundsich, useNA = "ifany")
table(p$satrelship, useNA = "ifany")
table(p$log_hhinc, useNA = "ifany")




## Welfare (any) ----
p <- p %>%
  dplyr::mutate(
    welfare_any = ifelse(
      is.na(sozhilfe) & is.na(grundsich),
      NA,
      ifelse(sozhilfe == 1 | grundsich == 1, 1, 0)
    )
  )
table(p$welfare_any, useNA = "ifany")


# Clean ----
vars_main <- c(
  "satrelship",
  "grundsich", "sozhilfe", "aII", "wohngeld",
  "sub_fin_hh", "lifestat", "log_hhinc", "inc_quartile", "low_income_q1"
)

p_reduc <- p


## Remove NAs ----
prop.table(table(complete.cases(p_reduc[vars_main])))
p_reduc <- p_reduc[complete.cases(p_reduc[vars_main]), ]
#rm(vars_main)