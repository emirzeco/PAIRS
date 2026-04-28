# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# R code for                                                                              # 
# Means-Tested Benefits and Parental Self-Efficacy among Low-Income Parents in Germany    #
# Author: Emir Zecovic                                                                    #
# Last Update: 24.04.2026                                                                 #
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
b <- haven::read_dta("C:/Users/Emir  PC/Desktop/PhD/Paper1/Datasets/pairfam_v14-2-0/Data/Stata/biopart.dta")
b_2<- haven::read_dta("C:/Users/Emir  PC/Desktop/PhD/Paper1/Datasets/pairfam_v14-2-0/Data/Stata/bioact.dta")



## Rename ----
new_var_names <- c(sex           = "sex_gen",
                   satrelship    = "sat3",
                   p_satrelship  = "sat4",
                   lifesat       = "sat6",
                   
                   sub_fin_hh    = "inc28",   # Overall, how satisfied are you with your household's financial situation?
                   #depriv_fin_hh = "inc27i2", # HH: Wir müssen häufig verzichten, wegen finanzieller Einschränkungen (W2-W14)
                   #strain_fin_hh = "inc27i3", # HH: Bei uns ist das Geld meistens knapp                              (W2-W14)
                   
                   wohngeld      = "inc10i4",  # Wohngeld oder Lastenzuschuss
                   sozhilfe      = "inc10i7",  # Sozialhilfe
                   aI            = "inc10i8",  # Arbeitslosengeld I (ALG I)
                   aII           = "inc10i9",  # Arbeitslosengeld II einschließlich Sozialgeld
                   grundsich     = "inc10i10", # Grundsicherung im Alter und bei Erwerbsminderung
                   krankgeld     = "inc10i11"  # Krankengeld
)
p <- rename(p,
            all_of(new_var_names))




## Negative values ----
vars_neg_na <- c(
  #"satrelship", "p_satrelship",
  #"reldur",
  "age", "sex", "wave", "cohort",
  "ykage",
  "relstat", "nkidsliv",
  "lfs",
  "plfs",
  "enrol",
  "crn20i4", "crn20i1", "crn20i2",  "crn20i3" 
  )

p <- p %>%
  mutate(
    across(
      all_of(vars_neg_na),
      ~ ifelse(. < 0, NA, .)
    )
  )


# Recoding ----
## Age ----
p <- p %>%
  mutate(
    agegrp = case_when(
      age >= 15 & age <= 20 ~ 0,
      age >= 21 & age <= 25 ~ 1,
      age >= 26 & age <= 30 ~ 2,
      age >= 31 & age <= 35 ~ 3,
      age >= 36 & age <= 40 ~ 4,
      age >= 41 & age <= 45 ~ 5,
      age >= 46 ~ 6,
      TRUE ~ NA_real_
    )
  )

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


## Child Age ----
p <- p %>%
  mutate(
    mkage = ykage,
    ykage = mkage / 12,
    kidagegrp = case_when(
      ykage >= 0 & ykage <= 3 ~ 0,
      ykage > 3 & ykage <= 6 ~ 1,
      ykage > 6 & ykage <= 9 ~ 2,
      ykage > 9 & ykage <= 12 ~ 3,
      ykage > 12 & ykage <= 15 ~ 4,
      ykage > 15 ~ 5,
      TRUE ~ NA_real_
    )
  )
table(p$kidagegrp)


## Relationship Status ----
### With Singles ----
p <- p %>%
  mutate(
    relstat4 = case_when(
      relstat %in% c(1, 6, 9)     ~ "Single",
      relstat %in% c(2, 5, 7, 10) ~ "LAT",
      relstat %in% c(3, 8, 11)    ~ "Cohabiting",
      relstat == 4                ~ "Married",
      TRUE                        ~ NA_character_
    ),
    relstat4 = factor(relstat4, 
                      levels = c("Single", "LAT", "Cohabiting", "Married"))
  )

### Without Singles ----
# p <- p %>%
#   mutate(
#     relstat3 = case_when(
#       relstat %in% c(2, 5, 7, 10) ~ "LAT",
#       relstat %in% c(3, 8, 11)    ~ "Cohabiting",
#       relstat == 4                ~ "Married",
#       TRUE                        ~ NA_character_
#     ),
#     relstat3 = factor(relstat3, 
#                       levels = c("LAT", "Cohabiting", "Married"))
#   )

### Without LAT ----
# p <- p %>%
#   mutate(
#     relstat2 = case_when(
#       relstat %in% c(3, 8, 11) ~ "Cohabiting",
#       relstat == 4             ~ "Married",
#       TRUE                     ~ NA_character_
#     ),
#     relstat2 = factor(relstat2, 
#                       levels = c("Cohabiting", "Married"))
#   )

## Labor Force Status ----
### Anchor ----
p <- p %>%
  mutate(
    lfstat = case_when(
      lfs == 9               ~ "Full-time employed",
      lfs == 10              ~ "Part-time employed",
      lfs %in% c(11, 13)     ~ "Marginal employment",
      lfs == 12              ~ "Self-employed",
      lfs == 4               ~ "Unemployed",
      lfs == 6               ~ "Retired",
      lfs %in% c(2, 3, 5, 7) ~ "Inactive",
      TRUE                ~ NA_character_
    ),
    lfstat = factor(lfstat,
                    levels = c("Full-time employed", "Part-time employed",
                               "Marginal employment", "Self-employed",
                               "Unemployed", "Retired", "Inactive"))
  )
table(p$lfstat, p$wave, useNA = "ifany")

### Partner ----
p <- p %>%
  mutate(
    p_lfstat = case_when(
      plfs == 9              ~ "Full-time employed",
      plfs == 10             ~ "Part-time employed",
      plfs %in% c(11, 13)    ~ "Marginal employment",
      plfs == 12             ~ "Self-employed",
      plfs == 4              ~ "Unemployed",
      plfs == 6              ~ "Retired",
      plfs %in% c(2,3, 5, 7) ~ "Inactive",
      TRUE                   ~ NA_character_
    ),
    p_lfstat = factor(p_lfstat,
                      levels = c("Full-time employed", "Part-time employed",
                                 "Marginal employment", "Self-employed",
                                 "Unemployed", "Retired", "Inactive"))
  )
table(p$p_lfstat, p$wave, useNA = "ifany")

## Income ----
### Log ----
#### GCEE ----
p <- p %>%
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
#       TRUE ~ hhincoecd
#     ),
#     log_hhincoecd = log1p(hhincoecd)
#   )

#### HHinc ----
# p <- p %>%
#   mutate(
#     hhincnet = case_when(
#       hhincnet < 0 ~ NA_real_,
#       TRUE ~ hhincnet
#     ),
#     log_hhincnet = log1p(hhincnet)
#   )


# Povertiy
## Objective povertiy ----
p <- p %>%
  mutate(
    hhincoecd = DescTools::Winsorize(
      hhincoecd,
      val = quantile(
        hhincoecd,
        probs = c(0.001, 0.999),
        na.rm = TRUE
      )
    ),
    de_oecd_median = case_when(
      wave == 4  ~ 19592,
      wave == 5  ~ 19545,
      wave == 6  ~ 19712,
      wave == 7  ~ 20644,
      wave == 8  ~ 21263,
      wave == 9  ~ 21906,
      wave == 10 ~ 22647,
      wave == 11 ~ 23504,
      wave == 12 ~ 25999,
      TRUE ~ NA_real_
    ),
    de_oecd_median = de_oecd_median / 12,
    oecd_mean = NA_real_,
    devoecdmedian = hhincoecd - de_oecd_median,
    oecd60 = 0.60 * de_oecd_median,
    devoecd60_per = (hhincoecd - oecd60) / oecd60 * 100
  ) %>%
  group_by(wave) %>%
  mutate(
    oecd_mean = mean(hhincoecd, na.rm = TRUE),
    sd2devoecd60 = sd(devoecd60_per, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    devsd2devoecd60_per =
      (hhincoecd - sd2devoecd60) / sd2devoecd60 * 100,
    povertySD_cat = case_when(
      devsd2devoecd60_per <= 0 ~ 2,
      devsd2devoecd60_per > 0 & devsd2devoecd60_per <= 100 ~ 1,
      devsd2devoecd60_per > 100 ~ 0,
      TRUE ~ NA_real_
    )
  )




## Subjective povertiy ----
p <- p %>%
  mutate(
    ecodep_own = {
      items <- pick(inc26i2, inc26i3)
      n_valid <- rowSums(!is.na(items))
      
      if_else(
        n_valid == 2,
        rowMeans(items, na.rm = TRUE),
        NA_real_
      )
    },
    ecodep = {
      items <- pick(inc27i2, inc27i3)
      n_valid <- rowSums(!is.na(items))
      
      if_else(
        n_valid == 2,
        rowMeans(items, na.rm = TRUE),
        NA_real_
      )
    },
    ecodep = if_else(
      is.na(ecodep),
      ecodep_own,
      ecodep
    )
  ) %>%
  arrange(id, wave)





## Benefits ----
### Main ----
p <- p %>%
  mutate(
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

### AGII + Soz.Hilfe + Grundischerung ----
p <- p %>%
  mutate(
    benefit_dummy = case_when(
      aII == 0 &
        grundsich == 0 &
        sozhilfe  == 0 ~ 0,
      aII == 1 | grundsich == 1 | sozhilfe == 1 ~ 1,
      TRUE ~ NA_real_
    )
  )


# Parental Self-Efficay ----
## Reverse crn20i4 ----
p <- p %>%
  mutate(
    crn20i4_r = case_when(
      crn20i4 == 1 ~ 5,
      crn20i4 == 2 ~ 4,
      crn20i4 == 3 ~ 3,
      crn20i4 == 4 ~ 2,
      crn20i4 == 5 ~ 1,
      TRUE ~ NA_real_
    )
  )


## Scale ----
p <- p %>%
  mutate(
    help = rowSums(!is.na(pick(
      crn20i1, crn20i2, crn20i3, crn20i4_r
    ))),
    comperz = if_else(
      help >= 2,
      rowMeans(
        pick(crn20i1, crn20i2, crn20i3, crn20i4_r),
        na.rm = TRUE
      ),
      NA_real_
    )
  ) %>%
  group_by(id) %>%
  mutate(
    comperz_mean = (lag(comperz) + dplyr::lead(comperz)) / 2,
    comperz = if_else(
      is.na(comperz),
      comperz_mean,
      comperz
    )
  ) %>%
  ungroup() %>%
  select(-help, -comperz_mean) %>%
  arrange(id, wave)




# Sample reduction ----
## Age ----
# p <- p %>%
#   filter(age >= 15) # Drop samples younger than 15

## Enrolled ----
p <- p %>%
  filter(enrol == 0) # Drop samples that are enrolled

## Num kids ----
p <- p %>%
  filter(
    !is.na(nkids),
    nkids != 0,
    !is.na(nkidsliv)
  )

## Child < 18 ----
p <- p %>%
  filter(
    !is.na(ykage),
    ykage < 18
  )

## Participation > 1 ----
p <- p %>%
  group_by(id) %>%
  mutate(
    pycount_r = n()
  ) %>%
  filter(pycount_r > 1) %>%
  ungroup()

# Wave 1-3
p <- p %>%
  filter(wave > 3) # Keep only starting wave4


# Missings ----
missings <- c(
  #"grundsich", "aII", "sozhilfe",
  "wohngeld",
  "benefit_dummy",
  
  "comperz",

  #"relstat4",

  #"agegrp","cohort", "sex", "wave",
  #"kidagegrp",                        # Number children in HH
  # "ykage", "mkage",                  # Age children
  "lfstat", #"p_lfstat",                # Labor force status (anchor, partner)
  
  "log_hhincgcee"
  #"log_hhincoecd"
  #"log_hhincnet"
  #"sub_fin_hh",
  )

## Remove NAs ----
p_reduc <- p
prop.table(table(complete.cases(p_reduc[missings])))
p_reduc <- p_reduc[complete.cases(p_reduc[missings]), ]
rm(missings, new_var_names, vars_neg_na)