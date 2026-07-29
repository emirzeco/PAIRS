# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# R code for                                                                              # 
# Means-Tested Benefits and Relationship Satisfaction among Low-Income Couples in the UK  #
# Author: Emir Zecovic                                                                    #
# Last Update: 01.08.2026                                                                 #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # #
# DATA PREP UKHLS   #
# # # # # # # # # # #  

# Setup ----
## Packages ----
# if ("convenience" %in% rownames(installed.packages()) ==F) {
#   devtools::install_github("ratsupaltuf/convenience", force=T)
# }

alabs<- function(x, kable=TRUE, extended=T){
  tb<- tibble(labels=names(attributes(x)$labels), values=attributes(x)$labels)

  if(extended==T) {
    lab<- attributes(x)$label
    cl<- class(x)
    u<- unique(x)

    if(kable==T) {

      cat(lab, "\n",
          "Class:",cl, "\n",
          "Unique values:", u, "\n",
          "\n"
      )
      print(knitr::kable(tb, caption="Labels"))

    }  else {
      cat(lab, "\n",
          "Class:",cl, "\n",
          "Unique values:", u, "\n",
          "\n"
      )
      print(tb)
    }
  } else if (kable==T) {

    kable(tb)
  }  else {
    tb
  }
}

packages <- c("tidyverse", "haven", "pastecs", "datawizard", #"convenience",
              "ggplot2", "ggalluvial", "ggthemes", "viridis", "ggrepel",
              "sjPlot", "lme4", "knitr", "kableExtra", 
              "stringr", "flextable", "officer", "sf", "plm", "stargazer",
              "patchwork", "tidytext", "sjlabelled")
install.packages(setdiff(packages, rownames(installed.packages())))
suppressMessages(lapply(packages, library, character.only = TRUE, quietly=T))
rm(packages)
options(max.print=10000)



## Load ----
uk <- readRDS("data/UKHLS_long.rds")
#uk <- readRDS("~/PAIRS/data/UKHLS_long.rds")

## Rename ----
new_var_names <- c(wave                  = "wavename",              # Wave
                   
                   hhnetinc              = "fihhmnnet1_dv",         # total household net income - no deductions
                   hhgrsinc              = "fihhmngrs_dv",          # gross household income: month before interview
                   
                   oecdscale             = "ieqmoecd_dv",           # Modified OECD equivalence scale
                   
                   hhbenefitinc_lag      = "fihhmnprben_dv",        # total household private benefit income: month before interview
                   hhbenefitinc          = "fimnsben_dv",           # amount income component 7: social benefit income
                   pinc_g                = "fimngrs_dv",            # total monthly personal income gross
                   pinc_n                = "fimnnet_dv",            # total net personal income - no deductions
                   pinc_lab_month_g      = "fimnlabgrs_dv",         # total monthly labor income gross
                   
                   chi_benefit_CTC       = "benctc",                # Income: Receives Child Tax Credit                                                  (W1-W15)
                   
                   #fam_benefit_IWC_lp   = "benfam4",               # Income: Family benefits: In-Work Credit for Lone Parents                           (W1-W5)
                   
                   dis_benefit_ESA       = "bendis2",               # Income: Disability benefits: Employment and Support Allowance                      (W1-W15)
                   dis_benefit_return_WC = "bendis6",               # Income: Disability benefits: Return to work credit                                 (W1-W15)
                   dis_benefit_UC        = "bendis11",              # Income: Disability benefits: Universal Credit                                      (W1-W15)
                   
                   hou_benefit_HB        = "benhou1",               # Receives housing-related benefit(s): Housing Benefit                               (W1-W5)
                   hou_benefit_CTS       = "benhou2",               # Receives housing-related benefit(s): Council tax benefit                           (W1-W5)
                   hou_benefit_rent      = "benhou3",               # Receives housing-related benefit(s): Rent rebate                                   (W1-W5)
                   hou_benefit_rate_r    = "benhou4",               # Receives housing-related benefit(s): Rate rebate                                   (W1-W5)
                   hou_benefit_UC        = "benhou5",               # Receives housing-related benefit(s): Universal Credit                              (W1-W5)
                   
                   tax_benefit_WTC       = "bentax1",               # Income: Tax Credits: Working Tax Credit, including Disabled Person's Tax Credit    (W1-W5)
                   tax_benefit_CTS       = "bentax2",               # Income: Tax Credits: Council Tax Benefit                                           (W1-W5)
                   tax_benefit_CTC       = "bentax4",               # Income: Tax Credits: Child Tax Credit                                              (W1-W5)
                   tax_benefit_return_WC = "bentax5",               # Income: Tax Credits: Return to Work Credit                                         (W1-W5)
                   tax_benefit_UC        = "bentax6",               # Income: Tax Credits: Universal Credit                                              (W1-W5)
                   
                   unemp_benefit_JSA     = "benunemp1",             # Income : Unemployment benefits: Job Seeker's Allowance                             (W1-W5)
                   unemp_benefit_NIC     = "benunemp2",             # Income : Unemployment benefits: National Insurance Credits                         (W1-W5)
                   unemp_benefit_UC      = "benunemp3",             # Income : Unemployment benefits: Universal Credit                                   (W1-W5)
                   
                   bt_benefit_unemp      = "btype1",                # Type of benefit or payment: Unemployment-related benefits, or National Insurance   (W1-W5)
                   bt_benefit_IS         = "btype2",                # Type of benefit or payment: Income Support                                         (W1-W5)
                   bt_benefit_sick       = "btype3",                # Type of benefit or payment: Sickness, disability or incapacity benefits            (W1-W5)
                   bt_benefit_CB         = "btype5",                # Type of benefit or payment: Child Benefit                                          (W1-W5)
                   bt_benefit_WTC        = "btype6",                # Type of benefit or payment: Tax credits, such as the Working Tax Credit or Child   (W1-W5)
                   bt_benefit_fam        = "btype7",                # Type of benefit or payment: Any other family related benefit or payment            (W1-W5)
                   bt_benefit_CTB        = "btype8",                # Type of benefit or payment: Housing or Council Tax Benefit (other than the single) (W1-W5)
                   bt_benefit_UC         = "btype10",               # Type of benefit or payment: Universal Credit                                       (W1-W5)
                   
                   inc_benefit_IS        = "benbase1",              # Income: Receives core benefits: Income Support                                     (W6-W15)
                   inc_benefit_JSA       = "benbase2",              # Income: Receives core benefits: Job Seeker's Allowance                             (W6-W15)
                   inc_benefit_CB        = "benbase3",              # Income: Receives core benefits: Child Benefit                                      (W6-W15)
                   inc_benefit_UC        = "benbase4",              # Income: Receives core benefits: Universal Credit                                   (W6-W15)
                   
                   inct_benefit_JSA      = "pbnft4",                # Income types received: Job Seekers Allowance (Unemployment) and/or Income Support  (W1-W15)
                   inct_benefit_ESA      = "pbnft5",                # Income types received: Employment and Support Allowance                            (W1-W15)
                   inct_benefit_CB       = "pbnft6",                # Income types received: Child Benefit                                               (W1-W15)
                   inct_benefit_WTC      = "pbnft7",                # Income types received: Working Tax Credit                                          (W1-W15)
                   inct_benefit_hous     = "pbnft8",                # Income types received: Housing Benefit/Rent Rebate                                 (W1-W15)
                   #inct_benefit_IB      = "pbnft9",                # Income types received: Incapacity Benefit (Replaces Invalidity and NI Sickness)    (W1-W15)
                   inct_benefit_CTC      = "pbnft11",               # Income types received: Child Tax Credit                                            (W1-W15)
                   inct_benefit_UC       = "pbnft13",               # Income types received: Universal Credit                                            (W1-W15)
                   
                   oth_benefit_IWC_lp    = "othben3",               # Other benefits or credits: In-Work Credit for Lone Parents                         (W6-W15)
                   oth_benefit_return_WC = "othben4",               # Other benefits or credits: Return to Work Credit                                   (W6-W15)
                   oth_benefit_WTC       = "othben5",               # Other benefits or credits: Working Tax Credit                                      (W6-W15)
                   oth_benefit_hous      = "othben8",               # Other benefits or credits: Housing Benefit                                         (W6-W15)
                   
                   #benefit_n = "frwc",                             # Period covered by last amount received (INCOME DATA)
                   
                   DAS_rel_sat           = "scdassat_dv",           # Dyadic Adjustment Scale: Relationship satisfaction subscale (W1, W3, W5, W7, W9, W11, W13, W15)
                   DAS_rel_coh           = "scdascoh_dv",           # Dyadic Adjustment Scale: Relationship cohesion subscale     (W1, W3, W5, W7, W9, W11, W13, W15)
                   rel_happy             = "screlhappy"             # Degree of happiness with relationship                       (W1, W3, W5, W7, W9, W11, W13, W15)
                   )
uk <- rename(uk,
            all_of(new_var_names))

## Factor transf ----
uk$country <- factor(
  uk$country,
  levels = c(1, 2, 3, 4),
  labels = c("England", "Wales", "Scotland", "Northern Ireland")
  )

uk$gor_dv <- factor(
  uk$gor_dv,
  levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
  labels = c("North East", "North West", "Yorkshire and the Humber", "East Midlands",
             "West Midlands", "East of England", "London", "South East", "South West",
             "Wales", "Scotland", "Northern Ireland")
  )

# Recoding ----
## Age ----
uk <- uk %>%
  mutate(
    age = case_when(
      wave  == 5 & agdv >= 0 ~ agdv,
      wave  != 5 & age_dv >= 0 ~ age_dv,
      TRUE ~ NA_real_
    )
  )
## Age center ----
uk <- mutate(uk,
              age_center = age - mean(age, na.rm = T))


## Sex ----
uk <- uk %>%
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

## PPSex ----
uk <- uk %>%
  mutate(
    ppsex = case_when(
      ppsex == 2 ~ 0,
      ppsex == 1 ~ 1,
      TRUE     ~ NA_real_
    ),
    ppsex = factor(
      ppsex,
      levels = c(0, 1),
      labels = c("Female (partner)", "Male (partner)")
    )
  )

## Ethnicity ----
# uk <- uk %>%
#   mutate(
#     ethnicity_raw = case_when(
#       ethn_dv > 0 ~ ethn_dv,
#       racel_dv > 0 ~ racel_dv,
#       TRUE ~ NA_real_
#     )
#   ) %>%
#   group_by(pidp) %>%
#   tidyr::fill(ethnicity_raw, .direction = "downup") %>%
#   ungroup()
# 
# uk <- uk %>%
#   mutate(
#     ethnicity = case_when(
#       ethnicity_raw == 1 ~ 1,
#       ethnicity_raw %in% c(
#         2, 3, 4, 5, 6, 7, 8, 9, 10,
#         11, 12, 13, 14, 15, 16, 17, 97
#       ) ~ 2,
#       TRUE ~ NA_real_
#     ),
#     ethnicity = factor(
#       ethnicity,
#       levels = c(1, 2),
#       labels = c("White British", "Other")
#     )
#   )



## Health ----
### Physical Health ----
uk <- uk %>%
  mutate(
    pcs = case_when(
      sf12pcs_dv < 0 ~ NA_real_,
      TRUE ~ sf12pcs_dv
    )
  )


### Mental Health ----
uk <- uk %>%
  mutate(
    mcs = case_when(
      sf12mcs_dv < 0 ~ NA_real_,
      TRUE ~ sf12mcs_dv
    )
  )


## Relationship Status ----
uk <- uk %>%
  mutate(
    relstat2 = case_when(
      livewith == 1 ~ 2,
      marstat == 2 ~ 1,
      TRUE ~ NA_real_
    ),
    relstat2 = factor(
      relstat2,
      levels = c(1, 2),
      labels = c("Cohabiting", "Married")
    )
  )
#table(uk$livesp) # Live with spouse

## Rel Happy ----
uk <- uk %>%
  mutate(
    rel_happy = case_when(
      rel_happy < 0 ~ NA_real_,
      TRUE ~ rel_happy
    )
  )



## Labor Force Status ----
### Anchor ----
uk <- uk %>%
  mutate(
    lfstat = case_when(
      jbstat == 2 & jbft_dv == 1      ~ "Full-time employed",
      jbstat == 2 & jbft_dv == 2      ~ "Part-time employed",
      jbstat == 1                     ~ "Self-employed",
      jbstat == 3                     ~ "Unemployed",
      jbstat %in% c(4,8)              ~ "Retired",
      jbstat %in%c (5, 6, 10, 14, 15) ~ "Inactive",
      TRUE                ~ NA_character_
      ),
    lfstat = factor(lfstat,
                    levels = c("Full-time employed", "Part-time employed",
                               "Self-employed", "Unemployed", "Retired", "Inactive"))
    )

### Partner ----
uk <- uk %>%
  mutate(
    p_lfstat = case_when(
      ncrr6 == 2                     ~ "Full/Part-time employed",
      ncrr6 == 1                     ~ "Self-employed",
      ncrr6 == 3                     ~ "Unemployed",
      ncrr6 %in% c(4,8)              ~ "Retired",
      ncrr6 %in%c (5, 6, 10, 14, 15) ~ "Inactive",
      TRUE                ~ NA_character_
      ),
    p_lfstat = factor(p_lfstat,
                      levels = c("Full/Part-time employed",
                                 "Self-employed",
                                 "Unemployed", "Retired", "Inactive"))
    )


## Income ----
uk <- uk %>%
  mutate(
    hhnetinc = case_when(
      hhnetinc >= 0 ~ as.numeric(hhnetinc),
      hhnetinc < 0 ~ 0
    ),
    
    hhgrsinc = case_when(
      hhgrsinc >= 0 ~ as.numeric(hhgrsinc),
      hhgrsinc < 0 ~ 0
    ),
    
    oecdscale = case_when(
      oecdscale > 0 ~ as.numeric(oecdscale),
      TRUE ~ NA_real_
    )
  )

### OECD ----
uk <- uk %>%
  mutate(
    hhnetinc_oecd = hhnetinc / oecdscale,
    hhgrsinc_oecd = hhgrsinc / oecdscale
  )

### Log ----
uk <- uk %>%
  mutate(
    log_hhnetinc_oecd = log(hhnetinc_oecd + 1), 
    log_hhgrsinc_oecd = log(hhgrsinc_oecd + 1)
  )


# Benefits ----
## Means-tested ----
uk <- uk %>%
  mutate(
    benefit_MT = case_when(
      if_any(
        c(
          # JSA
          inc_benefit_JSA,
          unemp_benefit_JSA,
          inct_benefit_JSA,
          bt_benefit_unemp,
          
          # Income Support
          bt_benefit_IS,
          inc_benefit_IS,
          
          # Housing Benefit
          oth_benefit_hous,
          inct_benefit_hous,
          hou_benefit_HB,
          
          # CTC
          chi_benefit_CTC,
          tax_benefit_CTS,
          
          # Working Tax Credit
          bt_benefit_WTC,
          oth_benefit_WTC,
          tax_benefit_WTC,
          inct_benefit_WTC,
          oth_benefit_IWC_lp,
          
          # Universal Credit
          unemp_benefit_UC,
          inct_benefit_UC,
          bt_benefit_UC,
          inc_benefit_UC,
          dis_benefit_UC,
          hou_benefit_UC,
          tax_benefit_UC
        ),
        ~ .x == 1
      ) ~ 1,
      
      if_any(
        c(
          inc_benefit_JSA,
          unemp_benefit_JSA,
          inct_benefit_JSA,
          bt_benefit_unemp,
          bt_benefit_IS,
          inc_benefit_IS,
          oth_benefit_hous,
          inct_benefit_hous,
          hou_benefit_HB,
          chi_benefit_CTC,
          tax_benefit_CTS,
          bt_benefit_WTC,
          oth_benefit_WTC,
          tax_benefit_WTC,
          inct_benefit_WTC,
          oth_benefit_IWC_lp,
          unemp_benefit_UC,
          inct_benefit_UC,
          bt_benefit_UC,
          inc_benefit_UC,
          dis_benefit_UC,
          hou_benefit_UC,
          tax_benefit_UC
        ),
        ~ .x == 0
      ) ~ 0,
      
      TRUE ~ NA_real_
    )
  )



# Sample reduction AGE ----
uk <- uk %>%
  filter(age >= 15) # Drop samples younger than 15


## Missings ----
# missings <- c(
#   "rel_happy",
#   "benefit_OOW", "benefit_IWB",
#   "log_hhincoecd",
# 
#   "relstat2",
#   "nchild_dv",
#   "lfstat",
# 
#   "health",
#   "age", "wave "
#   )
# 
## Remove NAs ----
# prop.table(table(complete.cases(uk_FE[missings])))
# uk_FE <- uk_FE[complete.cases(uk_FE[missings]), ]
# rm(new_var_names, missings)


# Save ----
saveRDS(uk, "C:/Users/Emir  PC/Desktop/PhD/Paper1/PAIRS/data/uk.rds")
