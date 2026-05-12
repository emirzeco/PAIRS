# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# R code for                                                                              # 
# Means-Tested Benefits and Relationship Satisfaction among Low-Income Couples in the UK  #
# Author: Emir Zecovic                                                                    #
# Last Update: 12.05.2026                                                                 #
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
              "ggplot2", "ggrepel", "sjPlot", "lme4", "knitr", "kableExtra", 
              "stringr", "flextable", "officer", "sf", "plm", "stargazer",
              "patchwork", "tidytext", "sjlabelled")
install.packages(setdiff(packages, rownames(installed.packages())))
suppressMessages(lapply(packages, library, character.only = TRUE, quietly=T))
rm(packages)
options(max.print=10000)



## Load ----
uk <- readRDS("C:/Users/Emir  PC/Desktop/PhD/Paper1/PAIRS/data/UKHLS_long.rds")
#uk <- readRDS("~/PAIRS/data/UKHLS_long.rds")

## Rename ----
new_var_names <- c(hhnetinc              = "fihhmnnet1_dv",         # total household net income - no deductions
                   hhincoecd             = "ieqmoecd_dv",           # Modified OECD equivalence scale
                   
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
                   #inct_benefit_WTC      = "pbnft7",               # Income types received: Working Tax Credit                                          (W1-W15)
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
## Agegroup ----
# uk <- uk %>%
#   mutate(
#     agegrp = case_when(
#       age >= 15 & age <= 20 ~ 0,
#       age >= 21 & age <= 25 ~ 1,
#       age >= 26 & age <= 30 ~ 2,
#       age >= 31 & age <= 35 ~ 3,
#       age >= 36 & age <= 40 ~ 4,
#       age >= 41 & age <= 45 ~ 5,
#       age >= 46 & age <= 50 ~ 6,
#       age >= 51 & age <= 55 ~ 7,
#       TRUE ~ NA_real_
#       )
#     )


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
      labels = c("Female", "Male")
    )
  )

## Ethnicity ----
uk <- uk %>%
  mutate(
    ethnicity = case_when(
      racedv == 1
      ~ 1,
      racedv %in% c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 97)
      ~ 2,
      TRUE ~ NA_real_
    ),
    ethnicity = factor(
      ethnicity,
      levels = c(1, 2),
      labels = c("White British", "Other")
    )
  )

## Health ----
uk <- uk %>%
  mutate(
    health = case_when(
      wavename %in% 1:5 & sf1 > 0
      ~ sf1,
      
      wavename >= 6 & scsf1 > 0
      ~ scsf1,
      wavename >= 6 & is.na(scsf1) & sf1 > 0
      ~ sf1,
      
      TRUE ~ NA_real_
    )
  ) # Ignore label warning!

uk <- uk %>%
  mutate(
    health = case_when(
      health %in% c(1, 2) ~ 1,  # Excellent / Very good
      health == 3 ~ 2,          # Good
      health %in% c(4, 5) ~ 3,  # Fair / Poor
      TRUE ~ NA_real_
    ),
    
    health = factor(
      health,
      levels = c(1, 2, 3),
      labels = c("Excellent/Very good", "Good", "Poor/Fair")
      )
    )




## Marital status ----
uk <- uk %>%
  mutate(
    marstat3 = case_when(
      marstat == 1 ~ "Married",
      marstat == 2 ~ "Divorced",
      marstat == 3 ~ "Widowed",
      TRUE         ~ NA_character_ # 1 Single; 3 same sex; 4 separated but legally married; 7 separated; 8; 9 
    ),
    marstat3 = factor(marstat3,
                      levels = c("Married", "Divorced", "Widowed"))
  )
#table(uk$livesp) # Live with spouse 

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
### Log ----
#### OECD ----
uk <- uk %>%
  mutate(
    hhincoecd = case_when(
      hhincoecd < 0 ~ NA_real_,
      TRUE ~ hhincoecd
    ),
    log_hhincoecd = log1p(hhincoecd)  ## HH-Income (Nettoäquivalenzeinkommen, OECD)
  )




# Benefits ----
## Out-of-work benefits ----
### JSA ----
#alabs(uk$unemp_benefit_JSA)          # Income : Unemployment benefits: Job Seeker's Allowance
#alabs(uk$inc_benefit_JSA)            # Income: Receives core benefits: Job Seeker's Allowance
#alabs(uk$inct_benefit_JSA)           # Income types received: Job Seekers Allowance (Unemployment) and/or Income Support
uk <- uk %>%
  mutate(
    benefit_JSA = case_when(
      unemp_benefit_JSA == 1  |
        inc_benefit_JSA == 1  |
        inct_benefit_JSA == 1  
      ~ 1,
      
      unemp_benefit_JSA == 0 |
        inc_benefit_JSA == 0 |
        inct_benefit_JSA == 0
      ~ 0,
      TRUE ~ NA_real_
    )
  )

### IS ----
#alabs(uk$bt_benefit_IS)               # Type of benefit or payment: Income Support
#alabs(uk$inc_benefit_IS)              # Income: Receives core benefits: Income Support
#alabs(uk$inct_benefit_JSA)            # !!! UNUSED !!!: Income types received: Job Seekers Allowance (Unemployment) and/or Income Support
uk <- uk %>%
  mutate(
    benefit_IS = case_when(
      bt_benefit_IS == 1 |
        inc_benefit_IS == 1
      ~ 1,
      
      bt_benefit_IS == 0 |
        inc_benefit_IS == 0
      ~ 0,
      TRUE ~ NA_real_
    )
  )

### ESA ----
#alabs(uk$dis_benefit_ESA)          # Income: Disability benefits: Employment and Support Allowance
#alabs(uk$inct_benefit_ESA)         # Income types received: Employment and Support Allowance
uk <- uk %>%
  mutate(
    benefit_ESA = case_when(
      dis_benefit_ESA == 1 |
        inct_benefit_ESA == 1
      ~ 1,
      
      dis_benefit_ESA == 0 |
        inct_benefit_ESA == 0
      ~ 0,
      TRUE ~ NA_real_
      )
    )

## Housing benefit ----
### HB ----
#alabs(uk$hou_benefit_HB)                # Receives housing-related benefit(s): Housing Benefit                               (W1-W5)
#alabs(uk$bt_benefit_CTB)                # Type of benefit or payment: Housing or Council Tax Benefit (other than the single) (W1-W5)
#alabs(uk$inct_benefit_hous)             # Income types received: Housing Benefit/Rent Rebate                                 (W1-W15)
#alabs(uk$oth_benefit_hous)              # Other benefits or credits: Housing Benefit
uk <- uk %>%
  mutate(
    benefit_HB = case_when(
      hou_benefit_HB == 1 |
        bt_benefit_CTB == 1 |
        inct_benefit_hous == 1 |
        oth_benefit_hous == 1
      ~ 1,
      
      hou_benefit_HB == 0 |
        bt_benefit_CTB == 0 |
        inct_benefit_hous == 0 |
        oth_benefit_hous == 0
      ~ 0,
      TRUE ~ NA_real_
    )
  )

## Personal tax credits ----
### CTC ----
#alabs(uk$chi_benefit_CTC)          # Income: Receives Child Tax Credit       (W1-W15)
#alabs(uk$tax_benefit_CTC)          # Income: Tax Credits: Child Tax Credit   (W1-W5)
#alabs(uk$inct_benefit_CTC)         # Income types received: Child Tax Credit (W1-W15)
uk <- uk %>%
  mutate(
    benefit_CTC = case_when(
      chi_benefit_CTC == 1 |
        tax_benefit_CTC == 1 |
        inct_benefit_CTC == 1
      ~ 1,
      
      chi_benefit_CTC == 2 |
        tax_benefit_CTC == 0 |
        inct_benefit_CTC == 0
      ~ 0,
      TRUE ~ NA_real_
    )
  )

### WTC ----
#alabs(tax_benefit_WTC)          # Income: Tax Credits: Working Tax Credit, including Disabled Person's Tax Credit    (W1-W5)
#alabs(bt_benefit_WTC)           # Type of benefit or payment: Tax credits, such as the Working Tax Credit or Child   (W1-W5)
#alabs(inct_benefit_WTC)         # Income types received: Working Tax Credit                                          (W1-W15)
#alabs(oth_benefit_WTC)          # Other benefits or credits: Working Tax Credit                                      (W6-W15)
uk <- uk %>%
  mutate(
    benefit_WTC = case_when(
      tax_benefit_WTC == 1 |
        bt_benefit_WTC == 1 |
        inct_benefit_WTC == 1 |
        oth_benefit_WTC == 1
      ~ 1,
      
      tax_benefit_WTC == 0 |
        bt_benefit_WTC == 0 |
        inct_benefit_WTC == 0 |
        oth_benefit_WTC == 0
      ~ 0,
      TRUE ~ NA_real_
      )
    )

## Other benefits ----
#### CTS ----
#benhou2   = "hou_benefit_CTS",          # Receives housing-related benefit(s): Council tax benefit (W1-W5)
#bentax2   = "tax_benefit_CTS",          # Income: Tax Credits: Council Tax Benefit                 (W1-W5)
uk <- uk %>%
  mutate(
    benefit_CTS = case_when(
      hou_benefit_CTS == 1  |
        tax_benefit_CTS == 1  
      ~ 1,
      
      hou_benefit_CTS == 0 |
        tax_benefit_CTS == 0
      ~ 0,
      TRUE ~ NA_real_
    )
  )


## Universal credit ----
#### UC ----
#bendis11  = "dis_benefit_UC",        # Income: Disability benefits: Universal Credit
#benhou5   = "hou_benefit_UC",        # Receives housing-related benefit(s): Universal Credit
#bentax6   = "tax_benefit_UC",        # Income: Tax Credits: Universal Credit
#benunemp3 = "unemp_benefit_UC",      # Income : Unemployment benefits: Universal Credit
#btype10   = "bt_benefit_UC",         # Type of benefit or payment: Universal Credit
#benbase4  = "inc_benefit_UC",        # Income: Receives core benefits: Universal Credit
#pbnft13   = "inct_benefit_UC",       # Income types received: Universal Credit 





# # # # # # # # # # # # # # # # # # # # # # #
## Unused ----
### Return to Work Credit ----
#bendis6   = "dis_benefit_return_WC"
#bentax5   = "tax_benefit_return_WC"
#othben4   = "oth_benefit_return_WC"

### Lone Parent In-Work Credit ----
#othben3   = "oth_benefit_IWC_lp"
#benfam4   = "fam_benefit_IWC_lp"

### Other family benefits ----
#btype7    = "bt_benefit_fam"

### Rent rebate ----
#benhou3   = "hou_benefit_rent_r"
#benhou4   = "hou_benefit_rate_r"

### National Insurance Credit ----
#benunemp2 = "unemp_benefit_NIC"

### Child Benefit ----
#btype5    = "bt_benefit_CB"
#benbase3  = "inc_benefit_CB"
#pbnft6    = "inct_benefit_CB"
# # # # # # # # # # # # # # # # # # # # # # #




# Sample reduction ----
## Age ----
uk <- uk %>%
  filter(age_dv >= 15) # Drop samples younger than 15
