# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# R code for                                                                              # 
# Means-Tested Benefits and Relationship Satisfaction among Low-Income Couples in UK      #
# Author: Emir Zecovic                                                                    #
# Last Update: 07.05.2026                                                                 #
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
new_var_names <- c(hhnetinc      = "fihhmnnet1_dv",     # total household net income - no deductions
                   hhincoecd     = "ieqmoecd_dv",       # Modified OECD equivalence scale
                   
                   hhbenefit     = "fihhmnprben_dv",    # total household private benefit income: month before interview
                   #a            = "fimnsben_dv",       # amount income component 7: social benefit income
                   fimngrs_dv    = "pinc_g",            # total monthly personal income gross
                   fimnnet_dv    = "pinc_n",            # total net personal income - no deductions
                   fimnlabgrs_dv = "pinc_month_g",      # total monthly labour income gross
                   
                   benctc     = "chi_benefit_CTC",      # Income: Receives Child Tax Credit (W1-W15)
                   #benfam4   = "fam_benefit_IWC_lp",   # Income: Family benefits: In-Work Credit for Lone Parents 
                   
                   bendis2   = "dis_benefit_ESA",       # Income: Disability benefits: Employment and Support Allowance
                   bendis6   = "dis_benefit_return_WC", # Income: Disability benefits: Return to work credit
                   bendis11  = "dis_benefit_UC",        # Income: Disability benefits: Universal Credit
                   
                   benhou1   = "hou_benefit_HB",        # Receives housing-related benefit(s): Housing Benefit
                   benhou2   = "hou_benefit_CTS",       # Receives housing-related benefit(s): Council tax benefit
                   benhou3   = "hou_benefit_rent_r",    # Receives housing-related benefit(s): Rent rebate
                   benhou4   = "hou_benefit_rate_r",    # Receives housing-related benefit(s): Rate rebate
                   benhou5   = "hou_benefit_UC",        # Receives housing-related benefit(s): Universal Credit
                   
                   bentax1   = "tax_benefit_WTC",       # Income: Tax Credits: Working Tax Credit, including Disabled Person's Tax Credit
                   bentax2   = "tax_benefit_CTS",       # Income: Tax Credits: Council Tax Benefit
                   bentax4   = "tax_benefit_CTC",       # Income: Tax Credits: Child Tax Credit
                   bentax5   = "tax_benefit_return_WC", # Income: Tax Credits: Return to Work Credit
                   bentax6   = "tax_benefit_UC",        # Income: Tax Credits: Universal Credit
                   
                   benunemp1 = "unemp_benefit_JSA",     # Income : Unemployment benefits: Job Seeker's Allowance
                   benunemp2 = "unemp_benefit_NIC",     # Income : Unemployment benefits: National Insurance Credits
                   benunemp3 = "unemp_benefit_UC",      # Income : Unemployment benefits: Universal Credit
                   
                   btype1    = "bt_benefit_unemp",      # Type of benefit or payment: Unemployment-related benefits, or National Insurance
                   btype2    = "bt_benefit_IS",         # Type of benefit or payment: Income Support
                   btype3    = "bt_benefit_sick",       # Type of benefit or payment: Sickness, disability or incapacity benefits
                   btype5    = "bt_benefit_CB",         # Type of benefit or payment: Child Benefit
                   btype6    = "bt_benefit_WTC",        # Type of benefit or payment: Tax credits, such as the Working Tax Credit or Child
                   btype7    = "bt_benefit_fam",        # Type of benefit or payment: Any other family related benefit or payment
                   btype8    = "bt_benefit_CTB",        # Type of benefit or payment: Housing or Council Tax Benefit (other than the single)
                   btype10   = "bt_benefit_UC",         # Type of benefit or payment: Universal Credit
                   
                   benbase1  = "inc_benefit_IS",        # Income: Receives core benefits: Income Support
                   benbase2  = "inc_benefit_JSA",       # Income: Receives core benefits: Job Seeker's Allowance
                   benbase3  = "inc_benefit_CB",        # Income: Receives core benefits: Child Benefit
                   benbase4  = "inc_benefit_UC",        # Income: Receives core benefits: Universal Credit
                   
                   pbnft4 = "inct_benefit_JSA",         # Income types received: Job Seekers Allowance (Unemployment) and/or Income Support
                   pbnft5 = "inct_benefit_ESA",         # Income types received: Employment and Support Allowance
                   pbnft6 = "inct_benefit_CB",          # Income types received: Child Benefit
                   pbnft7 = "inct_benefit_WTC",         # Income types received: Working Tax Credit
                   pbnft8 = "inct_benefit_hous",        # Income types received: Housing Benefit/Rent Rebate
                   #pbnft9 = "inct_benefit_IB",         # Income types received: Incapacity Benefit (Replaces Invalidity and NI Sickness)
                   pbnft11 = "inct_benefit_CTC",        # Income types received: Child Tax Credit
                   pbnft13 = "inct_benefit_UC",         # Income types received: Universal Credit
                   
                   othben3 = "oth_benefit_IWC_lp",      # Other benefits or credits: In-Work Credit for Lone Parents
                   othben4 = "oth_benefit_return_WC",   # Other benefits or credits: Return to Work Credit
                   othben5 = "oth_benefit_WTC",         # Other benefits or credits: Working Tax Credit
                   othben8 = "oth_benefit_hous",        # Other benefits or credits: Housing Benefit
                   
                   #frwc = "benefit_n",                 # Period covered by last amount received
                   
                   scdassat_dv = "DAS_rel_sat",         # Dyadic Adjustment Scale: Relationship satisfaction subscale
                   scdascoh_dv = "DAS_rel_coh",         # Dyadic Adjustment Scale: Relationship cohesion subscale
                   screlhappy = "rel_happy"             # Degree of happiness with relationship
                   )
uk <- rename(uk,
            all_of(new_var_names))

## Factor transf ----
uk$country <- factor(
  uk$country,
  levels = c(1, 2, 3, 4),
  labels = c("England", "Wales", "Scotland", "Northern Ireland")
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


## Marital status ----
uk <- uk %>%
  mutate(
    marstat4 = case_when(
      marstat == 1 ~ "Single",
      marstat == 2 ~ "Married",
      marstat == 3 ~ "Divorced",
      marstat == 4 ~ "Widowed",
      TRUE         ~ NA_character_ # 1 Single; 3 same sex; 4 separated but legally married; 7 separated; 8; 9 
    ),
    marstat4 = factor(marstat4,
                      levels = c("Cohabiting", "Married"))
  )

table(uk$livesp)

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
                    levels = c("Full/Part-time employed",
                               "Self-employed",
                               "Unemployed", "Retired", "Inactive"))
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
      TRUE ~ hhincgcee
    ),
    log_hhincoecd = log1p(hhincoecd)      ## HH-Income (Nettoäquivalenzeinkommen, OECD)
  )

## Benefits ----
### JSA ----
#benunemp1 = "unemp_benefit_JSA",     # Income : Unemployment benefits: Job Seeker's Allowance
#benbase2  = "inc_benefit_JSA",       # Income: Receives core benefits: Job Seeker's Allowance
#pbnft4 = "inct_benefit_JSA",         # Income types received: Job Seekers Allowance (Unemployment) and/or Income Support

### IS ----
#btype2    = "bt_benefit_IS",         # Type of benefit or payment: Income Support
#benbase1  = "inc_benefit_IS",        # Income: Receives core benefits: Income Support
#pbnft4 = "inct_benefit_JSA",         # Income types received: Job Seekers Allowance (Unemployment) and/or Income Support

### ESA ----
#bendis2   = "dis_benefit_ESA",       # Income: Disability benefits: Employment and Support Allowance
#pbnft5 = "inct_benefit_ESA",         # Income types received: Employment and Support Allowance

### Housing benefit ----
#benhou1   = "hou_benefit_HB",        # Receives housing-related benefit(s): Housing Benefit
#btype8    = "bt_benefit_CTB",        # Type of benefit or payment: Housing or Council Tax Benefit (other than the single)
#pbnft8 = "inct_benefit_hous",        # Income types received: Housing Benefit/Rent Rebate
#othben8 = "oth_benefit_hous",        # Other benefits or credits: Housing Benefit

### CTC ----
#benctc     = "chi_benefit_CTC",      # Income: Receives Child Tax Credit (W1-W15)
#bentax4   = "tax_benefit_CTC",       # Income: Tax Credits: Child Tax Credit
#pbnft11 = "inct_benefit_CTC",        # Income types received: Child Tax Credit

### WTC ----
#bentax1   = "tax_benefit_WTC",       # Income: Tax Credits: Working Tax Credit, including Disabled Person's Tax Credit
#btype6    = "bt_benefit_WTC",        # Type of benefit or payment: Tax credits, such as the Working Tax Credit or Child
#pbnft7 = "inct_benefit_WTC",         # Income types received: Working Tax Credit
#othben5 = "oth_benefit_WTC",         # Other benefits or credits: Working Tax Credit

### CTS ----
#benhou2   = "hou_benefit_CTS",       # Receives housing-related benefit(s): Council tax benefit
#bentax2   = "tax_benefit_CTS",       # Income: Tax Credits: Council Tax Benefit


# # # # # # # # # # # # # # # # # # # # # # # 
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
# # # # # # # # # # # # # # # # # # # # # # # #

### Child Benefit ----
#btype5    = "bt_benefit_CB"
#benbase3  = "inc_benefit_CB"
#pbnft6    = "inct_benefit_CB"


### UC ----
#bendis11  = "dis_benefit_UC",        # Income: Disability benefits: Universal Credit
#benhou5   = "hou_benefit_UC",        # Receives housing-related benefit(s): Universal Credit
#bentax6   = "tax_benefit_UC",        # Income: Tax Credits: Universal Credit
#benunemp3 = "unemp_benefit_UC",      # Income : Unemployment benefits: Universal Credit
#btype10   = "bt_benefit_UC",         # Type of benefit or payment: Universal Credit
#benbase4  = "inc_benefit_UC",        # Income: Receives core benefits: Universal Credit
#pbnft13 = "inct_benefit_UC",         # Income types received: Universal Credit 







# Sample reduction ----
## Age ----
uk <- uk %>%
  filter(age_dv >= 15) # Drop samples younger than 15
