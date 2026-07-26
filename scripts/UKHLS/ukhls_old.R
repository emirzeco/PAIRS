# # # # # # # #
# OLD R code  #
# # # # # # # #



# Benefits ----
## Out-of-work benefits ----
### JSA ----
#alabs(uk$unemp_benefit_JSA)          # Income : Unemployment benefits: Job Seeker's Allowance
#alabs(uk$inc_benefit_JSA)            # Income: Receives core benefits: Job Seeker's Allowance
#alabs(uk$inct_benefit_JSA)           # Income types received: Job Seekers Allowance (Unemployment) and/or Income Support
uk <- uk %>%
  mutate(
    benefit_JSA = case_when(
      unemp_benefit_JSA == 1  | inc_benefit_JSA == 1  | inct_benefit_JSA == 1  
      ~ 1,
      
      unemp_benefit_JSA == 0 | inc_benefit_JSA == 0 | inct_benefit_JSA == 0
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
      bt_benefit_IS == 1 | inc_benefit_IS == 1
      ~ 1,
      
      bt_benefit_IS == 0 | inc_benefit_IS == 0
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
      dis_benefit_ESA == 1 | inct_benefit_ESA == 1
      ~ 1,
      
      dis_benefit_ESA == 0 | inct_benefit_ESA == 0
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
      hou_benefit_HB == 1 | bt_benefit_CTB == 1 | inct_benefit_hous == 1 | oth_benefit_hous == 1
      ~ 1,
      
      hou_benefit_HB == 0 | bt_benefit_CTB == 0 | inct_benefit_hous == 0 | oth_benefit_hous == 0
      ~ 0,
      TRUE ~ NA_real_
    )
  )

## Personal tax credits ----
### CTC ----
#alabs(uk$chi_benefit_CTC)          # Income: Receives Child Tax Credit       (W1-W15)
#alabs(uk$tax_benefit_CTC)          # Income: Tax Credits: Child Tax Credit   (W1-W5)
#alabs(uk$inct_benefit_CTC)         # Income types received: Child Tax Credit (W1-W15)

## !Not used as means-tested for now!
# uk <- uk %>%
#   mutate(
#     benefit_CTC = case_when(
#       chi_benefit_CTC == 1 |
#         tax_benefit_CTC == 1 |
#         inct_benefit_CTC == 1
#       ~ 1,
#       
#       chi_benefit_CTC == 2 |
#         tax_benefit_CTC == 0 |
#         inct_benefit_CTC == 0
#       ~ 0,
#       TRUE ~ NA_real_
#     )
#   )

### WTC ----
#alabs(tax_benefit_WTC)          # Income: Tax Credits: Working Tax Credit, including Disabled Person's Tax Credit    (W1-W5)
#alabs(bt_benefit_WTC)           # Type of benefit or payment: Tax credits, such as the Working Tax Credit or Child   (W1-W5)
#alabs(inct_benefit_WTC)         # Income types received: Working Tax Credit                                          (W1-W15)
#alabs(oth_benefit_WTC)          # Other benefits or credits: Working Tax Credit                                      (W6-W15)
uk <- uk %>%
  mutate(
    benefit_WTC = case_when(
      tax_benefit_WTC == 1 | bt_benefit_WTC == 1 | inct_benefit_WTC == 1 | oth_benefit_WTC == 1
      ~ 1,
      
      tax_benefit_WTC == 0 | bt_benefit_WTC == 0 | inct_benefit_WTC == 0 | oth_benefit_WTC == 0
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
      hou_benefit_CTS == 1  | tax_benefit_CTS == 1  
      ~ 1,
      
      hou_benefit_CTS == 0 | tax_benefit_CTS == 0
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

# IV ----
## OOW Benefits -----
uk <- uk %>%
  mutate(
    benefit_OOW = case_when(
      benefit_JSA == 1 | benefit_IS == 1 | benefit_ESA == 1
      ~ 1,
      
      benefit_JSA == 0 | benefit_IS == 0 | benefit_ESA == 0
      ~ 0,
      TRUE ~ NA_real_
    )
  )

## IW-Benefits ----
uk <- uk %>%
  mutate(
    benefit_IWB = case_when(
      benefit_HB == 1 | benefit_WTC == 1
      ~ 1,
      
      benefit_HB == 0 | benefit_WTC == 0
      ~ 0,
      TRUE ~ NA_real_
    )
  )
