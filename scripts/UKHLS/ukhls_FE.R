# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# R code for                                                                              # 
# Means-Tested Benefits and Relationship Satisfaction among Low-Income Couples in the UK  #
# Author: Emir Zecovic                                                                    #
# Last Update: 19.05.2026                                                                 #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # # #
# FE ANALYSIS UKHLS   #
# # # # # # # # # # # #  



# M1 (HHInc) ----
## M1a ----
M1a <- plm(
  rel_happy ~ log_hhnetinc_oecd,
  data = uk,
  index = "pidp",
  model = "within"
)

## M1b ----
M1b <- plm(
  rel_happy ~ hhnetinc +
    relstat2 + lfstat + nchild_dv + mcs,
  data = uk,
  index = "pidp",
  model = "within"
)

## M1c ----
M1c <- plm(
  rel_happy ~ hhnetinc +
    relstat2 + lfstat + nchild_dv + mcs +
    wavename,
  data = uk,
  index = "pidp",
  model = "within"
)



# M2 (Means-tested) ----
## M2a ----
M2a <- plm(
  rel_happy ~ benefit_MT,
  data = uk,
  index = "pidp",
  model = "within"
)

## M2b ----
M2b <- plm(
  rel_happy ~ benefit_MT +
    log_hhgrsinc_oecd,
  data = uk,
  index = "pidp",
  model = "within"
)

## M2c ----
M2c <- plm(
  rel_happy ~ benefit_MT +
    hhnetinc + 
    relstat2 + lfstat + nchild_dv + mcs +
    wavename,
  data = uk,
  index = "pidp",
  model = "within"
)






# M4 (Male) ----
uk_men <- uk %>%
  filter(sex == "Male")

## M4a ----
M4a <- plm(
  rel_happy ~ benefit_OOW +
    hhnetinc + 
    relstat2 + lfstat + nchild_dv + mcs +
    wavename,
  data = uk_men,
  index = "pidp",
  model = "within"
)

## M4b ----
M4b <- plm(
  rel_happy ~ benefit_IWB +
    hhnetinc + 
    relstat2 + lfstat + nchild_dv + mcs +
    wavename,
  data = uk_men,
  index = "pidp",
  model = "within"
)



# M5 (Woman) ----
uk_women <- uk %>%
  filter(sex == "Female")

## M5a ----
M5a <- plm(
  rel_happy ~ benefit_OOW +
    log_hhincoecd + 
    relstat2 + lfstat + nchild_dv + mcs +
    wavename,
  data = uk_women,
  index = "pidp",
  model = "within"
)

## M5b ----
M5b <- plm(
  rel_happy ~ benefit_OOW +
    log_hhincoecd + 
    relstat2 + lfstat + nchild_dv + mcs +
    wavename,
  data = uk_women,
  index = "pidp",
  model = "within"
)






















# In-Work sample ----
uk_emp <- uk %>%
  filter(
    lfstat %in% c(
      "Full-time employed",
      "Part-time employed",
      "Self-employed"
    )
  )