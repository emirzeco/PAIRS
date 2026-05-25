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
  rel_happy ~ log_hhincoecd,
  data = uk,
  index = c("pidp", "wavename"),
  model = "within"
)

## M1b ----
M1b <- plm(
  rel_happy ~ log_hhincoecd +
    relstat2 + lfstat + nchild_dv + health,
  data = uk,
  index = c("pidp", "wavename"),
  model = "within"
)

## M1c ----
M1c <- plm(
  rel_happy ~ log_hhincoecd +
    relstat2 + lfstat + nchild_dv + health +
    wavename,
  data = uk,
  index = c("pidp", "wavename"),
  model = "within"
)



# M2 (Out-of-work benefits) ----
## M2a ----
M2a <- plm(
  rel_happy ~ benefit_OOW,
  data = uk,
  index = c("pidp", "wavename"),
  model = "within"
)

## M2b ----
M2b <- plm(
  rel_happy ~ benefit_OOW +
    log_hhincoecd,
  data = uk,
  index = c("pidp", "wavename"),
  model = "within"
)

## M2c ----
M2c <- plm(
  rel_happy ~ benefit_OOW +
    log_hhincoecd + 
    relstat2 + lfstat + nchild_dv + health +
    wavename,
  data = uk,
  index = c("pidp", "wavename"),
  model = "within"
)



# M3 (In-work benefits) ----
## M3a ----
M3a <- plm(
  rel_happy ~ benefit_IWB,
  data = uk,
  index = c("pidp", "wavename"),
  model = "within"
)

## M3b ----
M3b <- plm(
  rel_happy ~ benefit_IWB +
    log_hhincoecd,
  data = uk,
  index = c("pidp", "wavename"),
  model = "within"
)

## M3c ----
M3c <- plm(
  rel_happy ~ benefit_IWB +
    log_hhincoecd + 
    relstat2 + lfstat + nchild_dv + health +
    wavename,
  data = uk,
  index = c("pidp", "wavename"),
  model = "within"
)


# M4 (Male) ----
uk_men <- uk %>%
  filter(sex == "Male")

## M4a ----
M4a <- plm(
  rel_happy ~ benefit_OOW +
    log_hhincoecd + 
    relstat2 + lfstat + nchild_dv + health +
    wavename,
  data = uk_men,
  index = c("pidp", "wavename"),
  model = "within"
)

## M4b ----
M4b <- plm(
  rel_happy ~ benefit_IWB +
    log_hhincoecd + 
    relstat2 + lfstat + nchild_dv + health +
    wavename,
  data = uk_men,
  index = c("pidp", "wavename"),
  model = "within"
)



# M5 (Woman) ----
uk_women <- uk %>%
  filter(sex == "Female")

## M5a ----
M5a <- plm(
  rel_happy ~ benefit_OOW +
    log_hhincoecd + 
    relstat2 + lfstat + nchild_dv + health +
    wavename,
  data = uk_women,
  index = c("pidp", "wavename"),
  model = "within"
)

## M5b ----
M5b <- plm(
  rel_happy ~ benefit_OOW +
    log_hhincoecd + 
    relstat2 + lfstat + nchild_dv + health +
    wavename,
  data = uk_women,
  index = c("pidp", "wavename"),
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