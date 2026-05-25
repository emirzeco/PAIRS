# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# R code for                                                                              # 
# Means-Tested Benefits and Relationship Satisfaction among Low-Income Couples in the UK  #
# Author: Emir Zecovic                                                                    #
# Last Update: 19.05.2026                                                                 #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # # #
# RE ANALYSIS UKHLS   #
# # # # # # # # # # # #  



# M10 (HHInc) ----
## M10a ----
M10a <- plm(
  rel_happy ~ log_hhincoecd,
  data = uk,
  index = c("pidp", "wavename"),
  model = "random"
)

## M10b ----
M10b <- plm(
  rel_happy ~ log_hhincoecd +
    age + sex + ppsex + ethnicity + 
    relstat2 + lfstat + nchild_dv + health,
  data = uk,
  index = c("pidp", "wavename"),
  model = "random"
)

## M10c ----
M10c <- plm(
  rel_happy ~ log_hhincoecd +
    relstat2 + lfstat + nchild_dv + health +
    wavename,
  data = uk,
  index = c("pidp", "wavename"),
  model = "random"
)



# M11 (Out-of-work benefits) ----
## M11a ----
M11a <- plm(
  rel_happy ~ benefit_OOW,
  data = uk,
  index = c("pidp", "wavename"),
  model = "random"
)

## M11b ----
M11b <- plm(
  rel_happy ~ benefit_OOW +
    log_hhincoecd +
    age + sex + ppsex + ethnicity + 
    relstat2 + lfstat + nchild_dv + health,
  data = uk,
  index = c("pidp", "wavename"),
  model = "random"
)

## M11c ----
M11c <- plm(
  rel_happy ~ benefit_OOW +
    log_hhincoecd + 
    age + sex + ppsex + ethnicity + 
    relstat2 + lfstat + nchild_dv + health +
    wavename,
  data = uk,
  index = c("pidp", "wavename"),
  model = "random"
)



# M12 (In-work benefits) ----
## M12a ----
M12a <- plm(
  rel_happy ~ benefit_IWB,
  data = uk,
  index = c("pidp", "wavename"),
  model = "random"
)

## M12b ----
M12b <- plm(
  rel_happy ~ benefit_IWB +
    log_hhincoecd,
  data = uk,
  index = c("pidp", "wavename"),
  model = "random"
)

## M12c ----
M12c <- plm(
  rel_happy ~ benefit_IWB +
    log_hhincoecd +
    age + sex + ppsex + ethnicity + 
    relstat2 + lfstat + nchild_dv + health +
    wavename,
  data = uk,
  index = c("pidp", "wavename"),
  model = "random"
)


# M13 (Male) ----
uk_men <- uk %>%
  filter(sex == "Male")
