# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# R code for                                                                              # 
# Means-Tested Benefits and Relationship Satisfaction among Low-Income Couples in the UK  #
# Author: Emir Zecovic                                                                    #
# Last Update: 19.05.2026                                                                 #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # # #
# FE ANALYSIS UKHLS   #
# # # # # # # # # # # #  



# M1 (Log_HH_Inc_OECD) ----
## M1a ----
M1a <- plm(
  rel_happy ~ log_hhnetinc_oecd,
  data = uk,
  index = "pidp",
  model = "within"
)


## M1b ----
M1b <- plm(
  rel_happy ~ log_hhgrsinc_oecd,
  data = uk,
  index = "pidp",
  model = "within"
)


## M1c ----
M1c <- plm(
  rel_happy ~ log_hhnetinc_oecd +
    relstat2 + relevel(lfstat, ref = "Retired") + nchild + mcs + pcs + 
    factor(wave),
  data = uk,
  index = "pidp",
  model = "within"
)

## M1d ----
M1d <- plm(
  rel_happy ~ log_hhgrsinc_oecd +
    relstat2 + relevel(lfstat, ref = "Retired") + nchild + mcs + pcs + 
    factor(wave),
  data = uk,
  index = "pidp",
  model = "within"
)



stargazer(list(M1a, M1b, M1c, M1d),
          
          column.labels=c("Equivalence household net income (log) (no controls)", "Equivalence household gross income (log) (no controls)",
                          "Equivalence household net income (log) (controls)", "Equivalence household gross income (log) (controls)"
                          ),
          
          covariate.labels=c("Equivalence household net income (log)",
                             "Equivalence household gross income (log)",
                             "Married (Ref: Cohabiting)",
                             "Full-time employed (Ref. Retired)",
                             "Part-time employed",
                             "Self-employed",
                             "Unemployed",
                             "Inactive",
                             "Number of children (0-10)",
                             "Mental health (0-100)",
                             "Physical health (0-100)"
          ),
          
          align=TRUE,
          type = "text")








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
    log_hhnetinc_oecd,
  data = uk,
  index = "pidp",
  model = "within"
)

## M2c ----
M2c <- plm(
  rel_happy ~ benefit_MT +
    log_hhnetinc_oecd + 
    relstat2 + relevel(lfstat, ref = "Retired") + nchild,
  data = uk,
  index = "pidp",
  model = "within"
)

## M2d ----
M2d <- plm(
  rel_happy ~ benefit_MT +
    log_hhnetinc_oecd + 
    relstat2 + relevel(lfstat, ref = "Retired") + nchild + 
    factor(wave),
  data = uk,
  index = "pidp",
  model = "within"
)

## M2e ----
M2e <- plm(
  rel_happy ~ benefit_MT +
    log_hhgrsinc_oecd + 
    relstat2 + relevel(lfstat, ref = "Retired") + nchild + 
    factor(wave),
  data = uk,
  index = "pidp",
  model = "within"
)

stargazer(list(M2a, M2b, M2c, M2d, M2e),
          
          column.labels=c("M2a", "M2b", "M2c", "M2d", "M2e"),
          
          covariate.labels=c("Means-tested benefit receipt (0/1)",
                            "Equivalence household net income (log)",
                            "Equivalence household gross income (log)",
                            "Married (Ref: Cohabiting)",
                            "Full-time employed (Ref. Retired)",
                            "Part-time employed",
                            "Self-employed",
                            "Unemployed",
                            "Inactive",
                            "Number of children (0-10)"
                             ),
          
          align=TRUE,
          type = "text")






# M4 (Male) ----
uk_men <- uk %>%
  filter(sex == "Male")

## M4a ----
M4a <- plm(
  rel_happy ~ benefit_MT +
    log_hhnetinc_oecd + 
    factor(wave),
  data = uk_men,
  index = "pidp",
  model = "within"
)

## M4b  (all controls) ----
M4b <- plm(
  rel_happy ~ benefit_MT +
    log_hhnetinc_oecd + 
    relstat2 + lfstat + nchild + 
    factor(wave),
  data = uk_men,
  index = "pidp",
  model = "within"
)



# M5 (Woman) ----
uk_women <- uk %>%
  filter(sex == "Female")

## M5a ----
M5a <- plm(
  rel_happy ~ benefit_MT +
    log_hhnetinc_oecd + 
    factor(wave),
  data = uk_women,
  index = "pidp",
  model = "within"
)

## M5b (all controls) ----
M5b <- plm(
  rel_happy ~ benefit_MT +
    log_hhnetinc_oecd + 
    relstat2 + lfstat + nchild + 
    factor(wave),
  data = uk_women,
  index = "pidp",
  model = "within"
)









# In/Out-Work sample ----
uk_emp <- uk %>%
  filter(
    lfstat %in% c(
      "Full-time employed",
      "Part-time employed",
      "Self-employed"
    )
  )


uk_unemp <- uk %>%
  filter(
    lfstat %in% c(
      "Unemployed",
      "Retired",
      "Inactive"
    )
  )



## M100 ----
M100 <- plm(
  rel_happy ~ benefit_MT +
    log_hhgrsinc_oecd + 
    relstat2 + lfstat + nchild + factor(wave),
  data = uk_emp,
  index = "pidp",
  model = "within"
)


## M200 ----
M200 <- plm(
  rel_happy ~ benefit_MT +
    log_hhgrsinc_oecd + 
    relstat2 + lfstat + nchild + factor(wave),
  data = uk_unemp,
  index = "pidp",
  model = "within"
)

stargazer(list(M100, M200),
          type = "text")
