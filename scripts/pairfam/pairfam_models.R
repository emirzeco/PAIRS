# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# R code for                                                                              # 
# Means-Tested Benefits and Relationship Satisfaction among Low-Income Couples in Germany #
# Author: Emir Zecovic                                                                    #
# Last Update: 10.04.2026                                                                 #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # #
# MODELS PAIRFAM  #
# # # # # # # # # # 


# M1 ----
extra_rows <- tibble(
  term = "Controls",
  `Model 1a` = "No",
  `Model 1b` = "Yes"
)
attr(extra_rows, "position") <- "coef_end"
modelsummary::msummary(
  list(
    "Model 1a" = M1a,
    "Model 1b" = M1b
  ),
  title = "Household income and relationship satisfaction, fixed effects analyses",
  output = "M1_pairfam.html",
  coef_map = c(
    "log_hhincgcee"      = "Log Net Equivalized Household Income (GCEE)"
  ),
  coef_omit = "^(?!log_hhincgcee$).*",
  estimate  = "{estimate}{stars}",
  stars = c('*' = .05, '**' = .01, '***' = .001),
  fmt = 3,
  gof_map = c("nobs", "r.squared", "adj.r.squared", "aic", "bic"),
  add_rows = extra_rows,
  notes = c(
    "Notes: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001.",
    "Controls include relationship status, relationship duration, labor force status, partner’s labor force status, health status and wave.
    Source: pairfam (W1 - W13)."
  )
)