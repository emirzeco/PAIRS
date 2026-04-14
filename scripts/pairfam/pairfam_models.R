# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# R code for                                                                              # 
# Means-Tested Benefits and Relationship Satisfaction among Low-Income Couples in Germany #
# Author: Emir Zecovic                                                                    #
# Last Update: 13.04.2026                                                                 #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # #
# MODELS PAIRFAM  #
# # # # # # # # # # 

# FE ----
## M1 ----
extra_rows <- tibble(
  term = "Wave control",
  `Model 1a` = "No",
  `Model 1b` = "No",
  `Model 1c` = "No",
  `Model 1d` = "Yes",
  `Model 1e` = "Yes"
)
attr(extra_rows, "position") <- "coef_end"


modelsummary::msummary(
  list(
    "Model 1a" = M1a,
    "Model 1b" = M1b,
    "Model 1c" = M1c,
    "Model 1d" = M1d,
    "Model 1e" = M1e
  ),
  
  title = "Household income and relationship satisfaction, fixed effects analyses",
  output = "M1_pairfam.html",
  
  coef_map = c(
    "log_hhincgcee"               = "Log Net Equivalized Household Income (GCEE)",
    
    "relstat2Married"             = "Married (Ref: Cohabiting)",
    
    "lfstatRetired"               = "Retired (Ref: Parental leave)",
    "lfstatUnemployed"            = "Unemployed",
    "lfstatFull-time employed"    = "Full-time employed",
    "lfstatPart-time employed"    = "Part-time employed",
    "lfstatMarginal employment"   = "Marginal employment",
    "lfstatSelf-employed"         = "Self-employed",
    
    "p_lfstatRetired"             = "Retired (partner) (Ref: Parental leave)",
    "p_lfstatUnemployed"          = "Unemployed (partner)",
    "p_lfstatFull-time employed"  = "Full-time employed (partner)",
    "p_lfstatPart-time employed"  = "Part-time employed (partner)",
    "p_lfstatMarginal employment" = "Marginal employment (partner)",
    "p_lfstatSelf-employed"       = "Self-employed (partner)",
    
    "nkidsliv"                    = "Number of kids (0-10)",
    "hlt1"                        = "Health status (1-5)",
    
    "reldur"                      = "Relationship duration (months) (0-522)"
    ),
  
  add_rows = extra_rows,
  estimate  = "{estimate}{stars}",
  stars = c( "#"  = 0.1, '*' = .05, '**' = .01, '***' = .001),
  fmt = 3,
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  notes = c(
    "Notes: # p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001. Standard errors in parentheses.",
    "Controls include relationship status, labor force status, partner’s labor force status, number of kids, health status, relationship duration and wave",
    "Source: pairfam (W1 - W13)."
  )
)







## M2 ----
extra_rows <- tibble(
  term = "Wave control",
  `Model 2a` = "No",
  `Model 2b` = "No",
  `Model 2c` = "No",
  `Model 2d` = "Yes"
  )
attr(extra_rows, "position") <- "coef_end"

modelsummary::msummary(
  list(
    "Model 2a" = M2a,
    "Model 2b" = M2b,
    "Model 2c" = M2c,
    "Model 2d" = M2d
  ),
  
  title = "Grundsicherng and relationship satisfaction, fixed effects analyses",
  output = "M2_pairfam.html",
  
  coef_map = c(
    "benefit_dummy"               = "Grundsicherung (Grundsicherung, Sozialhilfe, AII) (0-1)",
    
    "log_hhincgcee"               = "Log Net Equivalized Household Income (GCEE)",
    "lifesat"                     = "Life satisfaction (0-10)",
    
    "relstat2Married"             = "Married (Ref: Cohabiting)",
    
    "lfstatRetired"               = "Retired (Ref: Parental leave)",
    "lfstatUnemployed"            = "Unemployed",
    "lfstatFull-time employed"    = "Full-time employed",
    "lfstatPart-time employed"    = "Part-time employed",
    "lfstatMarginal employment"   = "Marginal employment",
    "lfstatSelf-employed"         = "Self-employed",
    
    "p_lfstatRetired"             = "Retired (partner) (Ref: Parental leave)",
    "p_lfstatUnemployed"          = "Unemployed (partner)",
    "p_lfstatFull-time employed"  = "Full-time employed (partner)",
    "p_lfstatPart-time employed"  = "Part-time employed (partner)",
    "p_lfstatMarginal employment" = "Marginal employment (partner)",
    "p_lfstatSelf-employed"       = "Self-employed (partner)",
    
    "nkidsliv"                    = "Number of kids (0-10)",
    "hlt1"                        = "Health status (1-5)",
    
    "reldur"                      = "Relationship duration (months) (0-522)"
  ),
  
  add_rows = extra_rows,
  estimate  = "{estimate}{stars}",
  stars = c( "#"  = 0.1, '*' = .05, '**' = .01, '***' = .001),
  fmt = 3,
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  notes = c(
    "Notes: # p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001. Standard errors in parentheses.",
    "Grundsicherung indicates receipt of at least one means-tested benefit (Arbeitslosengeld II/Grundsicherung/Sozialhilfe)",
    "Controls include life satisfaction, relationship status, labor force status, partner’s labor force status, number of kids, health status, relationship duration and wave",
    "Source: pairfam (W1 - W13)."
  )
)




## M3 ----
extra_rows <- tibble(
  term = "Wave control",
  `Model 3a` = "No",
  `Model 3b` = "No",
  `Model 3c` = "No",
  `Model 3d` = "Yes"
)
attr(extra_rows, "position") <- "coef_end"

modelsummary::msummary(
  list(
    "Model 3a" = M3a,
    "Model 3b" = M3b,
    "Model 3c" = M3c,
    "Model 3d" = M3d
  ),
  
  title = "Wohngeld and relationship satisfaction, fixed effects analyses",
  output = "M3_pairfam.html",
  
  coef_map = c(
    "wohngeld"                    = "Wohngeld (0-1)",
    
    "log_hhincgcee"               = "Log Net Equivalized Household Income (GCEE)",
    "lifesat"                     = "Life satisfaction (0-10)",
    
    "relstat2Married"             = "Married (Ref: Cohabiting)",
    
    "lfstatRetired"               = "Retired (Ref: Parental leave)",
    "lfstatUnemployed"            = "Unemployed",
    "lfstatFull-time employed"    = "Full-time employed",
    "lfstatPart-time employed"    = "Part-time employed",
    "lfstatMarginal employment"   = "Marginal employment",
    "lfstatSelf-employed"         = "Self-employed",
    
    "p_lfstatRetired"             = "Retired (partner) (Ref: Parental leave)",
    "p_lfstatUnemployed"          = "Unemployed (partner)",
    "p_lfstatFull-time employed"  = "Full-time employed (partner)",
    "p_lfstatPart-time employed"  = "Part-time employed (partner)",
    "p_lfstatMarginal employment" = "Marginal employment (partner)",
    "p_lfstatSelf-employed"       = "Self-employed (partner)",
    
    "nkidsliv"                    = "Number of kids (0-10)",
    "hlt1"                        = "Health status (1-5)",
    
    "reldur"                      = "Relationship duration (months) (0-522)"
  ),
  
  add_rows = extra_rows,
  estimate  = "{estimate}{stars}",
  stars = c( "#"  = 0.1, '*' = .05, '**' = .01, '***' = .001),
  fmt = 3,
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  notes = c(
    "Notes: # p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001. Standard errors in parentheses.",
    "Controls include life satisfaction, relationship status, labor force status, partner’s labor force status, number of kids, health status, relationship duration and wave",
    "Source: pairfam (W1 - W13)."
  )
)




## In-Work ----
### M4 ----
extra_rows <- tibble(
  term = "Wave control",
  `Model 4a` = "No",
  `Model 4b` = "No",
  `Model 4c` = "Yes"
)
attr(extra_rows, "position") <- "coef_end"


modelsummary::msummary(
  list(
    "Model 4a" = M4a,
    "Model 4b" = M4b,
    "Model 4c" = M4c
  ),
  
  title = "Household income and relationship satisfaction, fixed effects analyses [In-Work sample]",
  output = "M4_pairfam.html",
  
  coef_map = c(
    "log_hhincgcee"               = "Log Net Equivalized Household Income (GCEE)",
    
    "relstat2Married"             = "Married (Ref: Cohabiting)",
    
    "lfstatPart-time employed"    = "Part-time employed",
    "lfstatMarginal employment"   = "Marginal employment",
    "lfstatSelf-employed"         = "Self-employed",
    
    "p_lfstatRetired"             = "Retired (partner) (Ref: Parental leave)",
    "p_lfstatUnemployed"          = "Unemployed (partner)",
    "p_lfstatFull-time employed"  = "Full-time employed (partner)",
    "p_lfstatPart-time employed"  = "Part-time employed (partner)",
    "p_lfstatMarginal employment" = "Marginal employment (partner)",
    "p_lfstatSelf-employed"       = "Self-employed (partner)",
    
    "nkidsliv"                    = "Number of kids (0-10)",
    "hlt1"                        = "Health status (1-5)",
    
    "reldur"                      = "Relationship duration (months) (0-522)"
  ),
  
  add_rows = extra_rows,
  estimate  = "{estimate}{stars}",
  stars = c( "#"  = 0.1, '*' = .05, '**' = .01, '***' = .001),
  fmt = 3,
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  notes = c(
    "Notes: # p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001. Standard errors in parentheses.",
    "In-Work anchor only sample",
    "Controls include relationship status, labor force status, partner’s labor force status, number of kids, health status, relationship duration and wave",
    "Source: pairfam (W1 - W13)."
  )
)






### M5 ----
extra_rows <- tibble(
  term = "Wave control",
  `Model 5a` = "No",
  `Model 5b` = "No",
  `Model 5c` = "No",
  `Model 5d` = "Yes"
)
attr(extra_rows, "position") <- "coef_end"

modelsummary::msummary(
  list(
    "Model 5a" = M5a,
    "Model 5b" = M5b,
    "Model 5c" = M5c,
    "Model 5d" = M5d
  ),
  
  title = "Grundsicherng and relationship satisfaction, fixed effects analyses [In-Work sample]",
  output = "M5_pairfam.html",
  
  coef_map = c(
    "benefit_dummy"               = "Grundsicherung (Grundsicherung, Sozialhilfe, AII) (0-1)",
    
    "log_hhincgcee"               = "Log Net Equivalized Household Income (GCEE)",
    "lifesat"                     = "Life satisfaction (0-10)",
    
    "relstat2Married"             = "Married (Ref: Cohabiting)",

    "lfstatPart-time employed"    = "Part-time employed",
    "lfstatMarginal employment"   = "Marginal employment",
    "lfstatSelf-employed"         = "Self-employed",
    
    "p_lfstatRetired"             = "Retired (partner) (Ref: Parental leave)",
    "p_lfstatUnemployed"          = "Unemployed (partner)",
    "p_lfstatFull-time employed"  = "Full-time employed (partner)",
    "p_lfstatPart-time employed"  = "Part-time employed (partner)",
    "p_lfstatMarginal employment" = "Marginal employment (partner)",
    "p_lfstatSelf-employed"       = "Self-employed (partner)",
    
    "nkidsliv"                    = "Number of kids (0-10)",
    "hlt1"                        = "Health status (1-5)",
    
    "reldur"                      = "Relationship duration (months) (0-522)"
  ),
  
  add_rows = extra_rows,
  estimate  = "{estimate}{stars}",
  stars = c( "#"  = 0.1, '*' = .05, '**' = .01, '***' = .001),
  fmt = 3,
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  notes = c(
    "Notes: # p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001. Standard errors in parentheses.",
    "In-Work anchor only sample",
    "Grundsicherung indicates receipt of at least one means-tested benefit (Arbeitslosengeld II/Grundsicherung/Sozialhilfe)",
    "Controls include life satisfaction, relationship status, labor force status, partner’s labor force status, number of kids, health status, relationship duration and wave",
    "Source: pairfam (W1 - W13)."
  )
)






### M6 ----
extra_rows <- tibble(
  term = "Wave control",
  `Model 6a` = "No",
  `Model 6b` = "No",
  `Model 6c` = "No",
  `Model 6d` = "Yes"
)
attr(extra_rows, "position") <- "coef_end"

modelsummary::msummary(
  list(
    "Model 6a" = M6a,
    "Model 6b" = M6b,
    "Model 6c" = M6c,
    "Model 6d" = M6d
  ),
  
  title = "Wohngeld and relationship satisfaction, fixed effects analyses [In-Work sample]",
  output = "M6_pairfam.html",
  
  coef_map = c(
    "wohngeld"                    = "Wohngeld (0-1)",
    
    "log_hhincgcee"               = "Log Net Equivalized Household Income (GCEE)",
    "lifesat"                     = "Life satisfaction (0-10)",
    
    "relstat2Married"             = "Married (Ref: Cohabiting)",

    "lfstatPart-time employed"    = "Part-time employed",
    "lfstatMarginal employment"   = "Marginal employment",
    "lfstatSelf-employed"         = "Self-employed",
    
    "p_lfstatRetired"             = "Retired (partner) (Ref: Parental leave)",
    "p_lfstatUnemployed"          = "Unemployed (partner)",
    "p_lfstatFull-time employed"  = "Full-time employed (partner)",
    "p_lfstatPart-time employed"  = "Part-time employed (partner)",
    "p_lfstatMarginal employment" = "Marginal employment (partner)",
    "p_lfstatSelf-employed"       = "Self-employed (partner)",
    
    "nkidsliv"                    = "Number of kids (0-10)",
    "hlt1"                        = "Health status (1-5)",
    
    "reldur"                      = "Relationship duration (months) (0-522)"
  ),
  
  add_rows = extra_rows,
  estimate  = "{estimate}{stars}",
  stars = c( "#"  = 0.1, '*' = .05, '**' = .01, '***' = .001),
  fmt = 3,
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  notes = c(
    "Notes: # p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001. Standard errors in parentheses.",
    "In-Work anchor only sample",
    "Controls include life satisfaction, relationship status, labor force status, partner’s labor force status, number of kids, health status, relationship duration and wave",
    "Source: pairfam (W1 - W13)."
  )
)


## Men ----
### M10-M13 ----
extra_rows <- tibble(
  term = "Wave control",
  `Model 10` = "Yes",
  `Model 11` = "Yes",
  `Model 12` = "Yes",
)
attr(extra_rows, "position") <- "coef_end"

modelsummary::msummary(
  list(
    "Model 10" = M10,
    "Model 11" = M11,
    "Model 12" = M12
  ),
  
  title = "Combined model relationship satisfaction, fixed effects analyses [Men only sample]",
  output = "M10-M12_pairfam.html",
  
  coef_map = c(
    "log_hhincgcee"               = "Log Net Equivalized Household Income (GCEE)",
    "benefit_dummy"               = "Grundsicherung (Grundsicherung, Sozialhilfe, AII) (0-1)",
    "wohngeld"                    = "Wohngeld (0-1)",
    
    "lifesat"                     = "Life satisfaction (0-10)",
    
    "relstat2Married"             = "Married (Ref: Cohabiting)",
    
    "lfstatRetired"               = "Retired (Ref: Parental leave)",
    "lfstatUnemployed"            = "Unemployed",
    "lfstatFull-time employed"    = "Full-time employed",
    "lfstatPart-time employed"    = "Part-time employed",
    "lfstatMarginal employment"   = "Marginal employment",
    "lfstatSelf-employed"         = "Self-employed",
    
    "p_lfstatRetired"             = "Retired (p) (Ref: Parental leave)",
    "p_lfstatUnemployed"          = "Unemployed (partner)",
    "p_lfstatFull-time employed"  = "Full-time employed (p)",
    "p_lfstatPart-time employed"  = "Part-time employed (p)",
    "p_lfstatMarginal employment" = "Marginal employment (p)",
    "p_lfstatSelf-employed"       = "Self-employed (p)",
    
    "nkidsliv"                    = "Number of kids (0-10)",
    "hlt1"                        = "Health status (1-5)",
    
    "reldur"                      = "Relationship duration (months) (0-522)"
  ),
  
  add_rows = extra_rows,
  estimate  = "{estimate}{stars}",
  stars = c( "#"  = 0.1, '*' = .05, '**' = .01, '***' = .001),
  fmt = 3,
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  notes = c(
    "Notes: # p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001. Standard errors in parentheses.",
    "Men only sample",
    "Controls include life satisfaction, relationship status, labor force status, partner’s labor force status, number of kids, health status, relationship duration and wave",
    "Source: pairfam (W1 - W13)."
  )
)




## Women ----
### M13-M15 ----
extra_rows <- tibble(
  term = "Wave control",
  `Model 13` = "Yes",
  `Model 14` = "Yes",
  `Model 15` = "Yes",
)
attr(extra_rows, "position") <- "coef_end"

modelsummary::msummary(
  list(
    "Model 13" = M13,
    "Model 14" = M14,
    "Model 15" = M15
  ),
  
  title = "Combined model relationship satisfaction, fixed effects analyses [Women only sample]",
  output = "M13-M15_pairfam.html",
  
  coef_map = c(
    "log_hhincgcee"               = "Log Net Equivalized Household Income (GCEE)",
    "benefit_dummy"               = "Grundsicherung (Grundsicherung, Sozialhilfe, AII) (0-1)",
    "wohngeld"                    = "Wohngeld (0-1)",
    
    "lifesat"                     = "Life satisfaction (0-10)",
    
    "relstat2Married"             = "Married (Ref: Cohabiting)",
    
    "lfstatRetired"               = "Retired (Ref: Parental leave)",
    "lfstatUnemployed"            = "Unemployed",
    "lfstatFull-time employed"    = "Full-time employed",
    "lfstatPart-time employed"    = "Part-time employed",
    "lfstatMarginal employment"   = "Marginal employment",
    "lfstatSelf-employed"         = "Self-employed",
    
    "p_lfstatRetired"             = "Retired (p) (Ref: Parental leave)",
    "p_lfstatUnemployed"          = "Unemployed (partner)",
    "p_lfstatFull-time employed"  = "Full-time employed (p)",
    "p_lfstatPart-time employed"  = "Part-time employed (p)",
    "p_lfstatMarginal employment" = "Marginal employment (p)",
    "p_lfstatSelf-employed"       = "Self-employed (p)",
    
    "nkidsliv"                    = "Number of kids (0-10)",
    "hlt1"                        = "Health status (1-5)",
    
    "reldur"                      = "Relationship duration (months) (0-522)"
  ),
  
  add_rows = extra_rows,
  estimate  = "{estimate}{stars}",
  stars = c( "#"  = 0.1, '*' = .05, '**' = .01, '***' = .001),
  fmt = 3,
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  notes = c(
    "Notes: # p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001. Standard errors in parentheses.",
    "Women only sample",
    "Controls include life satisfaction, relationship status, labor force status, partner’s labor force status, number of kids, health status, relationship duration and wave",
    "Source: pairfam (W1 - W13)."
  )
)









# RE ----
## M20 ----
extra_rows <- tibble(
  term = "Wave control",
  `Model 20` = "Yes"
)
attr(extra_rows, "position") <- "coef_end"


modelsummary::msummary(
  list(
    "Model 20" = M20
  ),
  
  title = "Household income and relationship satisfaction, random effects analyses",
  output = "M20_pairfam.html",
  
  coef_map = c(
    "log_hhincgcee"               = "Log Net Equivalized Household Income (GCEE)",
    
    "age"                         = "Age",
    "page"                        = "Partner age",
    "sexMale"                     = "Male (Ref.: Female)",
    
    "schoolLower secondary"                      = "Lower secondary",
    "schoolIntermediate secondary"               = "Intermediate secondary",
    "schoolUpper secondary (Fachhochschulreife)" = "Upper secondary (Fachhochschulreife)",
    "schoolUpper secondary (Abitur)"             = "Upper secondary (Abitur)",
  
    "migstatus"                                  = "Migration status",
  
    "relstat2Married"             = "Married (Ref: Cohabiting)",
    
    "lfstatRetired"               = "Retired (Ref: Parental leave)",
    "lfstatUnemployed"            = "Unemployed",
    "lfstatFull-time employed"    = "Full-time employed",
    "lfstatPart-time employed"    = "Part-time employed",
    "lfstatMarginal employment"   = "Marginal employment",
    "lfstatSelf-employed"         = "Self-employed",
    
    "p_lfstatRetired"             = "Retired (partner) (Ref: Parental leave)",
    "p_lfstatUnemployed"          = "Unemployed (p)",
    "p_lfstatFull-time employed"  = "Full-time employed (p)",
    "p_lfstatPart-time employed"  = "Part-time employed (p)",
    "p_lfstatMarginal employment" = "Marginal employment (p)",
    "p_lfstatSelf-employed"       = "Self-employed (p)",
    
    "nkidsliv"                    = "Number of kids (0-10)",
    "hlt1"                        = "Health status (1-5)",
    
    "reldur"                      = "Relationship duration (months) (0-522)",
    "cohort"                      = "Cohort",
    "east"                        = "East"
  ),
  
  add_rows = extra_rows,
  estimate  = "{estimate}{stars}",
  stars = c( "#"  = 0.1, '*' = .05, '**' = .01, '***' = .001),
  fmt = 3,
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  notes = c(
    "Notes: # p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001. Standard errors in parentheses.",
    "Controls include age, partner’s age, sex, highest school degree, elationship status, labor force status, partner’s labor force status, number of kids, health status, relationship duration cohort, region and wave",
    "Source: pairfam (W1 - W13)."
  )
)




## M21 ----
extra_rows <- tibble(
  term = "Wave control",
  `Model 21a` = "No",
  `Model 21b` = "No",
  `Model 21c` = "Yes"
)
attr(extra_rows, "position") <- "coef_end"


modelsummary::msummary(
  list(
    "Model 21a" = M21a,
    "Model 21b" = M21b,
    "Model 21c" = M21c
  ),
  
  title = "Grundsicherng and relationship satisfaction, random effects analyses",
  output = "M21_pairfam.html",
  
  coef_map = c(
    "benefit_dummy"               = "Grundsicherung (Grundsicherung, Sozialhilfe, AII) (0-1)",
    "log_hhincgcee"               = "Log Net Equivalized Household Income (GCEE)",
    
    "age"                         = "Age",
    "page"                        = "Partner age",
    "sexMale"                     = "Male (Ref.: Female)",
    
    "schoolLower secondary"                      = "Lower secondary",
    "schoolIntermediate secondary"               = "Intermediate secondary",
    "schoolUpper secondary (Fachhochschulreife)" = "Upper secondary (Fachhochschulreife)",
    "schoolUpper secondary (Abitur)"             = "Upper secondary (Abitur)",
    
    "migstatus"                                  = "Migration status",
    
    "relstat2Married"             = "Married (Ref: Cohabiting)",
    
    "lfstatRetired"               = "Retired (Ref: Parental leave)",
    "lfstatUnemployed"            = "Unemployed",
    "lfstatFull-time employed"    = "Full-time employed",
    "lfstatPart-time employed"    = "Part-time employed",
    "lfstatMarginal employment"   = "Marginal employment",
    "lfstatSelf-employed"         = "Self-employed",
    
    "p_lfstatRetired"             = "Retired (partner) (Ref: Parental leave)",
    "p_lfstatUnemployed"          = "Unemployed (p)",
    "p_lfstatFull-time employed"  = "Full-time employed (p)",
    "p_lfstatPart-time employed"  = "Part-time employed (p)",
    "p_lfstatMarginal employment" = "Marginal employment (p)",
    "p_lfstatSelf-employed"       = "Self-employed (p)",
    
    "nkidsliv"                    = "Number of kids (0-10)",
    "hlt1"                        = "Health status (1-5)",
    
    "reldur"                      = "Relationship duration (months) (0-522)",
    "cohort"                      = "Cohort",
    "east"                        = "East"
  ),
  
  add_rows = extra_rows,
  estimate  = "{estimate}{stars}",
  stars = c( "#"  = 0.1, '*' = .05, '**' = .01, '***' = .001),
  fmt = 3,
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  notes = c(
    "Notes: # p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001. Standard errors in parentheses.",
    "Grundsicherung indicates receipt of at least one means-tested benefit (Arbeitslosengeld II/Grundsicherung/Sozialhilfe)",
    "Controls include age, partner’s age, sex, highest school degree, elationship status, labor force status, partner’s labor force status, number of kids, health status, relationship duration cohort, region and wave",
    "Source: pairfam (W1 - W13)."
  )
)




## M22 ----
extra_rows <- tibble(
  term = "Wave control",
  `Model 22a` = "No",
  `Model 22b` = "Yes"
)
attr(extra_rows, "position") <- "coef_end"


modelsummary::msummary(
  list(
    "Model 22a" = M22a,
    "Model 22b" = M22b
    ),
  
  title = "Wohngeld and relationship satisfaction, random effects analyses",
  output = "M22_pairfam.html",
  
  coef_map = c(
    "wohngeld"                    = "Wohngeld (0-1)",
    "log_hhincgcee"               = "Log Net Equivalized Household Income (GCEE)",
    
    "age"                         = "Age",
    "page"                        = "Partner age",
    "sexMale"                     = "Male (Ref.: Female)",
    
    "schoolLower secondary"                      = "Lower secondary",
    "schoolIntermediate secondary"               = "Intermediate secondary",
    "schoolUpper secondary (Fachhochschulreife)" = "Upper secondary (Fachhochschulreife)",
    "schoolUpper secondary (Abitur)"             = "Upper secondary (Abitur)",
    
    "migstatus"                                  = "Migration status",
    
    "relstat2Married"             = "Married (Ref: Cohabiting)",
    
    "lfstatRetired"               = "Retired (Ref: Parental leave)",
    "lfstatUnemployed"            = "Unemployed",
    "lfstatFull-time employed"    = "Full-time employed",
    "lfstatPart-time employed"    = "Part-time employed",
    "lfstatMarginal employment"   = "Marginal employment",
    "lfstatSelf-employed"         = "Self-employed",
    
    "p_lfstatRetired"             = "Retired (partner) (Ref: Parental leave)",
    "p_lfstatUnemployed"          = "Unemployed (p)",
    "p_lfstatFull-time employed"  = "Full-time employed (p)",
    "p_lfstatPart-time employed"  = "Part-time employed (p)",
    "p_lfstatMarginal employment" = "Marginal employment (p)",
    "p_lfstatSelf-employed"       = "Self-employed (p)",
    
    "nkidsliv"                    = "Number of kids (0-10)",
    "hlt1"                        = "Health status (1-5)",
    
    "reldur"                      = "Relationship duration (months) (0-522)",
    "cohort"                      = "Cohort",
    "east"                        = "East"
  ),
  
  add_rows = extra_rows,
  estimate  = "{estimate}{stars}",
  stars = c( "#"  = 0.1, '*' = .05, '**' = .01, '***' = .001),
  fmt = 3,
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  notes = c(
    "Notes: # p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001. Standard errors in parentheses.",
    "Controls include age, partner’s age, sex, highest school degree, elationship status, labor force status, partner’s labor force status, number of kids, health status, relationship duration cohort, region and wave",
    "Source: pairfam (W1 - W13)."
  )
)







## In-Work ----
### M23-M24 ----
extra_rows <- tibble(
  term = "Wave control",
  `Model 23` = "Yes",
  `Model 24` = "Yes"
)
attr(extra_rows, "position") <- "coef_end"


modelsummary::msummary(
  list(
    "Model 23" = M24,
    "Model 24" = M23
  ),
  
  title = "Grundsicherung/Wohngeld and relationship satisfaction, random effects analyses [In-Work sample]",
  output = "M23-M24_pairfam.html",
  
  coef_map = c(
    "benefit_dummy"               = "Grundsicherung (Grundsicherung, Sozialhilfe, AII) (0-1)",
    "wohngeld"                    = "Wohngeld (0-1)",
    "log_hhincgcee"               = "Log Net Equivalized Household Income (GCEE)",
    
    "age"                         = "Age",
    "page"                        = "Partner age",
    "sexMale"                     = "Male (Ref.: Female)",
    
    "schoolLower secondary"                      = "Lower secondary",
    "schoolIntermediate secondary"               = "Intermediate secondary",
    "schoolUpper secondary (Fachhochschulreife)" = "Upper secondary (Fachhochschulreife)",
    "schoolUpper secondary (Abitur)"             = "Upper secondary (Abitur)",
    
    "migstatus"                                  = "Migration status",
    
    "relstat2Married"             = "Married (Ref: Cohabiting)",
    
    "lfstatRetired"               = "Retired (Ref: Parental leave)",
    "lfstatUnemployed"            = "Unemployed",
    "lfstatFull-time employed"    = "Full-time employed",
    "lfstatPart-time employed"    = "Part-time employed",
    "lfstatMarginal employment"   = "Marginal employment",
    "lfstatSelf-employed"         = "Self-employed",
    
    "p_lfstatRetired"             = "Retired (partner) (Ref: Parental leave)",
    "p_lfstatUnemployed"          = "Unemployed (p)",
    "p_lfstatFull-time employed"  = "Full-time employed (p)",
    "p_lfstatPart-time employed"  = "Part-time employed (p)",
    "p_lfstatMarginal employment" = "Marginal employment (p)",
    "p_lfstatSelf-employed"       = "Self-employed (p)",
    
    "nkidsliv"                    = "Number of kids (0-10)",
    "hlt1"                        = "Health status (1-5)",
    
    "reldur"                      = "Relationship duration (months) (0-522)",
    "cohort"                      = "Cohort",
    "east"                        = "East"
  ),
  
  add_rows = extra_rows,
  estimate  = "{estimate}{stars}",
  stars = c( "#"  = 0.1, '*' = .05, '**' = .01, '***' = .001),
  fmt = 3,
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  notes = c(
    "Notes: # p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001. Standard errors in parentheses.",
    "In-Work anchor only sample",
    "Controls include age, partner’s age, sex, highest school degree, elationship status, labor force status, partner’s labor force status, number of kids, health status, relationship duration cohort, region and wave",
    "Source: pairfam (W1 - W13)."
  )
)




## Men ----
### M25-M27 ----
extra_rows <- tibble(
  term = "Wave control",
  `Model 25` = "Yes",
  `Model 26` = "Yes",
  `Model 27` = "Yes"
)
attr(extra_rows, "position") <- "coef_end"


modelsummary::msummary(
  list(
    "Model 25" = M25,
    "Model 26" = M26,
    "Model 27" = M27
  ),
  
  title = "Combined model relationship satisfaction, random effects analyses [Men only sample]",
  output = "M25-M27_pairfam.html",
  
  coef_map = c(
    "log_hhincgcee"               = "Log Net Equivalized Household Income (GCEE)",
    "benefit_dummy"               = "Grundsicherung (Grundsicherung, Sozialhilfe, AII) (0-1)",
    "wohngeld"                    = "Wohngeld (0-1)",
    
    "age"                         = "Age",
    "page"                        = "Partner age",
    "sexMale"                     = "Male (Ref.: Female)",
    
    "schoolLower secondary"                      = "Lower secondary",
    "schoolIntermediate secondary"               = "Intermediate secondary",
    "schoolUpper secondary (Fachhochschulreife)" = "Upper secondary (Fachhochschulreife)",
    "schoolUpper secondary (Abitur)"             = "Upper secondary (Abitur)",
    
    "migstatus"                                  = "Migration status",
    
    "relstat2Married"             = "Married (Ref: Cohabiting)",
    
    "lfstatRetired"               = "Retired (Ref: Parental leave)",
    "lfstatUnemployed"            = "Unemployed",
    "lfstatFull-time employed"    = "Full-time employed",
    "lfstatPart-time employed"    = "Part-time employed",
    "lfstatMarginal employment"   = "Marginal employment",
    "lfstatSelf-employed"         = "Self-employed",
    
    "p_lfstatRetired"             = "Retired (partner) (Ref: Parental leave)",
    "p_lfstatUnemployed"          = "Unemployed (p)",
    "p_lfstatFull-time employed"  = "Full-time employed (p)",
    "p_lfstatPart-time employed"  = "Part-time employed (p)",
    "p_lfstatMarginal employment" = "Marginal employment (p)",
    "p_lfstatSelf-employed"       = "Self-employed (p)",
    
    "nkidsliv"                    = "Number of kids (0-10)",
    "hlt1"                        = "Health status (1-5)",
    
    "reldur"                      = "Relationship duration (months) (0-522)",
    "cohort"                      = "Cohort",
    "east"                        = "East"
  ),
  
  add_rows = extra_rows,
  estimate  = "{estimate}{stars}",
  stars = c( "#"  = 0.1, '*' = .05, '**' = .01, '***' = .001),
  fmt = 3,
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  notes = c(
    "Notes: # p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001. Standard errors in parentheses.",
    "Men only sample",
    "Controls include age, partner’s age, sex, highest school degree, elationship status, labor force status, partner’s labor force status, number of kids, health status, relationship duration cohort, region and wave",
    "Source: pairfam (W1 - W13)."
  )
)





## Women ----
### M28-M30 ----
extra_rows <- tibble(
  term = "Wave control",
  `Model 28` = "Yes",
  `Model 29` = "Yes",
  `Model 30` = "Yes"
)
attr(extra_rows, "position") <- "coef_end"


modelsummary::msummary(
  list(
    "Model 28" = M28,
    "Model 29" = M29,
    "Model 30" = M30
  ),
  
  title = "Combined model relationship satisfaction, random effects analyses [Women only sample]",
  output = "M28-M30_pairfam.html",
  
  coef_map = c(
    "log_hhincgcee"               = "Log Net Equivalized Household Income (GCEE)",
    "benefit_dummy"               = "Grundsicherung (Grundsicherung, Sozialhilfe, AII) (0-1)",
    "wohngeld"                    = "Wohngeld (0-1)",
    
    "age"                         = "Age",
    "page"                        = "Partner age",
    "sexMale"                     = "Male (Ref.: Female)",
    
    "schoolLower secondary"                      = "Lower secondary",
    "schoolIntermediate secondary"               = "Intermediate secondary",
    "schoolUpper secondary (Fachhochschulreife)" = "Upper secondary (Fachhochschulreife)",
    "schoolUpper secondary (Abitur)"             = "Upper secondary (Abitur)",
    
    "migstatus"                                  = "Migration status",
    
    "relstat2Married"             = "Married (Ref: Cohabiting)",
    
    "lfstatRetired"               = "Retired (Ref: Parental leave)",
    "lfstatUnemployed"            = "Unemployed",
    "lfstatFull-time employed"    = "Full-time employed",
    "lfstatPart-time employed"    = "Part-time employed",
    "lfstatMarginal employment"   = "Marginal employment",
    "lfstatSelf-employed"         = "Self-employed",
    
    "p_lfstatRetired"             = "Retired (partner) (Ref: Parental leave)",
    "p_lfstatUnemployed"          = "Unemployed (p)",
    "p_lfstatFull-time employed"  = "Full-time employed (p)",
    "p_lfstatPart-time employed"  = "Part-time employed (p)",
    "p_lfstatMarginal employment" = "Marginal employment (p)",
    "p_lfstatSelf-employed"       = "Self-employed (p)",
    
    "nkidsliv"                    = "Number of kids (0-10)",
    "hlt1"                        = "Health status (1-5)",
    
    "reldur"                      = "Relationship duration (months) (0-522)",
    "cohort"                      = "Cohort",
    "east"                        = "East"
  ),
  
  add_rows = extra_rows,
  estimate  = "{estimate}{stars}",
  stars = c( "#"  = 0.1, '*' = .05, '**' = .01, '***' = .001),
  fmt = 3,
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  notes = c(
    "Notes: # p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001. Standard errors in parentheses.",
    "Women only sample",
    "Controls include age, partner’s age, sex, highest school degree, elationship status, labor force status, partner’s labor force status, number of kids, health status, relationship duration cohort, region and wave",
    "Source: pairfam (W1 - W13)."
  )
)




## Duration ----
### M31-M32
extra_rows <- tibble(
  term = "Wave control",
  `Model 31` = "Yes",
  `Model 32` = "Yes"
)
attr(extra_rows, "position") <- "coef_end"


modelsummary::msummary(
  list(
    "Model 31" = M31,
    "Model 32" = M32
  ),
  
  title = "Grundsicherung/Wohngeld duration relationship satisfaction, random effects analyses",
  output = "M31-M32_pairfam.html",
  
  coef_map = c(
    "welfare_duration"            = "Grundsicherung duration (waves/years) (0-12)",
    "welfare_duration_sq"         = "Grundsicherung duration squared (waves/years) (0-12)",
    "wohngeld_duration"           = "Wohngeld duration (waves/years) (0-12)",
    "wohngeld_duration_sq"        = "Wohngeld duration squared (waves/years) (0-12)",
    "log_hhincgcee"               = "Log Net Equivalized Household Income (GCEE)",

    "age"                         = "Age",
    "page"                        = "Partner age",
    "sexMale"                     = "Male (Ref.: Female)",
    
    "schoolLower secondary"                      = "Lower secondary",
    "schoolIntermediate secondary"               = "Intermediate secondary",
    "schoolUpper secondary (Fachhochschulreife)" = "Upper secondary (Fachhochschulreife)",
    "schoolUpper secondary (Abitur)"             = "Upper secondary (Abitur)",
    
    "migstatus"                                  = "Migration status",
    
    "relstat2Married"             = "Married (Ref: Cohabiting)",
    
    "lfstatRetired"               = "Retired (Ref: Parental leave)",
    "lfstatUnemployed"            = "Unemployed",
    "lfstatFull-time employed"    = "Full-time employed",
    "lfstatPart-time employed"    = "Part-time employed",
    "lfstatMarginal employment"   = "Marginal employment",
    "lfstatSelf-employed"         = "Self-employed",
    
    "p_lfstatRetired"             = "Retired (partner) (Ref: Parental leave)",
    "p_lfstatUnemployed"          = "Unemployed (p)",
    "p_lfstatFull-time employed"  = "Full-time employed (p)",
    "p_lfstatPart-time employed"  = "Part-time employed (p)",
    "p_lfstatMarginal employment" = "Marginal employment (p)",
    "p_lfstatSelf-employed"       = "Self-employed (p)",
    
    "nkidsliv"                    = "Number of kids (0-10)",
    "hlt1"                        = "Health status (1-5)",
    
    "reldur"                      = "Relationship duration (months) (0-522)",
    "cohort"                      = "Cohort",
    "east"                        = "East"
  ),
  
  add_rows = extra_rows,
  estimate  = "{estimate}{stars}",
  stars = c( "#"  = 0.1, '*' = .05, '**' = .01, '***' = .001),
  fmt = 3,
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  notes = c(
    "Notes: # p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001. Standard errors in parentheses.",
    "Controls include age, partner’s age, sex, highest school degree, elationship status, labor force status, partner’s labor force status, number of kids, health status, relationship duration cohort, region and wave",
    "Source: pairfam (W1 - W13)."
  )
)
