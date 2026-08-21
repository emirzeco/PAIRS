# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# R code for                                                                              # 
# Means-Tested Benefits and Relationship Satisfaction among Couples in the UK             #
# Author: Emir Zecovic                                                                    #
# Last Update: 19.08.2026                                                                 #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # #
# Report ESPAnet    #  
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
              "sjPlot", "lme4", "knitr", "kableExtra", "gt", "survey",
              "stringr", "flextable", "officer", "sf", "plm", "stargazer",
              "patchwork", "tidytext", "sjlabelled")
install.packages(setdiff(packages, rownames(installed.packages())))
suppressMessages(lapply(packages, library, character.only = TRUE, quietly=T))
rm(packages)
options(max.print=10000)

## Load ----
uk <- readRDS("C:/Users/Emir  PC/Desktop/PhD/Paper1/PAIRS/data/uk.rds")
#uk <- readRDS("~/PAIRS/data/uk.rds")




# Desc. table ----
## Dummys ----
## Dummys ----
# uk_RE$lfstatdummy <- dummy(uk_RE$lfstat, rownames(table(uk_RE$lfstat)))
# uk_RE$sexdummy <- dummy(uk_RE$sex, rownames(table(uk_RE$sex)))
# uk_RE$ethnicitydummy <- dummy(uk_RE$ethnicity, rownames(table(uk_RE$ethnicity)))
# uk_RE$educationdummy <- dummy(uk_RE$education, rownames(table(uk_RE$education)))
# uk_RE$gor_dvdummy <- dummy(uk_RE$gor_dv, rownames(table(uk_RE$gor_dv)))
# uk_RE$wavedummy <- dummy(uk_RE$wave, rownames(table(uk_RE$wave)))
#
#
#
#
# ## Descriptives ----
# modelvars <- cbind(
#   uk_RE$rel_happy,
#   uk_RE$benefit_MT,
#   uk_RE$log_hhgrslabinc_oecd,
#
#   uk_RE$lfstatdummy, uk_RE$nchild,
#
#   uk_RE$sexdummy, uk_RE$age, uk_RE$ethnicitydummy, uk_RE$educationdummy, uk_RE$gor_dvdummy,
#   uk_RE$wavedummy
#   )
#
#
# descrhelp <- t(round(stat.desc(modelvars, basic = TRUE, desc = TRUE), 2))
# descr <- cbind(
#   descrhelp[, "mean"],     # Mean
#   descrhelp[, "std.dev"],  # Standard deviation
#   descrhelp[, "min"],      # Min
#   descrhelp[, "max"],      # Max
#   descrhelp[, "nbr.val"]   # N
# )
# rm(descrhelp)
# colnames(descr) <- c("Mean", "SD", "Min", "Max", "N")
#
# rownames(descr) <- c(
#   "Relationship satisfaction (0-7)",
#   "Means-tested benefit receipt (0/1)",
#   "Equivalence household gross labor income (log)",
#
#   "Full-time employed",
#   "Part-time employed",
#   "Self-employed",
#   "Unemployed",
#   "Retired",
#   "Inactive",
#
#   "Number of children (0-10)",
#
#   "Male",
#   "Female",
#   "Age (20-40)",
#   "White British",
#   "Other",
#   "Lower",
#   "Middle",
#   "Higher",
#
#   "North East", "North West", "Yorkshire and the Humber", "East Midlands",
#   "West Midlands", "East of England", "London", "South East", "South West",
#   "Wales", "Scotland", "Northern Ireland",
#
#   "Wave 1",
#   "Wave 3",
#   "Wave 5",
#   "Wave 7",
#   "Wave 9",
#   "Wave 11",
#   "Wave 13",
#   "Wave 15"
#   )
# descr


## Word ----
# descr_df <- as.data.frame(descr)
# descr_df <- cbind(Variable = rownames(descr_df), descr_df)
# rownames(descr_df) <- NULL
#
# word_table <- flextable(descr_df) %>%
#   theme_vanilla() %>%
#   autofit() %>%
#   set_table_properties(width = 1, layout = "autofit")  # Ensures all columns fit
#
# # Save as Word document
# doc <- read_docx()
# doc <- body_add_flextable(doc, word_table)
# print(doc, target = "results/UKHLS/descriptive_table.docx")
#
# # Print to console for verification
# print(descr_df)







# Plot ----
## Emp / Unemp
benefit_employment_plot <- uk %>%
  mutate(
    employment = case_when(
      lfstat %in% c(
        "Full-time",
        "Part-time",
        "Self-employed"
      ) ~ "Employed (Full, part, self)",
      
      lfstat %in% c(
        "Unemployed",
        "Retired",
        "Inactive"
      ) ~ "Not employed (Unemployed, Retired, Inactive)",
      
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(employment),
    !is.na(benefit_MT),
    !is.na(rel_happy)
  ) %>%
  ggplot(
    aes(
      x = wave,
      y = rel_happy,
      color = factor(
        benefit_MT,
        levels = c(0, 1),
        labels = c(
          "No means-tested benefit receipt",
          "Means-tested benefit receipt"
        )
      ),
      group = benefit_MT
    )
  ) +
  geom_smooth(method = "lm") +
  facet_wrap(~ employment) +
  theme_bw() +
  labs(
    x = "Wave",
    y = "Relationship satisfaction",
    color = "Means-tested benefit"
  )

# ggsave(
#   filename = "benefit_employment_plot.png",
#   plot = benefit_employment_plot,
#   width = 10,
#   height = 6,
#   dpi = 300
# )





# Missings FE ----
missings <- c("rel_happy", "benefit_MT", "log_hhgrslabinc_oecd",
              "relstat2", "lfstat", "nchild", "wave")
uk_FE <- uk
prop.table(table(complete.cases(uk_FE[missings])))
uk_FE <- uk_FE[complete.cases(uk_FE[missings]), ]


# FE
## M1 (Lab. Income) ----
### M1a ----
M1a <- plm(
  rel_happy ~ log_hhgrslabinc_oecd,
  data = uk_FE,
  index = "pidp",
  model = "within"
)

### M1b ----
M1b <- plm(
  rel_happy ~ log_hhgrslabinc_oecd +
    relstat2 + relevel(lfstat, ref = "Retired") + nchild + 
    factor(wave),
  data = uk_FE,
  index = "pidp",
  model = "within"
)

### Model 1 ----
stargazer(list(M1a, M1b),
          
          column.labels=c("M1a", "M1b"),
          
          keep = c("log_hhgrslabinc_oecd"),
          
          covariate.labels=c("Equivalence household gross labor income (log)"),
          
          add.lines = list(
            c("Controls", "No", "Yes")
          ),
          
          align=TRUE,
          type = "html",
          omit.stat = "f",
          out = "M1.html")






# M2 (Means-tested benefit) ----
## M2a ----
M2a <- plm(
  rel_happy ~ benefit_MT,
  data = uk_FE,
  index = "pidp",
  model = "within"
)

## M2b ----
M2b <- plm(
  rel_happy ~ benefit_MT +
    log_hhgrsinc_oecd + 
    relstat2 + relevel(lfstat, ref = "Retired") + nchild + 
    factor(wave),
  data = uk_FE,
  index = "pidp",
  model = "within"
)

# Model 2 ----
stargazer(list(M2a, M2b),
          
          column.labels=c("M2a", "M2b"),
    
          keep = c("benefit_MT", "log_hhgrsinc_oecd"),
          
          covariate.labels=c(
            "Means-tested benefit receipt (0/1)",
            "Equivalence household gross labor income (log)"
            ),
          
          add.lines = list(
            c("Controls", "No", "Yes")
          ),
          
          align=TRUE,
          omit.stat = "f",
          type = "html",
          out = "M2.html")






# M3 & M4 (In-/Out-work) ----
uk_emp <- uk_FE %>%
  filter(
    lfstat %in% c(
      "Full-time employed",
      "Part-time employed",
      "Self-employed"
    )
  )

## M3 (In-work) ----
### M3a ----
M3a <- plm(
  rel_happy ~ benefit_MT,
  data = uk_emp,
  index = "pidp",
  model = "within"
)

### M3b ----
M3b <- plm(
  rel_happy ~ benefit_MT +
    log_hhgrslabinc_oecd + 
    relstat2 + lfstat + nchild +
    factor(wave),
  data = uk_emp,
  index = "pidp",
  model = "within"
)


## M4 (Out-work) ----
uk_unemp <- uk_FE %>%
  filter(
    lfstat %in% c(
      "Unemployed",
      "Retired",
      "Inactive"
    )
  )


### M4a ----
M4a <- plm(
  rel_happy ~ benefit_MT,
  data = uk_unemp,
  index = "pidp",
  model = "within"
)

### M4b ----
M4b <- plm(
  rel_happy ~ benefit_MT +
    log_hhgrslabinc_oecd + 
    relstat2 + relevel(lfstat, ref = "Inactive") + nchild +
    factor(wave),
  data = uk_unemp,
  index = "pidp",
  model = "within"
)





## Model 3 & 4 ----
stargazer(list(M3a, M3b, M4a, M4b),
          
          column.labels=c("M3a (In-work)", "M3b (In-work)", "M4a (Out-of-work)", "M4b (Out-of-work)"),
          
          keep = c("benefit_MT","log_hhgrslabinc_oecd", "lfstat"),
          
          covariate.labels=c("Means-tested benefit receipt (0/1)",
                             "Equivalence household gross labor income (log)",

                             "Part-time employed (Ref. Full-time employed)",
                             "Self-employed",
                             
                             "Unemployed (Ref. Inactive)",
                             "Retired"
                             ),
          
          add.lines = list(
            c("Controls", "No", "Yes", "No", "Yes")
          ),
          
          omit.stat = "f",
          align=TRUE,
          out = "M3_M4.html")






# M5 (Male) & M6 (Female) ----
## M5 (Male) ----
uk_men <- uk_FE %>%
  filter(sex == "Male")

### M5a ----
M5a <- plm(
  rel_happy ~ benefit_MT +
    log_hhgrslabinc_oecd + 
    relstat2 + relevel(lfstat, ref = "Retired") + nchild + 
    factor(wave),
  data = uk_men,
  index = "pidp",
  model = "within"
)

## M6 (Female) ----
uk_women <- uk_FE %>%
  filter(sex == "Female")

### M6a ----
M6a <- plm(
  rel_happy ~ benefit_MT +
    log_hhgrslabinc_oecd + 
    relstat2 + relevel(lfstat, ref = "Retired") + nchild + 
    factor(wave),
  data = uk_women,
  index = "pidp",
  model = "within"
)


## Model 5 & 6 ----
stargazer(list(M5a, M6a),
          
          column.labels=c("M5a (Men)", "M6a (Women)"),
          keep = c("benefit_MT","log_hhgrslabinc_oecd"),
          
          covariate.labels=c("Means-tested benefit receipt (0/1)",
                             "Equivalence household gross labor income (log)"
                             ),
          add.lines = list(
            c("Controls", "Yes","Yes")
          ),
          
          omit.stat = "f",
          align=TRUE,
          out = "M5_M6.html")





# Male/Female In-/Out-Work ----
uk_emp_m<- uk_men %>%
  filter(
    lfstat %in% c(
      "Full-time employed",
      "Part-time employed",
      "Self-employed"
    )
  )
uk_emp_w<- uk_women %>%
  filter(
    lfstat %in% c(
      "Full-time employed",
      "Part-time employed",
      "Self-employed"
    )
  )

uk_unemp_m <- uk_men %>%
  filter(
    lfstat %in% c(
      "Unemployed",
      "Retired",
      "Inactive"
    )
  )

uk_unemp_w <- uk_women %>%
  filter(
    lfstat %in% c(
      "Unemployed",
      "Retired",
      "Inactive"
    )
  )





## M7a ----
M7a <- plm(
  rel_happy ~ benefit_MT +
    log_hhgrslabinc_oecd + 
    relstat2 + lfstat + nchild +
    factor(wave),
  data = uk_emp_m,
  index = "pidp",
  model = "within"
)

## M7b ----
M7b <- plm(
  rel_happy ~ benefit_MT +
    log_hhgrslabinc_oecd + 
    relstat2 + lfstat + nchild +
    factor(wave),
  data = uk_emp_w,
  index = "pidp",
  model = "within"
)

## M8a ----
M8a <- plm(
  rel_happy ~ benefit_MT +
    log_hhgrslabinc_oecd + 
    relstat2 + relevel(lfstat, ref = "Inactive") + nchild +
    factor(wave),
  data = uk_unemp_m,
  index = "pidp",
  model = "within"
)

## M8b ----
M8b <- plm(
  rel_happy ~ benefit_MT +
    log_hhgrslabinc_oecd + 
    relstat2 + relevel(lfstat, ref = "Inactive") + nchild +
    factor(wave),
  data = uk_unemp_w,
  index = "pidp",
  model = "within"
)


### Model 7 & 8 ----
stargazer(list(M7a, M7b, M8a, M8b),
          
          column.labels=c("M7a (In-work + Men)", "M7b (In-Work + Women)",
                          "M8a (Out-of-work + Men)", "M8b (Out-of-work + Women)"
          ),
          
          keep = c("benefit_MT","log_hhgrslabinc_oecd", "lfstat"),
          
          covariate.labels=c("Means-tested benefit receipt (0/1)",
                             "Equivalence household gross labor income (log)",
                             
                             "Part-time employed (Ref. Full-time employed)",
                             "Self-employed",
                             
                             "Unemployed (Ref. Inactive)",
                             "Retired"
          ),
          add.lines = list(
            c("Controls", "Yes","Yes", "Yes", "Yes")
          ),
          
          align=TRUE,
          omit.stat = "f",
          out = "M7_M8.html")





# Appendix models ----
## DAS ----
uk_men <- uk %>%
  filter(sex == "Male")
uk_women <- uk %>%
  filter(sex == "Female")


uk_emp_m<- uk_men %>%
  filter(
    lfstat %in% c(
      "Full-time employed",
      "Part-time employed",
      "Self-employed"
    )
  )
uk_emp_w<- uk_women %>%
  filter(
    lfstat %in% c(
      "Full-time employed",
      "Part-time employed",
      "Self-employed"
    )
  )

uk_unemp_m <- uk_men %>%
  filter(
    lfstat %in% c(
      "Unemployed",
      "Retired",
      "Inactive"
    )
  )

uk_unemp_w <- uk_women %>%
  filter(
    lfstat %in% c(
      "Unemployed",
      "Retired",
      "Inactive"
    )
  )



M100 <- plm(
  DAS_rel_sat ~ benefit_MT +
    log_hhgrslabinc_oecd + 
    relstat2 + relevel(lfstat, ref = "Retired") + nchild + 
    factor(wave),
  data = uk_emp_m,
  index = "pidp",
  model = "within"
)


M200 <- plm(
  DAS_rel_sat ~ benefit_MT +
    log_hhgrslabinc_oecd + 
    relstat2 + relevel(lfstat, ref = "Retired") + nchild + 
    factor(wave),
  data = uk_emp_w,
  index = "pidp",
  model = "within"
)


##
M300 <- plm(
  DAS_rel_sat ~ benefit_MT +
    log_hhgrslabinc_oecd + 
    relstat2 + relevel(lfstat, ref = "Inactive") + nchild +
    factor(wave),
  data = uk_unemp_m,
  index = "pidp",
  model = "within"
)

M400 <- plm(
  DAS_rel_sat ~ benefit_MT +
    log_hhgrslabinc_oecd + 
    relstat2 + relevel(lfstat, ref = "Inactive") + nchild +
    factor(wave),
  data = uk_unemp_w,
  index = "pidp",
  model = "within"
)


stargazer(list(M100, M200, M300, M400),
          
          column.labels=c("DAS (In-work + Men)", "DAS (In-Work + Women)",
                          "DAS (Out-of-work + Men)", "DAS (Out-of-work + Women)"
          ),
          
          keep = c("benefit_MT","log_hhgrslabinc_oecd", "lfstat"),
          
          covariate.labels=c("Means-tested benefit receipt (0/1)",
                             "Equivalence household gross labor income (log)",
                             
                             "Part-time employed (Ref. Full-time employed)",
                             "Self-employed",
                             
                             "Unemployed (Ref. Inactive)",
                             "Retired"
          ),
          add.lines = list(
            c("Controls", "Yes","Yes", "Yes", "Yes")
          ),
          
          
          align=TRUE,
          omit.stat = "f",
          out = "M_Bonus.html")










## RE ----
### Missings RE ----
missings <- c("rel_happy", "benefit_MT", "log_hhgrslabinc_oecd",
              "relstat2", "lfstat", "nchild", "wave",
              
              "sex", "age", "ethnicity", "education", "gor_dv")
uk_RE <- uk
prop.table(table(complete.cases(uk_RE[missings])))
uk_RE <- uk_RE[complete.cases(uk_RE[missings]), ]


uk_emp <- uk_RE %>%
  filter(
    lfstat %in% c(
      "Full-time employed",
      "Part-time employed",
      "Self-employed"
    )
  )

uk_unemp <- uk_RE %>%
  filter(
    lfstat %in% c(
      "Unemployed",
      "Retired",
      "Inactive"
    )
  )


## M20 ----
M20 <- plm(
  rel_happy ~ benefit_MT +
    log_hhgrslabinc_oecd +
    sex + age + ethnicity + education + 
    relstat2 + relevel(lfstat, ref = "Retired") + nchild + gor_dv + 
    factor(wave),
  data = uk_RE,
  index = "pidp",
  model = "random"
)


## M21 ----
M21 <- plm(
  rel_happy ~ benefit_MT +
    log_hhgrslabinc_oecd +
    sex + age + ethnicity + education + 
    relstat2 + lfstat + nchild + gor_dv + 
    factor(wave),
  data = uk_emp,
  index = "pidp",
  model = "random"
)

## M22 ----
M22 <- plm(
  rel_happy ~ benefit_MT +
    log_hhgrslabinc_oecd +
    sex + age + ethnicity + education + 
    relstat2 + relevel(lfstat, ref = "Inactive") + nchild + gor_dv + 
    factor(wave),
  data = uk_unemp,
  index = "pidp",
  model = "random"
)




stargazer(
  list(M20, M21, M22),
  column.labels = c("M20 (All)", "M21 (In-work)", "M22 (Out-of-work)"),
  
  keep = c("benefit_MT","log_hhgrslabinc_oecd", "lfstat"),
  
  covariate.labels = c(
    "Means-tested benefit receipt (0/1)",
    "Equivalence household gross labor income (log)",
    
    "Full-time employed (Ref. Retired)",
    "Part-time employed",
    "Self-employed",
    "Unemployed",
    "Inactive",
    
    "Part-time employed (Ref. Full-time employed)",
    "Self-employed",
    
    "Unemployed (Ref. Inactive)",
    "Retired"
  ),
  
  add.lines = list(
    c("Controls", "Yes", "Yes", "Yes")
  ),
  
  notes = "Controls include sex, age, educational attainment, ethnicity, marital status, household income, labor market status, number of children, region, and wave.",
  notes.append = FALSE,
  
  align = TRUE,
  omit.stat = "f",
  out = "M_RE_1.html"
)
