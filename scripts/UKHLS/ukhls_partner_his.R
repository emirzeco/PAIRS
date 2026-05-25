# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# R code for                                                                              # 
# Means-Tested Benefits and Relationship Satisfaction among Low-Income Couples in the UK  #
# Author: Emir Zecovic                                                                    #
# Last Update: 12.05.2026                                                                 #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # # # # # # #
# Partnership History UKHLS   #
# # # # # # # # # # # # # # # #

# Setup ----
## Packages ----
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

packages <- c("tidyverse", "haven", "pastecs", "datawizard",
              "ggplot2", "ggrepel", "sjPlot", "lme4", "knitr", "kableExtra", 
              "stringr", "flextable", "officer", "sf", "plm", "stargazer",
              "patchwork", "tidytext", "sjlabelled")
install.packages(setdiff(packages, rownames(installed.packages())))
suppressMessages(lapply(packages, library, character.only = TRUE, quietly=T))
rm(packages)
options(max.print=10000)



## Load ----
p_history <- haven::read_dta("C:/Users/Emir  PC/Desktop/PhD/Paper1/Datasets/UKDA-6614-stata/stata/stata14_se/ukhls/phistory_long.dta")