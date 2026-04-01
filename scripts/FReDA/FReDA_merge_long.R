# FReDAmerge.R  — tidyverse translation of FReDAmerge.do
# Release: 2025-11-06
# - Requires: tidyverse, haven, janitor, tidyr, purrr, labelled (optional)
# - Set the paths and variable lists in the TODO sections below.
library(tidyverse)
library(haven)
library(janitor)
library(tidyr)
library(purrr)
library(labelled)

# Prepare appending process ----
## TODO: set your paths
inpath       <- "C:/Users/Emir-/Desktop/phd/freda/data"                   # e.g. "D:/freda/raw"
outpath      <- "C:/Users/Emir-/Desktop/phd/freda"                        # e.g. "D:/freda/work"
pairfam_path <- "C:/Users/Emir-/Desktop/phd/data/FReDA/pairfam_v14-2-0/Data/Stata" 

## Wave definition ----
start_wave <- 1   # first wave to use
end_wave   <- 7   # last wave to use

# Append all FReDA waves (variable-selected) ----

## TODO: choose variables of interest ----
### (make sure id, welle, sample are included)
### Example: myvar <- c("id","welle","sample","sex","birthyr","edu","inc","region")
myvar <- c("id", "pid", "welle", "sample",
           "sex", "sex_reg", "age", "age_reg",
           "migback", "cohort", "east", "samesex",
           "school", "educy", "voctrain",
           "lfstat", "sd55", "job40", 
           
           # Income var
           "hhincnet", "inc13", "inc23", "hhincgcee", "hhincoecd", "inc52",
           # Benefit var
           "inc53i8","inc53i10", "inc53i12", "inc53i13", "inc53i22", "inc53i24",
           # Satisfaction relationship/life
           "sat3", "sat6",                                               
           
           # Relationship conflict
           "pa18i1", "pa18i2", "pa18i3", "pa18i4", "pa18i5",                   
           "pa18i6", "pa18i7", "pa18i8",
           "pa18i10", "pa18i11", "pa18i12", "pa18i13", "pa18i14", "pa18i15",
           "pa18i16",
           
           # Relationship status for Wave 3 (W1B)
           "pstat", "separation", "sd3", "bpa17",
           # Cohab
           "bpa4", "bpa7", "relstat"
           )

## Build list of FReDA files ----
freda_files <- paste0(inpath, "/FREDAanchor", start_wave:end_wave, ".dta")

## Helper: read one wave and keep only variables existing in that file (intersection)
read_freda_sel <- function(path, keep_vars) {
  df <- read_dta(path)
  # Keep only requested variables that actually exist in this file
  df %>% select(any_of(keep_vars)) %>% clean_names()
}

## Append all waves (selected vars only) ----
freda_long <- map_dfr(freda_files, read_freda_sel, keep_vars = myvar)

## Optional: sort & quick checks (analog to Stata: sort/browse/isid)
freda_long <- freda_long %>% arrange(id, welle)

## If you want to check uniqueness: 
## stopifnot(!any(duplicated(freda_long %>% select(id, welle))))

## Save appended dataset ----
write_dta(freda_long, file.path(outpath, "FREDAanchor_long.dta"))

# Create a balanced dataset ----
## fill missing FReDA waves for all respondents

## Reload to mirror do-file flow ----
freda_long <- read_dta(file.path(outpath, "FREDAanchor_long.dta")) %>% clean_names()

# Make a fully balanced panel across all ids and all waves from start_wave..end_wave
all_ids   <- freda_long %>% distinct(id)
all_waves <- tibble(welle = start_wave:end_wave)

freda_balanced <- all_ids %>%
  crossing(all_waves) %>%
  left_join(freda_long, by = c("id", "welle")) %>%
  arrange(id, welle)

## Fill time-constant variables across all waves per id ----

## In the .do file, this is done for 'sample' and any user-chosen vars
## TODO: list your time-constant variables here
## Example: time_const <- c("sample", "sex", "birthyr")

time_const <- c("sample", "sex", "sex_reg",
                "school", "educy", "voctrain",
                "migback", "cohort") # ADD MORE TIME-CONSTANT VARIABLES
if (length(time_const) > 0) {
  freda_balanced <- freda_balanced %>%
    group_by(id) %>%
    tidyr::fill(all_of(time_const), .direction = "downup") %>%
    ungroup()
}

## Save long + balanced ----
freda_balanced <- freda_balanced %>% arrange(id, welle)
write_dta(freda_balanced, file.path(outpath, "FREDAanchor_balanced.dta"))


# Append pairfam waves ----
## Load balanced FReDA dataset
#df <- read_dta(file.path(outpath, "FREDAanchor_balanced.dta")) %>% clean_names()