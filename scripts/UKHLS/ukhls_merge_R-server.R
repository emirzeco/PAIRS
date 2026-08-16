# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# R code for                                                                              # 
# Means-Tested Benefits and Relationship Satisfaction among Low-Income Couples in the UK  #
# Author: Emir Zecovic                                                                    #
# Last Update: 13.05.2026                                                                 #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # # # # # # #
# DATA MERGE R-Server UKHLS   #
# # # # # # # # # # # # # # # # 



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Note: The data file produced will be at the individual level and cover all ages, irrespective of the variables requested  # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Sample Code for your request:  49273f34301f49f08c202efda20e346c
# Note: You can retrieve your code at any time using this
# URL: https://www.understandingsociety.ac.uk/code-creator/syntax?retrieval_id=7a16ac7f042646e1852db80470418bed

# Setup ----
rm(list=ls())

## WD ----
## Replace "where" with the file path of the working folder
## (where any temporary files created will be stored
#setwd("C:/Users/Emir  PC/Desktop/PhD/Paper1/PAIRS")
setwd("/posit_share/home/zecovic-e")


## UK ----
## Replace "where" with the folderpath where the data has been downloaded and unzipped
#ukhls <- "C:/Users/Emir  PC/Desktop/PhD/Paper1/Datasets/UKDA-6614-stata/stata/stata14_se/ukhls"
ukhls <- "~/PAIRS/data//UKHLS"


## Nurse ----
## Replace "where" with the folderpath where the nurse data has been downloaded and unzipped
## eg: c:/ukhls_data/UKDA-7261-stata/stata/stata13_se/
# ukhls_nurse <- "where/UKDA-7261-stata/stata/stata13_se/"

## Outpath ----
## Replace "where" with the filepath of the folder where you want to store the final dataset produced
## eg: c:/ukhls/results
outputpath <- "~/PAIRS/data/"

## The file produced will be named as below.
## If you want to change the name do it here.
outputfilename <- "UKHLS_long"

## Waves ----
## By default the data will be extracted from the waves whose letter prefixes are written below, and merged.
## If you want to a different selection of waves, make the change here
allWaves <- c("a b c d e f g h i j k l m n o")

## Indall ----  
## These variables from the indall files will be included. These include some key variables as determined by us PLUS any variables requested by you.
indallvars <- c("intdaty_dv age_dv butype country ethn_dv ff_ivlolw gor_dv hhsize hidp iviolw marstat mastat_dv nchild_dv pidp pno ppsex psnen01_lw psnen01_xw psnen91_lw psnen91_xw psnen99_lw psnen99_xw psneng2_lw psneng2_xw psnenub_lw psnenub_xw psnenui_lw psnenui_xw psnenus_lw psnenus_xw psu racel_dv sex sex_dv strata urban_dv")

## Indresp ----
## These variables from the indresp files will be included. These include some key variables as determined by us PLUS any variables requested by you.
indvars <- c("indinus_lw qfhigh_dv age_dv benbase1 benbase2 benbase3 benbase4 benbase96 benctc bendis1 bendis10 bendis11 bendis12 bendis13 bendis14 bendis15 bendis16 bendis2 bendis3 bendis4 bendis5 bendis6 bendis7 bendis8 bendis9 bendis96 bendis97 benhou1 benhou2 benhou3 benhou4 benhou5 benhou6 benhou7 benhou8 benhou9 benhou96 bentax1 bentax10 bentax2 bentax3 bentax4 bentax5 bentax6 bentax7 bentax8 bentax9 bentax96 benunemp1 benunemp2 benunemp3 benunemp4 benunemp5 benunemp6 benunemp7 benunemp96 btype1 btype10 btype11 btype12 btype13 btype14 btype2 btype3 btype4 btype5 btype6 btype7 btype8 btype9 btype96 cohabn country ethn_dv ff_emplw ff_ivlolw ffbrfedlw fimngrs_dv fimnlabgrs_dv fimnnet_dv fimnsben_dv ftexw gor_dv hhsize hhtype_dv hidp ind5mus_lw ind5mus_xw indbd91_lw iviolw jbstat pjbptft jbft_dv jshrs employ ncrr6 marstat mastat_dv mlstat sf12pcs_dv sf12mcs_dv sf1 scsf1 nbornlw nchild_dv othben1 othben10 othben2 othben3 othben4 othben5 othben6 othben7 othben8 othben9 othben96 othben97 pbnft1 pbnft10 pbnft11 pbnft12 pbnft13 pbnft14 pbnft15 pbnft16 pbnft17 pbnft18 pbnft19 pbnft2 pbnft3 pbnft4 pbnft5 pbnft6 pbnft7 pbnft8 pbnft9 pbnft96 pidp pno ppsex prfitb psu racel_dv scdassat_dv scdascoh_dv sclfsato scghq1_dv screlhappy livesp livewith sclfsato sex sex_dv strata tenure_dv ukborn urban_dv coh1by ncrry4 cohab_dv jbhas fenow j1none")

### Unused ----
#### Weight
# indbdub_lw indin01_lw indin01_xw indin91_lw indin91_xw indin99_lw indin99_xw inding2_lw inding2_xw indinub_lw indinub_xw
# indinui_lw indinui_xw indinus_lw indinus_xw indns91_lw indnsub_lw indpxg2_xw indpxub_lw indpxub_xw indpxui_lw indpxui_xw indpxus_lw
# indpxus_xw indscg2_xw indscub_lw indscub_xw indscui_lw indscui_xw indscus_lw indscus_xw

## Child ----
## These variables from the child files will be included. These include some key variables as determined by us PLUS any variables requested by you.
chvars <- c("age_dv chddvg2_xw chddvub_lw chddvub_xw chddvui_lw chddvui_xw country gor_dv hhsize hidp iviolw pidp pno ppsex psnen01_lw psnen91_lw psneng2_lw psneng2_xw psnenub_lw psnenub_xw psnenui_lw psnenui_xw psnenus_lw psnenus_xw psu sex sex_dv strata urban_dv")

## HH ----
## These variables from the hhresp files will be included. These include some key variables as determined by us PLUS any variables requested by you.
hhvars <- c("country fihhmnnet1_dv fihhmngrs_dv fihhmnprben_dv gor_dv hhden01_xw hhden91_xw hhden99_xw hhdeng2_xw hhdenub_xw hhdenui_xw hhdenus_xw hhsize hhtype_dv hidp hsivlw ieqmoecd_dv nkids_dv psu strata tenure_dv urban_dv xpgaslw xpleclw nadoecd_dv nchoecd_dv")

## Youth ----
## These variables from the youth files will be included. These include some key variables as determined by us PLUS any variables requested by you.
youthvars <- c("age_dv country ethn_dv gor_dv hidp pidp pno psu racel_dv sex sex_dv strata urban_dv ypsmlw ypwklw ythscg2_xw ythscub_xw ythscui_xw ythscus_xw")

## Nurse ----
## These variables from the nurse labblood file will be included. These include some key variables as determined by us PLUS any variables requested by you. 
# xlabbloodvars <- c("")
# 
# # These variables from the nurse epigenetic clocks file will be included. These include some key variables as determined by us PLUS any variables requested by you. 
# xepigenclockvars <- c("")
# 
# # These variables from the nurse proteomic file will be included. These include some key variables as determined by us PLUS any variables requested by you. 
# xproteovars <- c("")





# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  
# Anything below this line should not be changed! Any changes to the selection of variables and waves, and location of folders, should be made above  #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Function to check if a package is installed and install it if necessary
check_and_install <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE, type = "binary")
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE, type = "source", INSTALL_opts = "--no-multiarch")
      library(package, character.only = TRUE)
    }
  }
}

# Check and install necessary packages
check_and_install("haven")
check_and_install("dplyr")
check_and_install("tidyr")
check_and_install("foreach")

#  // this program returns all variable names with the wave prefix
getVars <- function(arg1, arg2) {
  if (arg2== "") {
    wavemyvars <- arg1
  } else if (arg1 != "" & !is.null(arg2)) {
    # Split the string into separate elements
    wavemyvar <- strsplit(arg1, " ")[[1]]
    # Add the prefix "*_" to each element
    wavemyvar_prefix <- paste0(arg2,"_", wavemyvar)  
    # Concatenate the elements back into a single string
    wavemyvars <- paste(wavemyvar_prefix, collapse = " ")
  }  else {
    wavemyvars <- ""
  }
  return(wavemyvars)
}

# // this program to returns  which variables exist in this wave
getExistingVars <- function(vars, df) {
  existingVars <- c()
  for (var in vars) {
    if (var %in% colnames(df)) {
      existingVars <- c(existingVars, var)
    }
  }
  return(existingVars)
}

# Split the wave string into individual letters
allWaves <- strsplit(allWaves, " ")[[1]]

# Loop through each wave
for (wave in allWaves) {
  # Find the wave number (letter corresponding to the wave)
  waveno <- which(strsplit("abcdefghijklmnopqrstuvwxyz", "")[[1]] == wave)
  
  # find the wave household vars  
  wavehhvars <- getVars(hhvars, wave)
  
  # find the wave individual vars
  waveindvars <- getVars(indvars, wave)
  
  # find the wave all individual vars
  waveindallvars <- getVars(indallvars, wave)
  
  # find the wave child vars
  wavechvars <- getVars(chvars, wave)
  
  # find the wave youth vars
  waveyouthvars <- getVars(youthvars, wave)
  
  # open the the household level file with the required variable
  hhresp_dat <- read_dta(paste0(ukhls, "/", wave, "_hhresp.dta"), encoding = "latin1")
  
  vars_to_check <- c(paste0(wave, "_hidp"), wavehhvars)
  vars_to_check <- unlist(strsplit(vars_to_check, " "))
  
  # get the existing variables using the getExistingVars function
  existingVars <- getExistingVars(vars_to_check, hhresp_dat)
  
  # keep only the existing variables in the data frame
  df_hhresp <- hhresp_dat %>% select(all_of(existingVars))
  
  # check if individual, child, or youth variables are required
  if (!is.null(indvars) || !is.null(chvars) || !is.null(youthvars)) {
    # if any individual variable is required, first merge INDALL keeping the pidp
    indall_data <- right_join(df_hhresp, read_dta(paste0(ukhls, "/", wave, "_indall.dta"),
                                                  encoding = "latin1"), by = paste0(wave, "_hidp"))
    
    # drop rows where pidp is missing (loose households with no individuals)
    indall_data <- indall_data[!is.na(indall_data$pidp), ]
    
    # get the existing variables using the getExistingVars function
    vars_to_check <- c("pidp", paste0(wave, "_hidp"), wavehhvars, waveindallvars)
    vars_to_check <- unlist(strsplit(vars_to_check, " "))
    existingVars <- getExistingVars(vars_to_check, indall_data)
    
    # keep only the existing variables in the data frame
    df_alldata <- indall_data %>% select(all_of(existingVars))
    
    # add any requested individual variables
    if (!is.null(indvars)){
      # Remove the duplicate variable from df_indresp before merging to avoid any suffix being added
      df_indresp <- haven::read_dta(
        paste0(ukhls, "/", wave, "_indresp.dta"),
        encoding = "latin1"
      )
      df_indresp <- df_indresp[ , !names(df_indresp) %in% names(df_alldata) | names(df_indresp) == "pidp"]
      
      indvars_dat <- merge(df_alldata, df_indresp, by = "pidp", all = TRUE)
      
      # get the existing variables using the getExistingVars function
      vars_to_check <- c("pidp", paste0(wave, "_hidp"), wavehhvars, waveindvars, waveyouthvars, wavechvars, waveindallvars)
      vars_to_check <- unlist(strsplit(vars_to_check, " "))
      existingVars <- getExistingVars(vars_to_check, indvars_dat)
      
      # keep only the existing variables in the data frame
      df_alldata <- indvars_dat %>% select(all_of(existingVars))
    }
    
    # add any requested youth variables
    if (!is.null(waveyouthvars)){
      # remove the duplicate variable from df_youth before merging to avoid any suffix being added
      df_youth <- read_dta(paste0(ukhls, "/", wave, "_youth.dta"), encoding = "latin1")
      df_youth <- df_youth[ , !names(df_youth) %in% names(df_alldata) | names(df_youth) == "pidp"]
      
      youth_dat <- merge(df_alldata, df_youth, by = "pidp", all = TRUE)
      
      # get the existing variables using the getExistingVars function
      vars_to_check <- c("pidp", paste0( wave, "_hidp"), wavehhvars, waveindvars, waveyouthvars, wavechvars, waveindallvars)
      vars_to_check <- unlist(strsplit(vars_to_check, " "))
      existingVars <- getExistingVars(vars_to_check, youth_dat)
      
      # keep only the existing variables in the data frame
      df_alldata <- youth_dat %>% select(all_of(existingVars))
    }
    
    # add any requested child variables
    if (!is.null(wavechvars)){
      df_child <- read_dta(paste0(ukhls, "/", wave, "_child.dta"), encoding = "latin1")
      df_child <- df_child[ , !names(df_child) %in% names(df_alldata) | names(df_child) == "pidp"]
      child_dat <- merge(df_alldata, df_child, by = "pidp", all = TRUE)
      
      # Get the existing variables using the getExistingVars function
      vars_to_check <- c("pidp", paste0(wave, "_hidp"), wavehhvars, waveindvars, waveyouthvars, wavechvars, waveindallvars)
      existingVars <- getExistingVars(unlist(strsplit(vars_to_check, " ")), child_dat)
      
      # Keep only the existing variables in the data frame
      df_alldata <- child_dat %>% select(all_of(existingVars))
    }
  }	
  # Create a wave variable
  df_alldata$wavename <- waveno
  
  # Drop the wave prefix from all variables
  colnames(df_alldata) <- gsub(paste0(wave, "_"), "", colnames(df_alldata))
  
  # Save the file that was created
  saveRDS(df_alldata, file = paste0("temp_", wave, ".rds"))
  
}


# Nurse Logic ----
## setup Labblood,xepigen_clocks and proteo 
# df_allnurse <- NULL
# xlabbloodvars <- getVars(xlabbloodvars,"")
# xepigenvars <- getVars(xepigenclockvars,"")
# xproteovars <- getVars(xproteovars,"")
# 
# # add any requested labblood variables
# if (!is.null(xlabbloodvars)){
#   df_labblood <- read_dta(paste0(ukhls_nurse,"/", "xlabblood_ns.dta"))
#   # Get the existing variables using the getExistingVars function
#   vars_to_check <- c("pidp", xlabbloodvars)
#   existingVars <- getExistingVars(unlist(strsplit(vars_to_check, " ")), df_labblood)
#   # Keep only the existing variables in the data frame
#   df_allnurse <- df_labblood %>% select(all_of(existingVars))
# }
# 
# # add any requested epigen variables
# if (!is.null(xepigenvars)){
#   df_epigen <- read_dta(paste0(ukhls_nurse,"/", "xepigen_clocks_ns.dta"))
#   epigen_dat <- merge(df_allnurse, df_epigen, by = "pidp", all = TRUE)
#   # Get the existing variables using the getExistingVars function
#   vars_to_check <- c("pidp", xlabbloodvars, xepigenvars)
#   existingVars <- getExistingVars(unlist(strsplit(vars_to_check, " ")), epigen_dat)
#   # Keep only the existing variables in the data frame
#   df_allnurse <- epigen_dat %>% select(all_of(existingVars))
# }
# 
# # add any requested proteo variables
# if (!is.null(xproteovars)){
#   df_proteo <- read_dta(paste0(ukhls_nurse,"/", "xproteo_ns.dta"))
#   proteo_dat <- merge(df_allnurse, df_proteo, by = "pidp", all = TRUE)
#   # Get the existing variables using the getExistingVars function
#   vars_to_check <- c("pidp", paste0(wave, "_hidp"), xlabbloodvars, xepigenvars, xproteovars )
#   existingVars <- getExistingVars(unlist(strsplit(vars_to_check, " ")), proteo_dat)
#   # Keep only the existing variables in the data frame
#   df_allnurse <- proteo_dat %>% select(all_of(existingVars))
# }
# 
# # Save the file that was created
# # Drop the wave prefix from all variables
# colnames(df_allnurse) <- gsub(paste0(wave, "_"), "", colnames(df_allnurse))
# saveRDS(df_allnurse, file = paste0("temp_nurse", ".rds"))# Loop through the remaining waves and append them in the long format


# Final file ----
df_append <- NULL  # Initialize an empty dataframe to store the result
for (w in allWaves) {
  df_temp <- readRDS(paste0("temp_", w, ".rds"))
  df_append <- bind_rows(df_append, df_temp)
}

# Move pidp to the beginning of the file
df_longdata <- df_append[order(df_append$pidp), ]

## Merge ----
## Merge wave-independent nurse/lab data AFTER combining waves
#df_longdata <- merge(df_longdata, df_allnurse, by = "pidp", all = TRUE)
df_longdata <- df_longdata[order(df_longdata$pidp), ]

# Check how many observations are available from each wave
table(df_longdata$wavename)

## Save ----
saveRDS(df_longdata, file = paste0(outputpath, "/", outputfilename, ".rds"))
haven::write_dta(df_longdata, path = paste0(outputpath, "/", outputfilename, ".dta"))

# Erase temporary files
for (wave in allWaves) {
  file.remove(paste0("temp_", wave, ".rds"))
}