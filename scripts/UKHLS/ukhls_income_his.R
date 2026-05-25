# Load ----
path_ukhls <- "C:/Users/Emir  PC/Desktop/PhD/Paper1/Datasets/UKDA-6614-stata/stata/stata14_se/ukhls"
waves <- letters[1:15]

for (wave in waves) {
  
  assign(
    paste0(wave, "_income"),
    
    read_dta(
      file.path(path_ukhls, paste0(wave, "_income.dta"))
    )
  )
}





# Append to long-format ----
waves <- letters[1:15]

income_long <- purrr::map2_dfr(
  waves,
  seq_along(waves),
  function(wave, wave_number) {
    income_data <- get(paste0(wave, "_income"))
    
    income_data %>%
      transmute(
        pidp = pidp,
        wavename = wave_number,
        hidp = .data[[paste0(wave, "_hidp")]],
        pno = .data[[paste0(wave, "_pno")]],
        fiseq = .data[[paste0(wave, "_fiseq")]],
        ficode = .data[[paste0(wave, "_ficode")]],
        frval = .data[[paste0(wave, "_frval")]],
        frwc = .data[[paste0(wave, "_frwc")]],
        frjt = .data[[paste0(wave, "_frjt")]]
      )
  }
)

## Check ----
income_long %>%
  count(wavename)
income_long %>%
  glimpse()
income_long %>%
  count(pidp, wavename) %>%
  filter(n > 1)


# Benefit dictionary ----
benefit_codes <- tibble::tribble(
  ~ficode, ~benefit_name,
  10, "DLA",
  14, "IB",
  15, "IS",
  16, "JSA",
  18, "CB",
  19, "CTC",
  20, "WTC",
  22, "HB",
  23, "CTS",
  33, "ESA",
  40, "UC",
  41, "PIP"
)


## Attach ----
income_selected <- income_long %>%
  mutate(
    ficode = as.numeric(ficode),
    frval = if_else(frval < 0, NA_real_, as.numeric(frval)),
    frwc = if_else(frwc < 0, NA_real_, as.numeric(frwc)),
    frjt = if_else(frjt < 0, NA_real_, as.numeric(frjt))
  ) %>%
  inner_join(benefit_codes, by = "ficode")


## collapse ----
get_mode <- function(x) {
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return(NA_real_)
  }
  
  as.numeric(names(sort(table(x), decreasing = TRUE))[1])
}

income_benefit_summary <- income_selected %>%
  group_by(pidp, wavename, benefit_name) %>%
  summarise(
    benefit_received = 1,
    benefit_amount = sum(frval, na.rm = TRUE),
    benefit_n_records = n(),
    benefit_joint = as.numeric(any(frjt == 2, na.rm = TRUE)),
    benefit_sole = as.numeric(any(frjt == 1, na.rm = TRUE)),
    benefit_period = get_mode(frwc),
    .groups = "drop"
  )
## Reshape ----
income_wide <- income_benefit_summary %>%
  tidyr::pivot_wider(
    names_from = benefit_name,
    values_from = c(
      benefit_received,
      benefit_amount,
      benefit_n_records,
      benefit_joint,
      benefit_sole,
      benefit_period
    ),
    values_fill = list(
      benefit_received = 0,
      benefit_amount = 0,
      benefit_n_records = 0,
      benefit_joint = 0,
      benefit_sole = 0
    )
  )





# Merge ----
uk <- uk %>%
  left_join(income_wide, by = c("pidp", "wavename"))
rm(a_income, b_income, c_income, d_income, e_income, f_income,
   g_income, h_income, i_income, j_income, k_income, l_income,
   m_income, n_income, o_income,
   benefit_codes, income_benefit_summary, income_long, income_selected, income_wide,
   waves, wave, path_ukhls, get_mode)