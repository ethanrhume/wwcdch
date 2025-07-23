#### Setup ####
rm(list = ls())

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(here, 
               tidyverse,
               purrr,
               broom,
               flextable,
               gtsummary) #Loads necessary packages + installs if needed

here::i_am("scripts/02_transform.R") #Ensures `here` properly IDs top-level directory

source(here("R", "custom_functions.R")) #Adds custom functions to environment

dat <- read_csv(here("data", "dat_clean.csv"))
wa_cases <- read_csv(here("data", "dat_wa_cases.csv"))
ww_cases <- read_csv(here("data", "dat_ww_cases.csv"))
notifiable <- read_csv(here("data", "notifiable.csv"))
con_index <- read_csv(here("data", "con_index.csv"))
pop <- read_csv(here("data", "pop.csv"))
enteric <- readRDS(here("data", "enteric.rds"))

sti_sets_helper <- list.files(path = here("data", "sti_sets"), 
                              pattern = "\\.csv$", full.names = TRUE)
sti_sets <- lapply(sti_sets_helper, read_csv)
names(sti_sets) <- tools::file_path_sans_ext(basename(sti_sets_helper))

fiveyr_notifiable <- notifiable |>
  tail(5)

#### Regression Analysis ####
#We start by performing a basic linear regression analysis on each condition.

#Select only relevant vars, excluding those with no data
vars <- names(notifiable)[grepl("^rate_|^warate_", names(notifiable))]
vars <- vars[sapply(notifiable[vars], function(x) any(!is.na(x)))]
fiveyr_vars <- names(fiveyr_notifiable)[grepl("^rate_|^warate_", 
                                              names(fiveyr_notifiable))]
fiveyr_vars <- fiveyr_vars[sapply(fiveyr_notifiable[fiveyr_vars], 
                                  function(x) any(!is.na(x)))]

#Fit models and add to list called models
models <- lapply(vars, function(var) {
  lm(as.formula(paste(var, "~ year")), data = notifiable)
})
fiveyr_models <- lapply(fiveyr_vars, function(var){
  lm(as.formula(paste(var, "~ year")), data = fiveyr_notifiable)
})

names(models) <- vars
names(fiveyr_models) <- fiveyr_vars

#### Largest Increases and Decreases ####
#We then use previously calculated regression coefficients to determine which 
#conditions have seen the largest increases or decreases since the beginning of
#data collection as well as the last five years.

#Extract slopes and r-squared values from our models
model_summary <- map2_df(models, names(models), ~ {
  coef <- tidy(.x)
  slope <- coef |> filter(term == "year") |> pull(estimate)
  r2 <- summary(.x)$r.squared
  tibble(var = .y, slope = slope, r_squared = r2)
})

fiveyr_model_summary <- map2_df(fiveyr_models, names(fiveyr_models), ~ {
  coef <- tidy(.x)
  slope <- coef |> filter(term == "year") |> pull(estimate)
  r2 <- summary(.x)$r.squared
  tibble(var = .y, slope = slope, r_squared = r2)
})

#Using logical statements, determine whether each represents a Walla Walla or WA
#model
model_summary <- model_summary |>
  mutate(
    index = as.integer(gsub(".*_(\\d+)", "\\1", var)),
    type = ifelse(grepl("^warate_", var), "wa", "ww")
  )

fiveyr_model_summary <- fiveyr_model_summary |>
  mutate(
    index = as.integer(gsub(".*_(\\d+)", "\\1", var)),
    type = ifelse(grepl("^warate_", var), "wa", "ww")
  )

#Integrate WW vs WA into headers
summary_wide <- model_summary |>
  pivot_wider(
    id_cols = index,
    names_from = type,
    values_from = c(slope, r_squared),
    names_glue = "{type}_{.value}"
  ) |>
  rename(
    slope = ww_slope,
    slope_r2 = ww_r_squared,
    wa_slope = wa_slope,
    wa_slope_r2 = wa_r_squared
  ) |>
  mutate(slope_diff = slope - wa_slope)

fiveyr_summary_wide <- fiveyr_model_summary |>
  pivot_wider(
    id_cols = index,
    names_from = type,
    values_from = c(slope, r_squared),
    names_glue = "{type}_{.value}"
  ) |>
  rename(
    slope = ww_slope,
    slope_r2 = ww_r_squared,
    wa_slope = wa_slope,
    wa_slope_r2 = wa_r_squared
  ) |>
  mutate(slope_diff = slope - wa_slope)

#Joining with con_index
slope_table_full <- con_index |>
  mutate(index = row_number()) |>
  left_join(summary_wide, by = "index") |>
  select(condition, slope, slope_r2, wa_slope, wa_slope_r2, slope_diff)

fiveyr_slope_table_full <- con_index |>
  mutate(index = row_number()) |>
  left_join(fiveyr_summary_wide, by = "index") |>
  select(condition, slope, slope_r2, wa_slope, wa_slope_r2, slope_diff)

#Let's remove the many rare conditions for which WW county has no data and therefore
#a slope of zero, at least for this analysis.

slope_table <- slope_table_full |>
  filter(slope != 0)

fiveyr_slope_table <- fiveyr_slope_table_full |>
  filter(slope != 0)

#### Greatest Burden of Disease ####

#We identify the top five conditions with the greatest burden of disease in WWC
#overall, as well as the last five years, as compared to the WA state average

#We will calculate an incidence rate over the entire time period as well as the
#last five years using the 'pop' dataframe to sum person-years at-risk and the 
#'cases' dataframes to sum historic cases. 

#We will calculate relative risks to do the actual comparisons, using our incidence
#rates. It's important to exclude any condition where the historic toal case number
#is less than 5, or 16 for STIs.

wa_pop_sum <- sum(pop$washington)
ww_pop_sum <- sum(pop$walla_walla)
wa_pop_sum5 <- sum(tail(pop$washington, 5))
ww_pop_sum5 <- sum(tail(pop$walla_walla, 5))

historic_ir <- data.frame(
  condition = ww_cases$condition,
  ww_case = ww_cases$total,
  ww_pop = ww_pop_sum,
  wa_case = wa_cases$total,
  wa_pop = wa_pop_sum
)

fiveyr_ir <- data.frame(
  condition = wa_cases$condition,
  ww_case = ww_cases$total5,
  ww_pop = ww_pop_sum5,
  wa_case = wa_cases$total5,
  wa_pop = wa_pop_sum5
)

historic_ir <- historic_ir |>
  mutate(ww_ir = ww_case / ww_pop * 100000,
         wa_ir = wa_case / wa_pop * 100000) |>
  mutate(STI = ifelse(
    str_detect(condition, 
               "Chancroid|Chlamydia|Gonorrhea|Herpes|HIV|Lymphogranuloma|Hepatitis|Syphilis"),
    1,
    0)) |>
  mutate(rr = ww_ir / wa_ir)

fiveyr_ir <- fiveyr_ir |>
  mutate(ww_ir = ww_case / ww_pop * 100000,
         wa_ir = wa_case / wa_pop * 100000) |>
  mutate(STI = ifelse(
    str_detect(condition, 
               "Chancroid|Chlamydia|Gonorrhea|Herpes|HIV|Lymphogranuloma|Hepatitis|Syphilis"),
    1,
    0)) |>
  mutate(rr = ww_ir / wa_ir)

topburden_historic <- historic_ir |>
  filter(
    (STI == 1 & ww_case >= 17 & wa_case >= 17) |
      (STI == 0 & ww_case >= 6 & wa_case >= 6)
  ) |>
  arrange(desc(rr)) |>
  slice_head(n = 5)

topburden_fiveyr <- fiveyr_ir |>
  filter(
    (STI == 1 & ww_case >= 17 & wa_case >= 17) |
      (STI == 0 & ww_case >= 6 & wa_case >= 6)
  ) |>
  arrange(desc(rr)) |>
  slice_head(n = 5)

#### STIs and Enteric Illness Analyses ####
#We explore who holds the burden of disease for STIs and enteric illnesses based
#on race/ethnicity, sex, and age. We also determine whether this has shifted over
#time.
### Enteric ###
raw_summary_enteric_sex <- enteric |> 
  filter(condition != "STEC") |>
  droplevels() |>
  tbl_cross(row = condition, col = sex, 
            percent = "row",
            missing = "no",
            label = condition ~ "Enteric illness") |> 
  modify_header(stat_by = "**{level}**", stat_0 = "Total") |> 
  modify_spanning_header(starts_with("stat_") ~ "**Sex**")

raw_summary_enteric_sex$table_body <- raw_summary_enteric_sex$table_body |>
  mutate(across(starts_with("stat_"),
                ~ ifelse(parse_number(.x) <= 5, "≤ 5", .x)))

raw_summary_enteric_sex |> as_flex_table()

raw_summary_enteric_race <- enteric |>
  tbl_cross(row = race_1, col = condition,
            percent = "column",
            missing = "no",
            label = race_1 ~ "Race") |>
  modify_header(stat_by = "**{level}**", stat_0 = "Total") |>
  modify_spanning_header(starts_with("stat_") ~ "**Enteric illness**")

raw_summary_enteric_race$table_body <- raw_summary_enteric_race$table_body |>
  mutate(across(starts_with("stat_"),
                ~ ifelse(parse_number(.x) <= 5, "≤ 5", .x)))

raw_summary_enteric_race |> as_flex_table()

raw_summary_enteric_eth <- enteric |>
  tbl_cross(row = hispanic, col = condition,
            percent = "column",
            missing = "no",
            label = hispanic ~ "Ethnicity") |>
  modify_header(stat_by = "**{level}**", stat_0 = "Total") |>
  modify_spanning_header(starts_with("stat_") ~ "**Enteric illness**")

raw_summary_enteric_eth$table_body <- raw_summary_enteric_eth$table_body |>
  mutate(across(starts_with("stat_"),
                ~ ifelse(parse_number(.x) <= 5, "≤ 5", .x)))

raw_summary_enteric_eth |> as_flex_table()

raw_summary_enteric_age <- enteric |> 
  filter(condition != "STEC") |>
  droplevels() |>
  tbl_cross(row = agecat, col = condition,
            percent = "column",
            missing = "no",
            label = agecat ~ "Age category") |>
  modify_header(stat_by = "**{level}**", stat_0 = "Total") |>
  modify_spanning_header(starts_with("stat_") ~ "**Enteric illness**")

raw_summary_enteric_age$table_body <- raw_summary_enteric_age$table_body |>
  mutate(across(starts_with("stat_"),
                ~ ifelse(parse_number(.x) <= 5, "≤ 5", .x)))

raw_summary_enteric_age |> as_flex_table()


### STIs ###

chl_gon_hcv_helper <- c("chlamydia_sex", "gonorrhea_sex", "hcv_sex")
chl_gon_hcv <- sti_sets[chl_gon_hcv_helper]

add_total <- function(df) {
  df$total <- rowSums(df[, -1], 
                      na.rm = TRUE)
  return(df)
}

chl_gon_hcv <- lapply(chl_gon_hcv, add_total)

