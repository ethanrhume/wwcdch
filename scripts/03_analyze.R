#### Setup ####
rm(list = ls())

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(here, 
               tidyverse,
               purrr,
               broom,
               flextable,
               gtsummary,
               epiR,
               haven,
               gt) #Loads necessary packages + installs if needed

here::i_am("scripts/03_analyze.R") #Ensures `here` properly IDs top-level directory

source(here("R", "custom_functions.R")) #Adds custom functions to environment

#### Read in data ####
dat <- readRDS(here("data", "output", "dat_clean.rds"))
wa_cases <- readRDS(here("data", "output", "dat_wa_cases.rds"))
ww_cases <- readRDS(here("data", "output", "dat_ww_cases.rds"))
notifiable <- readRDS(here("data", "output", "notifiable.rds"))
con_index <- readRDS(here("data", "output", "con_index.rds"))
pop <- readRDS(here("data", "output", "pop.rds"))
enteric <- readRDS(here("data", "output", "enteric.rds"))
ww_relprop <- readRDS(here("data", "output", "ww_relprop.rds"))

sti_sets_helper <- list.files(path = here("data", "output", "sti_sets"), 
                              pattern = "\\.rds$", full.names = TRUE)
sti_sets <- lapply(sti_sets_helper, readRDS)
names(sti_sets) <- tools::file_path_sans_ext(basename(sti_sets_helper))

fiveyr_notifiable <- notifiable |>
  tail(5)

#### Calculate Incidence Rates ####
### 1. Walla Walla IR by year and condition###
ww_inc <- ww_cases |>
  select(-c(total, total5))

names(ww_inc) <- gsub("_case", "", names(ww_inc))

ww_inc <- pivot_longer(ww_inc, cols = c("2010", "2011", "2012", "2013", "2014",
                                        "2015", "2016", "2017", "2018", "2019",
                                        "2020", "2021", "2022", "2023"),
                       names_to = "year", values_to = "count")

ww_inc <- merge(ww_inc, pop, by = "year")

ww_inc$pop <- ww_inc$walla_walla
  
ww_inc <- ww_inc |> 
  select(c(year, condition, count, pop)) |>
  mutate(inc = count / pop * 100000,
         lower = qchisq(0.025, 2 * count) / (2 * pop) * 100000,
         upper = qchisq(0.975, 2 * (count + 1)) / (2 * pop) * 100000) |>
  mutate(
    across(c(inc, lower, upper),
    ~ case_when(
      count <= 16 & count > 0 & str_detect(condition, "Chancroid|Chlamydia|Gonorrhea|Hepatitis|Herpes|HIV|Syphilis") ~ tagged_na("a"),
      count <= 5 & count > 0 ~ tagged_na("a"),
      count == NA & condition == "Lead, Child Blood" ~ tagged_na("a"), #WTN presuppressed data has to be accounted for
      is.na(.)   ~ NA_real_,
      TRUE       ~ .
    )
  ))|>
  arrange(year, condition)

### 2. Washington IR by year and condition###
wa_inc <- wa_cases |>
  select(-c(total, total5))

names(wa_inc) <- gsub("_wa_case", "", names(wa_inc))

wa_inc <- pivot_longer(wa_inc, cols = c("2010", "2011", "2012", "2013", "2014",
                                        "2015", "2016", "2017", "2018", "2019",
                                        "2020", "2021", "2022", "2023"),
                       names_to = "year", values_to = "count")

wa_inc <- merge(wa_inc, pop, by = "year")

wa_inc$pop <- wa_inc$washington

wa_inc <- wa_inc |> 
  select(c(year, condition, count, pop)) |>
  mutate(inc = count / pop * 100000,
         lower = qchisq(0.025, 2 * count) / (2 * pop) * 100000,
         upper = qchisq(0.975, 2 * (count + 1)) / (2 * pop) * 100000) |>
  arrange(year, condition)
  
### 3. Walla Walla IR by condition, historic ###
wa_pop_sum5 <- sum(tail(pop$washington, 5))
ww_pop_sum5 <- sum(tail(pop$walla_walla, 5))

ww_historic_inc <- data.frame(
  condition = unique(ww_inc$condition)
)

ww_historic_inc <- ww_historic_inc |>
  arrange(condition) |>
  mutate(
    count = tapply(ww_inc$count, ww_inc$condition, sum),
    pop = sum(pop$walla_walla),
    inc = count / pop * 100000,
    lower = qchisq(0.025, 2 * count) / (2 * pop) * 100000,
    upper = qchisq(0.975, 2 * (count + 1)) / (2 * pop) * 100000)


### 4. Walla Walla IR by condition, last 5 years ###
ww_fiveyr_inc <- data.frame(
  condition = unique(ww_inc$condition)
)

ww_fiveyr_inc <- ww_fiveyr_inc |>
  arrange(condition) |>
  mutate(
    count = tapply(ww_inc$count[ww_inc$year %in% unique(fiveyr_notifiable$year)],
                   ww_inc$condition[ww_inc$year %in% unique(fiveyr_notifiable$year)],
                   sum),
    pop = ww_pop_sum5,
    inc = count / pop * 100000,
    lower = qchisq(0.025, 2 * count) / (2 * pop) * 100000,
    upper = qchisq(0.975, 2 * (count + 1)) / (2 * pop) * 100000)

### 5. Washington IR by condition, historic ###
wa_historic_inc <- data.frame(
  condition = unique(wa_inc$condition)
)

wa_historic_inc <- wa_historic_inc |>
  arrange(condition) |>
  mutate(
    count = tapply(wa_inc$count, wa_inc$condition, sum),
    pop = sum(pop$washington),
    inc = count / pop * 100000,
    lower = qchisq(0.025, 2 * count) / (2 * pop) * 100000,
    upper = qchisq(0.975, 2 * (count + 1)) / (2 * pop) * 100000)

### 6. Washington IR by condition, last 5 years ###
wa_fiveyr_inc <- data.frame(
  condition = unique(wa_inc$condition)
)

wa_fiveyr_inc <- wa_fiveyr_inc |>
  arrange(condition) |>
  mutate(
    count = tapply(wa_inc$count[wa_inc$year %in% unique(fiveyr_notifiable$year)],
                   wa_inc$condition[wa_inc$year %in% unique(fiveyr_notifiable$year)],
                   sum),
    pop = wa_pop_sum5,
    inc = count / pop * 100000,
    lower = qchisq(0.025, 2 * count) / (2 * pop) * 100000,
    upper = qchisq(0.975, 2 * (count + 1)) / (2 * pop) * 100000)

### 7. Incidence Rate Ratios (IRR/RR) WW vs. WA Historic ###
irr_table_historic <- data.frame(
  condition = wa_historic_inc$condition
)

irr_table_historic <- irr_table_historic |>
  mutate(
    ww_ir = ww_historic_inc$inc,
    ww_count = ww_historic_inc$count,
    wa_ir = wa_historic_inc$inc,
    wa_count = wa_historic_inc$count,
    irr = ww_ir / wa_ir,
    lower = exp(log(irr) - (1.96 * (sqrt(1 / ww_historic_inc$count + 1 / wa_historic_inc$count)))),
    upper = exp(log(irr) + (1.96 * (sqrt(1 / ww_historic_inc$count + 1 / wa_historic_inc$count))))
  ) |>
  filter(
    ww_count > 5 & wa_count > 5
  )

### 8. Incidence Rate Ratios (IRR/RR) WW vs. WA, last five years ###
irr_table_fiveyr <- data.frame(
  condition = wa_fiveyr_inc$condition
)

irr_table_fiveyr <- irr_table_fiveyr |>
  mutate(
    ww_ir = ww_fiveyr_inc$inc,
    ww_count = ww_historic_inc$count,
    wa_ir = wa_fiveyr_inc$inc,
    wa_count = wa_historic_inc$count,
    irr = ww_ir / wa_ir,
    lower = exp(log(irr) - (1.96 * (sqrt(1 / ww_fiveyr_inc$count + 1 / wa_fiveyr_inc$count)))),
    upper = exp(log(irr) + (1.96 * (sqrt(1 / ww_fiveyr_inc$count + 1 / wa_fiveyr_inc$count))))
  ) |>
  filter(
    ww_count > 5 & wa_count > 5
  )

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
#These are lists of regression models over the entire historical database and
#the most recent five years, respectively. They are of little use on their own
#since they are not named with the specific conditions

#### Largest Increases and Decreases ####
#We then use previously calculated regression coefficients to determine which 
#conditions have seen the largest increases or decreases since the beginning of
#data collection as well as the last five years.

#Extract slopes and r-squared values from our models
model_summary <- map2_df(models, names(models), ~ {
  coef <- tidy(.x)
  slope <- coef |> filter(term == "year") |> pull(estimate)
  r2 <- summary(.x)$r.squared
  intercept <- coef |> filter(term == "(Intercept)") |> pull(estimate)
  tibble(var = .y, slope = slope, r_squared = r2, intercept = intercept)
})

fiveyr_model_summary <- map2_df(fiveyr_models, names(fiveyr_models), ~ {
  coef <- tidy(.x)
  slope <- coef |> filter(term == "year") |> pull(estimate)
  r2 <- summary(.x)$r.squared
  intercept <- coef |> filter(term == "(Intercept)") |> pull(estimate)
  tibble(var = .y, slope = slope, r_squared = r2, intercept = intercept)
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
    values_from = c(slope, r_squared, intercept),
    names_glue = "{type}_{.value}"
  ) |>
  rename(
    slope = ww_slope,
    slope_r2 = ww_r_squared,
    slope_intercept = ww_intercept,
    wa_slope = wa_slope,
    wa_slope_r2 = wa_r_squared,
    wa_slope_intercept = wa_intercept
  ) |>
  mutate(slope_diff = slope - wa_slope)

fiveyr_summary_wide <- fiveyr_model_summary |>
  pivot_wider(
    id_cols = index,
    names_from = type,
    values_from = c(slope, r_squared, intercept),
    names_glue = "{type}_{.value}"
  ) |>
  rename(
    slope = ww_slope,
    slope_r2 = ww_r_squared,
    slope_intercept = ww_intercept,
    wa_slope = wa_slope,
    wa_slope_r2 = wa_r_squared,
    wa_slope_intercept = wa_intercept
  ) |>
  mutate(slope_diff = slope - wa_slope)

#Joining with con_index
slope_table_full <- con_index |>
  mutate(index = row_number()) |>
  left_join(summary_wide, by = "index") |>
  select(condition, slope, slope_r2, slope_intercept, wa_slope, wa_slope_r2, 
         wa_slope_intercept, slope_diff)

fiveyr_slope_table_full <- con_index |>
  mutate(index = row_number()) |>
  left_join(fiveyr_summary_wide, by = "index") |>
  select(condition, slope, slope_r2, slope_intercept, wa_slope, wa_slope_r2, 
         wa_slope_intercept,
         slope_diff)

#Let's remove the many rare conditions for which WW county has no data and therefore
#a slope of zero, at least for this analysis.

slope_table <- slope_table_full |>
  filter(slope != 0) |>
  arrange(desc(slope_diff))

fiveyr_slope_table <- fiveyr_slope_table_full |>
  filter(slope != 0) |>
  arrange(desc(slope_diff))

#These two tiblles are arranged by slope_diff, a variable storing the difference
#between Walla Walla Co. incidence and WA State incidence. A positive number
#indicates incidence in WW is rising faster than in WA, while a negative number
#indicates incidence in WW is falling faster than in WA. Importantly, this doesn't
#directly correspond with the incidence being higher or lower overall, just slope
#or change in incidence over time. The next section will calculate a historic IR
#over the entire time period.

#### STIs and Enteric Illness Analyses ####
#We explore who holds the burden of disease for STIs and enteric illnesses based
#on race/ethnicity, sex, and age. We also determine whether this has shifted over
#time.

### Enteric ###
## 1. Enteric by sex ##
raw_summary_enteric_sex <- enteric |> 
  filter(condition != "STEC") |>
  droplevels() |>
  tbl_cross(row = sex, col = condition, 
            percent = "column",
            missing = "no",
            label = sex ~ "Sex") |> 
  modify_header(stat_by = "**{level}**", stat_0 = "Total") |> 
  modify_spanning_header(starts_with("stat_") ~ "**Enteric illness**") |>
  modify_fmt_fun(
    all_stat_cols() ~ function(x) {
      stringr::str_extract(x, "\\d+\\.?\\d*%")
    }
  )

enteric_sex <- raw_summary_enteric_sex |> as_flex_table()

## 2. Enteric by race ##
raw_summary_enteric_race <- enteric |>
  tbl_cross(row = race_1, col = condition,
            percent = "column",
            missing = "no",
            label = race_1 ~ "Race") |>
  modify_header(stat_by = "**{level}**", stat_0 = "Total") |>
  modify_spanning_header(starts_with("stat_") ~ "**Enteric illness**") |>
  modify_fmt_fun(
    all_stat_cols() ~ function(x) {
      stringr::str_extract(x, "\\d+\\.?\\d*%")
    }
  )

enteric_race <- raw_summary_enteric_race |> as_flex_table()

## 3. Enteric by ethnicity ##
raw_summary_enteric_eth <- enteric |>
  tbl_cross(row = hispanic, col = condition,
            percent = "column",
            missing = "no",
            label = hispanic ~ "Ethnicity") |>
  modify_header(stat_by = "**{level}**", stat_0 = "Total") |>
  modify_spanning_header(starts_with("stat_") ~ "**Enteric illness**") |>
  modify_fmt_fun(
    all_stat_cols() ~ function(x) {
      stringr::str_extract(x, "\\d+\\.?\\d*%")
    }
  )

enteric_eth <- raw_summary_enteric_eth |> as_flex_table()

## 4. Enteric by age ##
raw_summary_enteric_age <- enteric |> 
  tbl_cross(row = agecat, col = condition,
            percent = "column",
            missing = "no",
            label = agecat ~ "Age category") |>
  modify_header(stat_by = "**{level}**", stat_0 = "Total") |>
  modify_spanning_header(starts_with("stat_") ~ "**Enteric illness**") |>
  modify_fmt_fun(
    all_stat_cols() ~ function(x) {
      stringr::str_extract(x, "\\d+\\.?\\d*%")
    }
  )

enteric_age <- raw_summary_enteric_age |> as_flex_table()

### STIs ###

### Combined relative proportions ###
## HISTORIC ##
historic_relprop <- data.frame(
  condition = c("Campylobacteriosis",
                "Salmonellosis",
                "Shigellosis",
                "Shiga toxin-producing E. coli ",
                "Chlamydia",
                "Gonorrhea",
                "Hepatitis C, All",
                "HIV, prevalent HIV cases")
)

# Necessary since STEC is messy in original excel file
clean_condition <- function(x) {
  x <- gsub("\u00A0", " ", x, fixed = TRUE)  # Replace non-breaking space with regular space
  x <- trimws(x)                             # Trim leading/trailing whitespace
  x <- gsub("\\s+", " ", x)                  # Collapse multiple spaces
  return(x)
}

# Clean both versions of condition before joining
historic_relprop$condition <- clean_condition(historic_relprop$condition)
ww_cases$condition <- clean_condition(ww_cases$condition)

ww_cases_subset <- ww_cases |>
  select(condition, total, total5) |>
  mutate(condition = trimws(condition)) |>
  semi_join(historic_relprop |> mutate(condition = trimws(condition)), by = "condition")

ww_cases_subset$condition <- c("Campylobacteriosis",
                               "Chlamydia",
                               "Gonorrhea",
                               "Hepatitis C, All",
                               "HIV, prevalent HIV cases",
                               "Salmonellosis",
                               "STEC",
                               "Shigellosis")

hist_enteric_age_summary <- enteric |>
  count(condition, agecat) |>
  pivot_wider(names_from = agecat, values_from = n, values_fill = 0)

historic_relprop <- ww_cases_subset |>
  left_join(hist_enteric_age_summary, by = "condition")

relprop_chlam_age <- sapply(sti_sets$chlamydia_age[-1], sum)
relprop_chlam_age[9] <- sum(tail(relprop_chlam_age, -8))
relprop_chlam_age <- head(relprop_chlam_age, 9)
historic_relprop[2, -c(1:3, 13)] <- as.list(relprop_chlam_age)
historic_relprop[2, ] <- c(historic_relprop[2, -13], 0)

relprop_gonn_age <- sapply(sti_sets$gonorrhea_age[-1], sum)
relprop_gonn_age[9] <- sum(tail(relprop_gonn_age, -8))
relprop_gonn_age <- head(relprop_gonn_age, 9)
historic_relprop[3, -c(1:3, 13)] <- as.list(relprop_gonn_age)
historic_relprop[3, ] <- c(historic_relprop[3, -13], 0)

relprop_hcv_age <- sapply(sti_sets$hcv_age[-1], sum)
relprop_hcv_age[9] <- sum(tail(relprop_hcv_age, -8))
relprop_hcv_age <- head(relprop_hcv_age, 9)
historic_relprop[4, -c(1:3, 13)] <- as.list(relprop_hcv_age)
historic_relprop[4, ] <- c(historic_relprop[4, -13], 0)

relprop_hiv_age <- sapply(sti_sets$hiv_age[-1], sum)
relprop_hiv_age <- c(0, relprop_hiv_age)
historic_relprop[5, -c(1:3, 13)] <- as.list(relprop_hiv_age)
historic_relprop[5, ] <- c(historic_relprop[5, -13], 0)

historic_relprop <- historic_relprop |>
  select(-`NA`)

hist_enteric_sex_summary <- enteric |>
  count(condition, sex) |>
  pivot_wider(names_from = sex, values_from = n, values_fill = 0)

historic_relprop <- historic_relprop |>
  left_join(hist_enteric_sex_summary, by = "condition")

historic_relprop <- historic_relprop |>
  select(-`NA`)

relprop_chlam_sex <- sapply(sti_sets$chlamydia_sex[ , -c(1, 4)], sum)
relprop_chlam_sex <- c(relprop_chlam_sex[2], relprop_chlam_sex[1])
historic_relprop[2, c(13:14)] <- as.list(relprop_chlam_sex)

relprop_gonn_sex <- sapply(sti_sets$gonorrhea_sex[ , -c(1, 4)], sum)
relprop_gonn_sex <- c(relprop_gonn_sex[2], relprop_gonn_sex[1])
historic_relprop[3, c(13:14)] <- as.list(relprop_gonn_sex)

relprop_hcv_sex <- sapply(sti_sets$hcv_sex[ , -c(1)], sum)
relprop_hcv_sex <- c(relprop_hcv_sex[2], relprop_hcv_sex[1])
historic_relprop[4, c(13:14)] <- as.list(relprop_hcv_sex)

relprop_hiv_sex <- sapply(sti_sets$hiv_sex[ , -c(1)], sum)
relprop_hiv_sex <- c(relprop_hiv_sex[2], relprop_hiv_sex[1])
historic_relprop[5, c(13:14)] <- as.list(relprop_hiv_sex)

historic_relprop[7, c(13:14)] <- NA #Make sure missingness noted for STEC sex data

hist_enteric_race_summary <- enteric |>
  count(condition, race_1) |>
  pivot_wider(names_from = race_1, values_from = n, values_fill = 0) |>
  select(condition, 
         White,
         `Black or African American`,
         `American Indian or Alska Native`,
         Asian,
         `Native Hawaiian or Pacific Islander`,
         Other,
         `NA`)

historic_relprop <- historic_relprop |>
  left_join(hist_enteric_race_summary, by = "condition")

relprop_chlam_race <- sapply(sti_sets$chlamydia_race[ , -c(1)], sum)
relprop_chlam_race <- c(relprop_chlam_race[8], 
                       relprop_chlam_race[3],
                       relprop_chlam_race[1], 
                       relprop_chlam_race[2],
                       relprop_chlam_race[6], 
                       relprop_chlam_race[7], 
                       relprop_chlam_race[4],
                       relprop_chlam_race[5])
relprop_chlam_race[6] <- sum(tail(relprop_chlam_race, -5))
relprop_chlam_race <- head(relprop_chlam_race, 6)
historic_relprop[2, c(15:20)] <- as.list(relprop_chlam_race)

relprop_gonn_race <- sapply(sti_sets$gonorrhea_race[ , -c(1)], sum)
relprop_gonn_race <- c(relprop_gonn_race[8], 
                        relprop_gonn_race[3],
                        relprop_gonn_race[1], 
                        relprop_gonn_race[2],
                        relprop_gonn_race[6], 
                        relprop_gonn_race[7], 
                        relprop_gonn_race[4],
                        relprop_gonn_race[5])
relprop_gonn_race[6] <- sum(tail(relprop_gonn_race, -5))
relprop_gonn_race <- head(relprop_gonn_race, 6)
historic_relprop[3, c(15:20)] <- as.list(relprop_gonn_race)

relprop_hcv_race <- sapply(sti_sets$hcv_race[ , -c(1)], sum)
relprop_hcv_race <- c(relprop_hcv_race[5], 
                       relprop_hcv_race[2],
                       relprop_hcv_race[1],
                      asian = NA,
                       relprop_hcv_race[4],
                       relprop_hcv_race[3], 
                       relprop_hcv_race[6], 
                       relprop_hcv_race[7])
relprop_hcv_race[6] <- sum(tail(relprop_hcv_race, -5))
relprop_hcv_race <- head(relprop_hcv_race, 6)
historic_relprop[4, c(15:20)] <- as.list(relprop_hcv_race)

relprop_hiv_race <- sapply(sti_sets$hiv_race[ , -c(1)], sum)
relprop_hiv_race <- c(relprop_hiv_race[6], 
                      relprop_hiv_race[3],
                      relprop_hiv_race[1],
                      relprop_hiv_race[2],
                      nhpi = NA,
                      relprop_hiv_race[4],
                      relprop_hiv_race[5])
relprop_hiv_race[6] <- sum(tail(relprop_hiv_race, -5))
relprop_hiv_race <- head(relprop_hcv_race, 6)
historic_relprop[5, c(15:20)] <- as.list(relprop_hiv_race)

historic_relprop <- historic_relprop |>
  select(-`NA`)

hist_enteric_eth_summary <- enteric |>
  count(condition, hispanic) |>
  pivot_wider(names_from = hispanic, values_from = n, values_fill = 0) |>
  select(condition,
         `Hispanic or Latino/a`)

historic_relprop <- historic_relprop |>
  left_join(hist_enteric_eth_summary, by = "condition")

relprop_chlam_eth <- sapply(sti_sets$chlamydia_race[ , 5], sum)
historic_relprop[2, 21] <- as.list(relprop_chlam_eth)

relprop_gonn_eth <- sapply(sti_sets$gonorrhea_race[ , 5], sum)
historic_relprop[3, 21] <- as.list(relprop_gonn_eth)

relprop_hcv_eth <- sapply(sti_sets$hcv_race[ , 4], sum)
historic_relprop[4, 21] <- as.list(relprop_hcv_eth)

relprop_hiv_eth <- sapply(sti_sets$hiv_race[ , 5], sum)
historic_relprop[5, 21] <- as.list(relprop_hiv_eth)

historic_relprop <- historic_relprop |>
  select(-total5)

historic_relprop <- historic_relprop |>
  mutate(across(
    .cols = -c("condition", "total"),
    .fns = ~ . / total * 100
  ))

historic_relprop <- historic_relprop |>
  mutate(across(where(is.numeric), ~ round(., 2)))


names(ww_relprop) <- names(historic_relprop)

historic_relprop <- rbind(historic_relprop, ww_relprop) |>
  filter(condition != "Last five years")

historic_relprop[9, 1] <- "Walla Walla"

names(historic_relprop) <- tosnake(names(historic_relprop))

histrelprop_long <- historic_relprop |>
  pivot_longer(
    cols = -c(condition, total),
    names_to = "group",
    values_to = "percent"
  )

histrelprop_long <- histrelprop_long |>
  mutate(
    demo_type = case_when(
      str_detect(group, "^\\d+_to_\\d+$|^80$") ~ "Age",
      group %in% c("male", "female") ~ "Sex",
      group %in% c(
        "white", 
        "black_or_african_american", 
        "american_indian_or_alska_native", 
        "asian", 
        "native_hawaiian_or_pacific_islander", 
        "other"
      ) ~ "Race",
      group == "hispanic_or_latino_a" ~ "Ethnicity",
      TRUE ~ "Other"
    )
  ) |>
  mutate(
    group_label = case_when(
      group == "80" ~ "80+",
      group == "black_or_african_american" ~ "Black or African American",
      group == "american_indian_or_alska_native" ~ "AI/AN",
      group == "native_hawaiian_or_pacific_islander" ~ "NH/PI",
      group == "hispanic_or_latino_a" ~ "Hispanic or Latino/a",
      TRUE ~ str_replace_all(group, "_", " ") |> str_to_title()
    )
  )

pop_row <- histrelprop_long |>
  filter(condition == "Walla Walla") |>
  select(group, demo_type, population_percent = percent)

histrelprop_long <- histrelprop_long |>
  filter(condition != "Walla Walla") |>
  left_join(pop_row, by = c("group", "demo_type"))

## FIVE YEAR ##
fiveyr_relprop <- data.frame(
  condition = c("Campylobacteriosis",
                "Salmonellosis",
                "Shigellosis",
                "Shiga toxin-producing E. coli ",
                "Chlamydia",
                "Gonorrhea",
                "Hepatitis C, All",
                "HIV, prevalent HIV cases")
)

# Necessary since STEC is messy in original excel file
clean_condition <- function(x) {
  x <- gsub("\u00A0", " ", x, fixed = TRUE)  # Replace non-breaking space with regular space
  x <- trimws(x)                             # Trim leading/trailing whitespace
  x <- gsub("\\s+", " ", x)                  # Collapse multiple spaces
  return(x)
}

# Clean both versions of condition before joining
fiveyr_relprop$condition <- clean_condition(fiveyr_relprop$condition)
ww_cases$condition <- clean_condition(ww_cases$condition)

fiveyr_ww_cases_subset <- ww_cases |>
  select(condition, total5) |>
  mutate(condition = trimws(condition)) |>
  semi_join(fiveyr_relprop |> mutate(condition = trimws(condition)), by = "condition")

fiveyr_ww_cases_subset$condition <- c("Campylobacteriosis",
                               "Chlamydia",
                               "Gonorrhea",
                               "Hepatitis C, All",
                               "HIV, prevalent HIV cases",
                               "Salmonellosis",
                               "STEC",
                               "Shigellosis")

lastfiveyrs <- tail(unique(enteric$year), 5)

fiveyr_enteric_age_summary <- enteric |>
  filter(
    year %in% lastfiveyrs
  ) |>
  count(condition, agecat) |>
  pivot_wider(names_from = agecat, values_from = n, values_fill = 0)

fiveyr_relprop <- ww_cases_subset |>
  left_join(fiveyr_enteric_age_summary, by = "condition")

fiveyr_relprop_chlam_age <- sapply(tail(sti_sets$chlamydia_age[-1], 5), sum)
fiveyr_relprop_chlam_age[9] <- sum(tail(fiveyr_relprop_chlam_age, -8))
fiveyr_relprop_chlam_age <- head(fiveyr_relprop_chlam_age, 9)
fiveyr_relprop[2, -c(1:3)] <- as.list(fiveyr_relprop_chlam_age)

fiveyr_relprop_gonn_age <- sapply(tail(sti_sets$gonorrhea_age[-1], 5), sum)
fiveyr_relprop_gonn_age[9] <- sum(tail(fiveyr_relprop_gonn_age, -8))
fiveyr_relprop_gonn_age <- head(fiveyr_relprop_gonn_age, 9)
fiveyr_relprop[3, -c(1:3)] <- as.list(fiveyr_relprop_gonn_age)

fiveyr_relprop_hcv_age <- sapply(tail(sti_sets$hcv_age[-1], 5), sum)
fiveyr_relprop_hcv_age[9] <- sum(tail(fiveyr_relprop_hcv_age, -8))
fiveyr_relprop_hcv_age <- head(fiveyr_relprop_hcv_age, 9)
fiveyr_relprop[4, -c(1:3)] <- as.list(fiveyr_relprop_hcv_age)

fiveyr_relprop_hiv_age <- sapply(tail(sti_sets$hiv_age[-1], 5), sum)
fiveyr_relprop_hiv_age <- c(0, fiveyr_relprop_hiv_age)
fiveyr_relprop[5, -c(1:3)] <- as.list(fiveyr_relprop_hiv_age)

fiveyr_enteric_sex_summary <- enteric |>
  filter(
    year %in% lastfiveyrs
  ) |>
  count(condition, sex) |>
  pivot_wider(names_from = sex, values_from = n, values_fill = 0)

fiveyr_relprop <- fiveyr_relprop |>
  left_join(fiveyr_enteric_sex_summary, by = "condition")

fiveyr_relprop <- fiveyr_relprop |>
  select(-`NA`)

fiveyr_relprop_chlam_sex <- sapply(tail(sti_sets$chlamydia_sex[ , -c(1, 4)], 5), sum)
fiveyr_relprop_chlam_sex <- c(fiveyr_relprop_chlam_sex[2], fiveyr_relprop_chlam_sex[1])
fiveyr_relprop[2, c(13:14)] <- as.list(fiveyr_relprop_chlam_sex)

fiveyr_relprop_gonn_sex <- sapply(tail(sti_sets$gonorrhea_sex[ , -c(1, 4)], 5), sum)
fiveyr_relprop_gonn_sex <- c(relprop_gonn_sex[2], relprop_gonn_sex[1])
fiveyr_relprop[3, c(13:14)] <- as.list(fiveyr_relprop_gonn_sex)

fiveyr_relprop_hcv_sex <- sapply(tail(sti_sets$hcv_sex[ , -c(1)], 5), sum)
fiveyr_relprop_hcv_sex <- c(fiveyr_relprop_hcv_sex[2], fiveyr_relprop_hcv_sex[1])
fiveyr_relprop[4, c(13:14)] <- as.list(fiveyr_relprop_hcv_sex)

fiveyr_relprop_hiv_sex <- sapply(tail(sti_sets$hiv_sex[ , -c(1)], 5), sum)
fiveyr_relprop_hiv_sex <- c(fiveyr_relprop_hiv_sex[2], fiveyr_relprop_hiv_sex[1])
fiveyr_relprop[5, c(13:14)] <- as.list(fiveyr_relprop_hiv_sex)

fiveyr_relprop[7, c(13:14)] <- NA #Make sure missingness noted for STEC sex data

expected_race_levels <- c(
  "White",
  "Black or African American",
  "American Indian or Alska Native",
  "Asian",
  "Native Hawaiian or Pacific Islander",
  "Other",
  "NA"
)

fiveyr_enteric_race_summary <- enteric |>
  filter(year %in% lastfiveyrs) |>
  count(condition, race_1) |>
  pivot_wider(
    names_from = race_1,
    values_from = n,
    values_fill = 0
  ) |>
  (\(df) {
    missing_cols <- setdiff(expected_race_levels, names(df))
    if (length(missing_cols) > 0) {
      df[missing_cols] <- 0
    }
    df |> select(condition, all_of(expected_race_levels))
  })()


fiveyr_relprop <- fiveyr_relprop |>
  left_join(fiveyr_enteric_race_summary, by = "condition")

fiveyr_relprop_chlam_race <- sapply(tail(sti_sets$chlamydia_race[ , -c(1)], 5), sum)
fiveyr_relprop_chlam_race <- c(fiveyr_relprop_chlam_race[8], 
                               fiveyr_relprop_chlam_race[3],
                               fiveyr_relprop_chlam_race[1], 
                               fiveyr_relprop_chlam_race[2],
                               fiveyr_relprop_chlam_race[6], 
                               fiveyr_relprop_chlam_race[7], 
                               fiveyr_relprop_chlam_race[4],
                               fiveyr_relprop_chlam_race[5])
fiveyr_relprop_chlam_race[6] <- sum(tail(fiveyr_relprop_chlam_race, -5))
fiveyr_relprop_chlam_race <- head(fiveyr_relprop_chlam_race, 6)
fiveyr_relprop[2, c(15:20)] <- as.list(fiveyr_relprop_chlam_race)

fiveyr_relprop_gonn_race <- sapply(tail(sti_sets$gonorrhea_race[ , -c(1)], 5), sum)
fiveyr_relprop_gonn_race <- c(fiveyr_relprop_gonn_race[8], 
                              fiveyr_relprop_gonn_race[3],
                              fiveyr_relprop_gonn_race[1], 
                              fiveyr_relprop_gonn_race[2],
                              fiveyr_relprop_gonn_race[6], 
                              fiveyr_relprop_gonn_race[7], 
                              fiveyr_relprop_gonn_race[4],
                              fiveyr_relprop_gonn_race[5])
fiveyr_relprop_gonn_race[6] <- sum(tail(fiveyr_relprop_gonn_race, -5))
fiveyr_relprop_gonn_race <- head(fiveyr_relprop_gonn_race, 6)
fiveyr_relprop[3, c(15:20)] <- as.list(fiveyr_relprop_gonn_race)

fiveyr_relprop_hcv_race <- sapply(tail(sti_sets$hcv_race[ , -c(1)], 5), sum)
fiveyr_relprop_hcv_race <- c(fiveyr_relprop_hcv_race[5], 
                             fiveyr_relprop_hcv_race[2],
                             fiveyr_relprop_hcv_race[1],
                             asian = NA,
                             fiveyr_relprop_hcv_race[4],
                             fiveyr_relprop_hcv_race[3], 
                             fiveyr_relprop_hcv_race[6], 
                             fiveyr_relprop_hcv_race[7])
fiveyr_relprop_hcv_race[6] <- sum(tail(fiveyr_relprop_hcv_race, -5))
fiveyr_relprop_hcv_race <- head(fiveyr_relprop_hcv_race, 6)
fiveyr_relprop[4, c(15:20)] <- as.list(fiveyr_relprop_hcv_race)

fiveyr_relprop_hiv_race <- sapply(tail(sti_sets$hiv_race[ , -c(1)], 5), sum)
fiveyr_relprop_hiv_race <- c(fiveyr_relprop_hiv_race[6], 
                             fiveyr_relprop_hiv_race[3],
                             fiveyr_relprop_hiv_race[1],
                             fiveyr_relprop_hiv_race[2],
                             nhpi = NA,
                             fiveyr_relprop_hiv_race[4],
                             fiveyr_relprop_hiv_race[5])
fiveyr_relprop_hiv_race[6] <- sum(tail(fiveyr_relprop_hiv_race, -5))
fiveyr_relprop_hiv_race <- head(fiveyr_relprop_hcv_race, 6)
fiveyr_relprop[5, c(15:20)] <- as.list(fiveyr_relprop_hiv_race)

fiveyr_relprop <- fiveyr_relprop |>
  select(-`NA`)

fiveyr_enteric_eth_summary <- enteric |>
  filter(
    year %in% lastfiveyrs
  ) |>
  count(condition, hispanic) |>
  pivot_wider(names_from = hispanic, values_from = n, values_fill = 0) |>
  select(condition,
         `Hispanic or Latino/a`)

fiveyr_relprop <- fiveyr_relprop |>
  left_join(fiveyr_enteric_eth_summary, by = "condition")

fiveyr_relprop_chlam_eth <- sapply(tail(sti_sets$chlamydia_race[ , 5], 5), sum)
fiveyr_relprop[2, 21] <- as.list(fiveyr_relprop_chlam_eth)

fiveyr_relprop_gonn_eth <- sapply(tail(sti_sets$gonorrhea_race[ , 5], 5), sum)
fiveyr_relprop[3, 21] <- as.list(fiveyr_relprop_gonn_eth)

fiveyr_relprop_hcv_eth <- sapply(tail(sti_sets$hcv_race[ , 4], 5), sum)
fiveyr_relprop[4, 21] <- as.list(fiveyr_relprop_hcv_eth)

fiveyr_relprop_hiv_eth <- sapply(tail(sti_sets$hiv_race[ , 5], 5), sum)
fiveyr_relprop[5, 21] <- as.list(fiveyr_relprop_hiv_eth)

fiveyr_relprop <- fiveyr_relprop |>
  select(-total)

fiveyr_relprop <- fiveyr_relprop |>
  mutate(across(
    .cols = -c("condition", "total5"),
    .fns = ~ . / total5 * 100
  ))

fiveyr_relprop <- fiveyr_relprop |>
  mutate(across(where(is.numeric), ~ round(., 2)))

names(ww_relprop) <- names(fiveyr_relprop)

fiveyr_relprop <- rbind(fiveyr_relprop, ww_relprop) |>
  filter(condition != "Historic")

fiveyr_relprop[9, 1] <- "Walla Walla"

names(fiveyr_relprop) <- tosnake(names(fiveyr_relprop))

fiveyr_relprop_long <- fiveyr_relprop |>
  pivot_longer(
    cols = -c(condition, total5),
    names_to = "group",
    values_to = "percent"
  )

fiveyr_relprop_long <- fiveyr_relprop_long |>
  mutate(
    demo_type = case_when(
      str_detect(group, "^\\d+_to_\\d+$|^80$") ~ "Age",
      group %in% c("male", "female") ~ "Sex",
      group %in% c(
        "white", 
        "black_or_african_american", 
        "american_indian_or_alska_native", 
        "asian", 
        "native_hawaiian_or_pacific_islander", 
        "other"
      ) ~ "Race",
      group == "hispanic_or_latino_a" ~ "Ethnicity",
      TRUE ~ "Other"
    )
  ) |>
  mutate(
    group_label = case_when(
      group == "80" ~ "80+",
      group == "black_or_african_american" ~ "Black or African American",
      group == "american_indian_or_alska_native" ~ "AI/AN",
      group == "native_hawaiian_or_pacific_islander" ~ "NH/PI",
      group == "hispanic_or_latino_a" ~ "Hispanic or Latino/a",
      TRUE ~ str_replace_all(group, "_", " ") |> str_to_title()
    )
  )

fiveyr_pop_row <- fiveyr_relprop_long |>
  filter(condition == "Walla Walla") |>
  select(group, demo_type, population_percent = percent)

fiveyr_relprop_long <- fiveyr_relprop_long |>
  filter(condition != "Walla Walla") |>
  left_join(fiveyr_pop_row, by = c("group", "demo_type"))

#### Save outputs and df's needed for visualization ####
saveRDS(ww_historic_inc, here("data", "output", "ww_historic_inc.rds"))
saveRDS(wa_historic_inc, here("data", "output", "wa_historic_inc.rds"))
saveRDS(ww_fiveyr_inc, here("data", "output", "ww_fiveyr_inc.rds"))
saveRDS(wa_fiveyr_inc, here("data", "output", "wa_fiveyr_inc.rds"))
saveRDS(ww_inc, here("data", "output", "ww_inc.rds"))
saveRDS(wa_inc, here("data", "output", "wa_inc.rds"))
saveRDS(slope_table, here("data", "output", "slope_table.rds"))
saveRDS(fiveyr_slope_table, here("data", "output", "fiveyr_slope_table.rds"))
saveRDS(irr_table_historic, here("data", "output", "irr_table_historic.rds"))
saveRDS(irr_table_fiveyr, here("data", "output", "irr_table_fiveyr.rds"))
saveRDS(slope_table, here("data", "output", "slope_table.rds"))
saveRDS(fiveyr_slope_table, here("data", "output", "fiveyr_slope_table.rds"))
saveRDS(histrelprop_long, here("data", "output", "histrelprop_long.rds"))
saveRDS(fiveyr_relprop_long, here("data", "output", "fiveyrrelprop_long.rds"))
saveRDS(enteric_sex, here("data", "output", "enteric_sex.rds"))
saveRDS(enteric_race, here("data", "output", "enteric_race.rds"))
saveRDS(enteric_eth, here("data", "output", "enteric_eth.rds"))
saveRDS(enteric_age, here("data", "output", "enteric_age.rds"))
