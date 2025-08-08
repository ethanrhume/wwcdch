#### Setup ####
rm(list = ls())

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(here, 
               tidyverse,
               purrr) #Loads necessary packages + installs if needed

here::i_am("scripts/02_transform.R") #Ensures `here` properly IDs top-level directory

source(here("R", "custom_functions.R")) #Adds custom functions to environment


#### Read in data ####
dat <- readRDS(here("data", "output", "dat_clean.rds"))
wa_cases <- readRDS(here("data", "output", "dat_wa_cases.rds"))
ww_cases <- readRDS(here("data", "output", "dat_ww_cases.rds"))
enteric <- readRDS(here("data", "output", "enteric.rds"))
wa_dat <- readRDS(here("data", "output", "wa_dat.rds"))
ww_dat <- readRDS(here("data", "output", "ww_dat.rds"))
sade <- readRDS(here("data", "output", "sade.rds"))

sti_sets_helper <- list.files(path = here("data", "output", "sti_sets"), 
                              pattern = "\\.rds$", full.names = TRUE)
sti_sets <- lapply(sti_sets_helper, readRDS)
names(sti_sets) <- tools::file_path_sans_ext(basename(sti_sets_helper))

#### Transformation ####
### 1. wa_cases and ww_cases ###
#This is important for calculating IRs in the next script:
wa_cases$total <- rowSums(wa_cases[ , -1], na.rm = TRUE)
wa_cases$total5 <- rowSums(wa_cases[ , (ncol(wa_cases)-5):ncol(wa_cases)-1], 
                           na.rm = TRUE)

ww_cases$total <- rowSums(ww_cases[ , -1], na.rm = TRUE)
ww_cases$total5 <- rowSums(ww_cases[ , (ncol(ww_cases)-5):ncol(ww_cases)-1], 
                           na.rm = TRUE)

### 2. notifiable ###
#We want this dataframe to be X observations of Y variables, where X is the number
#of years included in analysis. Y is notifiable conditions with extant data
#times 2 (for both cases and rates).

year <- c(2010:2023) #Adjust for years to include in analysis

notifiable <- data.frame(year) #Initialize new dataframe

con_length <- length(dat$condition)
con_names <- unique(dat$condition)

for (i in seq_along(con_names)) { #Linewise output examples for 'Anthrax' provided
  cond <- con_names[i] #Anthrax
  rate_col <- paste0("rate_", i) #rate_1
  wa_col <- paste0("warate_", i) #warate_1
  
  #Subset dat to the row for this condition
  row <- dat[dat$condition == cond, ] #dat[Anthrax, ] row
  
  # Extract the rate and warate values across years
  rate_values <- sapply(year, function(y) row[[paste0(y, "_rate")]]) 
  wa_values   <- sapply(year, function(y) row[[paste0(y, "_wa_rate")]])
  
  #Add to 'notifiable' dataframe
  notifiable[[rate_col]] <- rate_values
  notifiable[[wa_col]]   <- wa_values
}

### 3. con_index ###
#This is important because notifiable uses numbers instead of condition names in
#the variable names. This will be utilized during 03_analysis.R
con_index <- data.frame(condition = dat$condition)

### 4. sti_sets ###
#A lot of formatting needs to be done with these dfs as we want to perform as 
#many analysis operations as possible in a listwise fashion. Therefore, they need
#to be as standardized as possible.

add_year <- function(x) {
  names(x)[1] <- "year"
  return(x)
}

sti_sets <- sapply(sti_sets, add_year)
sti_sets <- sapply(sti_sets, function(df) {
  names(df) <- sapply(names(df), tosnake)
  df
})

#We need to get rid of NAs and pre-calculated totals which included them
sti_sets[["gonorrhea_age"]] <- sti_sets[["gonorrhea_age"]] |>
  set_names(~ gsub("14", "total", .x))

#Then some small format changes and corrections to standardize our analyses
sti_sets <- lapply(sti_sets, function(df) {
  set_names(df, ~ gsub("^X", "", .x))
})

sti_sets <- lapply(sti_sets, function(df) {
  set_names(df, ~ gsub("^x", "", .x))
})

sti_sets[["hcv_age"]] <- sti_sets[["hcv_age"]] |>
  set_names(~ gsub("^10$", "0_9", .x))

sti_sets[["hiv_sex"]] <- sti_sets[["hiv_sex"]] |>
  set_names(~ gsub("^m$", "male", .x)) |>
  set_names(~ gsub("^f$", "female", .x))

sti_sets[["hiv_race"]] <- sti_sets[["hiv_race"]] |>
  set_names(~ gsub("american_indian_alaska_native", "ai_an", .x)) |>
  set_names(~ gsub("black_african_american", "black", .x)) |>
  set_names(~ gsub("hispanic_latino_all_races", "hispanic", .x)) |>
  set_names(~ gsub("multiracial", "multi", .x))

sti_sets[["hcv_race"]] <- sti_sets[["hcv_race"]] |>
  set_names(~ gsub("american_indian_or_alaska_native", "ai_an", .x)) |>
  set_names(~ gsub("hispanic_latino_a_or_latinx_any_race", "hispanic", .x)) |>
  set_names(~ gsub("native_hawaiian_other_pacific_islander", "nh_pi", .x)) |>
  set_names(~ gsub("multiple_races", "multi", .x))

race_rename <- function(df) {
  nms <- names(df)
  nms <- gsub("aian_non_hispanic", "ai_an", nms)
  nms <- gsub("asian_non_hispanic", "asian", nms)
  nms <- gsub("black_non_hispanic", "black", nms)
  nms <- gsub("multi_non_hispanic", "multi", nms)
  nms <- gsub("nhopi_non_hispanic", "nh_pi", nms)
  nms <- gsub("other_race_non_his", "other", nms)
  nms <- gsub("white_non_hispanic", "white", nms)
  nms <- gsub("unknown_race_non_h", "unknown", nms)
  names(df) <- nms
  df
}

sti_sets[c("chlamydia_race", "gonorrhea_race")] <-
  map(sti_sets[c("chlamydia_race", "gonorrhea_race")], race_rename)

#Remove NAs and totals
sti_sets <- map(sti_sets, ~ select(.x, -any_of(c("total", "unknown", 
                                                 "not_reported", 
                                                 "other_or_unknown"))))

### 5. enteric ###
enteric$age <- as.numeric(as.character(enteric$age))

enteric$sex <- factor(enteric$sex, levels = c(1, 2), 
                      labels = c("Male", "Female"))
enteric$hispanic <- factor(enteric$hispanic, levels = c(0, 1),
                           labels = c("Non-Hispanic", "Hispanic or Latino/a"))
enteric$race_1 <- factor(enteric$race_1, levels = c(1:6), 
                         labels = c("White", "Black or African American", "Asian",
                                    "American Indian or Alska Native", 
                                    "Native Hawaiian or Pacific Islander",
                                    "Other"))
enteric$condition <- as.factor(enteric$condition)

enteric$agecat <- NA
enteric$agecat[enteric$age <= 9] <- 1
enteric$agecat[enteric$age <= 19 & enteric$age >= 10] <- 2
enteric$agecat[enteric$age <= 29 & enteric$age >= 20] <- 3
enteric$agecat[enteric$age <= 39 & enteric$age >= 30] <- 4
enteric$agecat[enteric$age <= 49 & enteric$age >= 40] <- 5
enteric$agecat[enteric$age <= 59 & enteric$age >= 50] <- 6
enteric$agecat[enteric$age <= 69 & enteric$age >= 60] <- 7
enteric$agecat[enteric$age <= 79 & enteric$age >= 70] <- 8
enteric$agecat[enteric$age >= 80] <- 9
enteric$agecat <- factor(enteric$agecat, levels = c(1:9),
                         labels = c("0 to 9", "10 to 19", "20 to 29", "30 to 39", 
                                    "40 to 49", "50 to 59", "60 to 69", "70 to 79", 
                                    "> 80"))

### 6. wa_dat and ww_dat for time-series plots ###
wa_dat <- wa_dat |>
  pivot_longer(
    cols = -all_of("condition"),
    names_to = "year", 
    values_to = "rate"
  )

wa_dat$year <- as.numeric(wa_dat$year)

ww_dat <- ww_dat |>
  pivot_longer(
    cols = -all_of("condition"),
    names_to = "year",
    values_to = "rate"
  )

ww_dat$year <- as.numeric(ww_dat$year)

### 7. Relative proportions of each demographic in WW ###
ww_relprop_all <- sade |>
  filter(Year != 2024) # REMOVE IN SUBSEQUENT ANALYSES
  
ww_relprop_all <- ww_relprop_all |>
  mutate(across(all_of(names(ww_relprop_all)), as.numeric))

to_sum <- ww_relprop_all |>
  select(-c("Year"))

to_sum <- names(to_sum)

historic_relprop_row <- sapply(ww_relprop_all[to_sum], sum)
historic_relprop_row <- c(Year = "Historic", historic_relprop_row)

fiveyr_relprop_row <- sapply(tail(ww_relprop_all[to_sum], 5), sum)
fiveyr_relprop_row <- c(Year = "Last five years", fiveyr_relprop_row)

ww_relprop_all <- rbind(ww_relprop_all, fiveyr_relprop_row, historic_relprop_row)
ww_relprop <- tail(ww_relprop_all, 2) |>
  mutate(across(-"Year", as.numeric))
  
ww_relprop <- ww_relprop |>
  mutate(across(
    .cols = -c("Total", "Year"),
    .fns = ~ . / Total * 100
  ))

ww_relprop <- ww_relprop |>
  mutate(across(where(is.numeric), ~ round(., 2)))

(names(ww_relprop_all))
saveRDS(notifiable, here("data", "output", "notifiable.rds"))
saveRDS(con_index, here("data", "output", "con_index.rds"))
saveRDS(wa_dat, here("data", "output", "wa_dat.rds"))
saveRDS(ww_dat, here("data", "output", "ww_dat.rds"))
saveRDS(wa_cases, here("data", "output", "dat_wa_cases.rds"))
saveRDS(ww_cases, here("data", "output", "dat_ww_cases.rds"))
saveRDS(enteric, here("data", "output", "enteric.rds"))
saveRDS(ww_relprop, here("data", "output", "ww_relprop.rds"))

walk(names(sti_sets), function(name) {
  saveRDS(
    sti_sets[[name]],
    file = here("data", "output", "sti_sets", paste0(name, ".rds"))
  )
})

