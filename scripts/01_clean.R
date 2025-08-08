#### Setup ####
rm(list = ls())

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(here, 
               tidyverse,
               readxl,
               labelled) #Loads necessary packages + installs if needed

here::i_am("scripts/01_clean.R") #Ensures `here` properly IDs top-level directory

source(here("R", "custom_functions.R")) #Adds custom functions to environment

#### Read in data ####
dat <- read_excel(here("data", "raw", "IN-HOUSE Data Storage Notifiable Diseases.xlsx"),
                  sheet = "2010 to Current Year", skip = 3) #Skips top 3 rows

pop <- read_excel(here("data", "raw", "IN-HOUSE Data Storage Notifiable Diseases.xlsx"),
                  sheet = "Population", range = "B4:D18")

chlamydia_age <- read_excel(here("data", "raw", "WDRS and STI data", 
                                 "STI Data request DOH folder", 
                                 "Walla Walla STI Data Through 2023.xlsx"),
                  sheet = " Age", range = "C3:P14")

chlamydia_race <- read_excel(here("data", "raw", "WDRS and STI data", 
                                 "STI Data request DOH folder", 
                                 "Walla Walla STI Data Through 2023.xlsx"),
                            sheet = "Race Eth", range = "B3:K14")

chlamydia_sex <- read_excel(here("data", "raw", "WDRS and STI data", 
                                  "STI Data request DOH folder", 
                                  "Walla Walla STI Data Through 2023.xlsx"),
                             sheet = "Gender", range = "B3:F14")

chlamydia_coinfection <- read_excel(here("data", "raw", "WDRS and STI data", 
                                          "STI Data request DOH folder", 
                                          "Walla Walla STI Data Through 2023.xlsx"),
                                     sheet = "Other", range = "A20:C31")

chlamydia_visit <- read_excel(here("data", "raw", "WDRS and STI data", 
                                   "STI Data request DOH folder", 
                                   "Walla Walla STI Data Through 2023.xlsx"),
                              sheet = "Other", range = "I20:M31")

gonorrhea_age <- read_excel(here("data", "raw", "WDRS and STI data", 
                                 "STI Data request DOH folder", 
                                 "Walla Walla STI Data Through 2023.xlsx"),
                            sheet = " Age", range = "C17:P28")

gonorrhea_race <- read_excel(here("data", "raw", "WDRS and STI data", 
                                  "STI Data request DOH folder", 
                                  "Walla Walla STI Data Through 2023.xlsx"),
                             sheet = "Race Eth", range = "B17:K28")

gonorrhea_sex <- read_excel(here("data", "raw", "WDRS and STI data", 
                                 "STI Data request DOH folder", 
                                 "Walla Walla STI Data Through 2023.xlsx"),
                            sheet = "Gender", range = "B17:F28")

gonorrhea_coinfection <- read_excel(here("data", "raw", "WDRS and STI data", 
                                          "STI Data request DOH folder", 
                                          "Walla Walla STI Data Through 2023.xlsx"),
                                     sheet = "Other", range = "A20:E31")

gonorrhea_visit <- read_excel(here("data", "raw", "WDRS and STI data", 
                                   "STI Data request DOH folder", 
                                   "Walla Walla STI Data Through 2023.xlsx"),
                              sheet = "Other", range = "I20:Q31")

reinfections <- read_excel(here("data", "raw", "WDRS and STI data", 
                                "STI Data request DOH folder", 
                                "Walla Walla STI Data Through 2023.xlsx"),
                           sheet = "Other", range = "Q4:R14", col_names = FALSE)

hiv_prevalence <- read_excel(here("data", "raw", "WDRS and STI data", 
                                  "STI Data request DOH folder", 
                                  "Walla_Walla_HIV_Data Request.xlsx"),
                             sheet = "Prevalence", range = "A3:AJ15")

hcv_age <- read_excel(here("data", "raw", "WDRS and STI data", 
                           "STI Data request DOH folder", 
                           "Walla_Walla_Data_Request_FINAL_HCV.xlsx"),
                      sheet = "Age", range = "A2:N16")

hcv_race <- read_excel(here("data", "raw", "WDRS and STI data", 
                           "STI Data request DOH folder", 
                           "Walla_Walla_Data_Request_FINAL_HCV.xlsx"),
                      sheet = "Race_Ethnicity", range = "A2:I16")

hcv_sex <- read_excel(here("data", "raw", "WDRS and STI data", 
                           "STI Data request DOH folder", 
                           "Walla_Walla_Data_Request_FINAL_HCV.xlsx"),
                      sheet = "Gender", range = "A2:D16")

enteric <- read_excel(here("data", "raw", "enteric_raw.xlsx"), 
                      sheet = "tidy", range = "A1:H417")

sade1 <- read_excel(here("data", "raw", "ofm_pop_sade_county_2010_2019.xlsx"),
                    sheet = "Total")

sade2 <- read_excel(here("data", "raw", "ofm_pop_sade_county_2020_2024.xlsx"),
                    sheet = "Total")

sade3 <- read_excel(here("data", "raw", "ofm_pop_sade_county_2010_2019.xlsx"),
                    sheet = "Hispanic")

sade4 <- read_excel(here("data", "raw", "ofm_pop_sade_county_2020_2024.xlsx"),
                    sheet = "Hispanic")

#### Cleaning ####
### 1. dat ###
#Replace the underscore with NA
dat[dat == "_"] <- NA 

names(dat) <- names(dat) |>
  tosnake()

#We need to differentiate our NAs between true missingness and suppressed rates.
#The 'labelled' package allows tagged NAs to assist with this.
dat <- dat |>
  mutate(
    across(matches("^(2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021|2022)_(wa_)?rate$"), ~ case_when(
      . == "≤ 5" ~ tagged_na("a"),
      . == "≤ 16" ~ tagged_na("a"),
      is.na(.)   ~ NA_real_,
      TRUE       ~ as.numeric(.)
    ))
  )

#Convert all counts and rates to numeric
dat[ , -c(1, 2)] <- lapply(dat[ , -c(1, 2)], as.numeric)

#Construct a vector of conditions with no extant data
unmeasured <- dat |>
  rowwise() |>
  filter(all(is.na(c_across(-c(1,2))))) |>
  pull(condition)


#Construct a vector of all raw case number variables, which won't be needed for
#most analyses. Remove this from one version of 'dat' but keep for a separate 
#version. Conversely, construct a vector of rates to help filter the cases 'dat'
raw_numbers <- names(dat)[grepl("case", names(dat))]
rates <- names(dat)[grepl("rate", names(dat))]

#I am also dropping '24 data as they aren't added to the dataset at time
#of this edit. Simply remove from the select() call below to include in future
#analyses.
dat_clean <- dat |>
  select(-type, -any_of(raw_numbers), -c(`2024_rate`, `2024_wa_rate`)) |>
  filter(!condition %in% unmeasured)

dat_cases <- dat |>
  select(-type, -any_of(rates), -c(`2024_case`, `2024_wa_case`)) |>
  filter(!condition %in% unmeasured)

#We will need to have dat_cases separated into dat_wa_cases and dat_ww_cases
wa_names <- names(dat_cases)[grepl("wa", names(dat_cases))]
dat_wa_cases <- dat_cases |>
  select(condition, any_of(wa_names))

dat_ww_cases <- dat_cases |>
  select(-any_of(wa_names))

#Shorten condition names by eliminating any details inside parentheses. This will 
#make plots more legible.
dat_clean$condition <- gsub("\\s*\\([^\\)]+\\)", "", dat_clean$condition)
dat_wa_cases$condition <- gsub("\\s*\\([^\\)]+\\)", "", dat_wa_cases$condition)
dat_ww_cases$condition <- gsub("\\s*\\([^\\)]+\\)", "", dat_ww_cases$condition)

#Finally, for one of our plots we will want a separate version of dat_clean for 
#WA and WW. In the next script we will pivot these dfs.
wa_rates <- names(dat_clean)[grepl("wa", names(dat_clean))]
wa_dat <- dat_clean |>
  select(condition, all_of(wa_rates))
names(wa_dat) <- gsub("_wa_rate$", "", names(wa_dat))
ww_dat <- dat_clean |>
  select(-all_of(wa_rates))
names(ww_dat) <- gsub("_rate$", "", names(ww_dat))

### 2. pop ###
#Rename the variables in 'pop'
names(pop) <- c("year", "walla_walla", "washington")

### 3. sti_sets ###
#Due to excel formatting, need to adjust gonorrhea_coinfection and gonorrhea_visit
#to ignore chlamydia data

proper_headings_1 <- c("year", "Coinfection reported", "No coinfection reported")
proper_headings_2 <- c("year", "Symptomatic", "Routine exam", "Exposed", 
                       "Not reported")

gonorrhea_coinfection <- gonorrhea_coinfection |>
  select(names(gonorrhea_coinfection)[c(1, 4, 5)])

gonorrhea_visit <- gonorrhea_visit |>
  select(names(gonorrhea_visit)[c(1, 6:9)])

names(gonorrhea_coinfection) <- proper_headings_1
names(gonorrhea_visit) <- proper_headings_2

#Also need to add names for reinfections (this are multiple STIs within a year)
names(reinfections) <- c("year", "reinfections")

#And adjust one long name for hcv_age
names(hcv_age)[2] <- "total"

#Split HIV_prevalence into separate dfs
names(hiv_prevalence) <- tosnake(names(hiv_prevalence))
hiv_age <- hiv_prevalence |>
  select(year, `10_19`, `20_29`, `30_39`, `40_49`, `50_59`, `60_69`, `70_79`, 
         `80`)

hiv_sex <- hiv_prevalence |>
  select(year, f, m)

hiv_race <- hiv_prevalence |>
  select(year, american_indian_alaska_native, asian, black_african_american, 
         hispanic_latino_all_races, multiracial, white)

#Now we create the list sti_sets
sti_sets <- list(
  chlamydia_age = chlamydia_age, 
  chlamydia_race = chlamydia_race, 
  chlamydia_sex = chlamydia_sex,
  chlamydia_coinfection = chlamydia_coinfection,
  chlamydia_visit = chlamydia_visit,
  gonorrhea_age = gonorrhea_age, 
  gonorrhea_race = gonorrhea_race, 
  gonorrhea_sex = gonorrhea_sex,
  gonorrhea_coinfection = gonorrhea_coinfection,
  gonorrhea_visit = gonorrhea_visit,
  hiv_age = hiv_age,
  hiv_race = hiv_race,
  hiv_sex = hiv_sex,
  hcv_age = hcv_age,
  hcv_race = hcv_race,
  hcv_sex = hcv_sex)

### 4. sade (small area demographic estimates) ###
sade2 <- sade2 |>
  filter(`Area Name` == "Walla Walla")

sade1 <- sade1 |>
  filter(`Area Name` == "Walla Walla")

sade <- rbind(sade1, sade2) |>
  select(c("Year",
           "Age Group",
           "Total",
           "Male",
           "Female",
           "White Total",
           "Black Total",
           "AIAN Total",
           "Asian Total",
           "NHPI Total",
           "Two or More Races Total"))

sade_age <- sade[ , c("Year", "Age Group", "Total")] |>
  pivot_wider(names_from = `Age Group`,
              values_from = Total)

sade_age <- sade_age |>
  mutate(across(all_of(names(sade_age)), as.numeric)) |>
  mutate(
    `0 to 9`   = `0-4` + `5-9`,
    `10 to 19` = `10-14` + `15-19`,
    `20 to 29` = `20-24` + `25-29`,
    `30 to 39` = `30-34` + `35-39`,
    `40 to 49` = `40-44` + `45-49`,
    `50 to 59` = `50-54` + `55-59`,
    `60 to 69` = `60-64` + `65-69`,
    `70 to 79` = `70-74` + `75-79`,
    `> 80` = `80-84` + `85+`
  ) |>
  select(-c(`0-4`, `5-9`, `10-14`, `15-19`, `20-24`, `25-29`,
            `30-34`, `35-39`, `40-44`, `45-49`, `50-54`, `55-59`,
            `60-64`, `65-69`, `70-74`, `75-79`, `80-84`, `85+`))

sade_rest <- sade |>
  filter(`Age Group` == "Total") |>
  select(-c("Age Group", "Total"))

sade3 <- sade3 |>
  filter(`Area Name` == "Walla Walla")

sade4 <- sade4 |>
  filter(`Area Name` == "Walla Walla")

sade_hispanic <- rbind(sade3, sade4) |>
  filter(`Age Group` == "Total") |>
  select(Year, `Hispanic Total`)

sade <- merge(sade_age, sade_rest, by = "Year") 
sade <- merge(sade, sade_hispanic, by = "Year")

names(sade) <- gsub("\\s+Total$", "", names(sade))

sade <- sade |>
  mutate(across(all_of(names(sade)), as.numeric)) |>
  mutate(across(everything(), ~ round(., 0)))



#### Write files ####
saveRDS(pop, here("data", "output", "pop.rds"))
saveRDS(dat_clean, here("data", "output", "dat_clean.rds"))
saveRDS(wa_dat, here("data", "output", "wa_dat.rds"))
saveRDS(ww_dat, here("data", "output", "ww_dat.rds"))
saveRDS(dat_wa_cases, here("data", "output", "dat_wa_cases.rds"))
saveRDS(dat_ww_cases, here("data", "output", "dat_ww_cases.rds"))
saveRDS(enteric, here("data", "output", "enteric.rds"))
saveRDS(sade, here("data", "output", "sade.rds"))
walk(names(sti_sets), function(name) {
  saveRDS(
    sti_sets[[name]],
    file = here("data", "output", "sti_sets", paste0(name, ".rds"))
  )
})

