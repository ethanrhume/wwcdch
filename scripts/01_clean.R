#### Setup ####
rm(list = ls())

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(here, tidyverse) #Loads necessary packages + installs if needed

here::i_am("scripts/01_clean.R") #Ensures `here` properly IDs top-level directory

source(here("R", "custom_functions.R")) #Adds custom functions to environment
dat <- read_csv(here("data", "raw_survey.csv")) #Loads in data

# ...cleaning steps...

write_csv(dat, here("outputs", "survey_clean.csv"))