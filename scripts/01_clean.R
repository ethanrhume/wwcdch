#### Setup ####
rm(list = ls())

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(here, tidyverse) #Loads necessary packages

here::i_am("scripts/01_clean.R") #Anchors the project

source(here("R", "custom_functions.R")) #Adds custom functions to environment
dat <- read_csv(here("data", "raw_survey.csv")) #Loads in data

# ...cleaning steps...

write_csv(dat, here("outputs", "survey_clean.csv"))