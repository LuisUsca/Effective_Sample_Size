# script to obtain length composition input sample size

# load/source libraries/functions for testing
library(purrr)
library(tidyverse)
library(tidytable)
library(psych)
library(vroom)
library(here)

source_files <- list.files(here::here("R"), "*.R$")
map(here::here("R", source_files), source)

# set number of desired bootstrap iterations (suggested here: 10 for testing, 500 for running)
#iters = 500
iters = 50

# for testing run time
if(iters < 100){
  st <- Sys.time()
}

# read in data ----

data <- vroom::vroom(here::here("data", "data.csv"),delim = ",") %>% 
  dplyr::rename_with(tolower) %>%  # make all column names lower case
  tidytable::mutate(species = 1) # make a dummy variable to allow for multiple species (not sure if that's what the sp column is)


# resample marginal comps
fsh_iss(iters = iters, 
        data = data, 
        yrs = 0, 
        bin = 1, 
        exp_meth = 'marginal', 
        boot_trip = TRUE, 
        boot_lengths = TRUE, 
        save = 'marg')

# test expanded comps
fsh_iss(iters = iters, 
        data = data, 
        yrs = 0, 
        bin = 1, 
        exp_meth = 'expanded', 
        boot_trip = TRUE, 
        boot_lengths = TRUE, 
        save = 'exp')

# For testing run time of 500 iterations ----
if(iters < 100){
  end <- Sys.time()
  runtime <- (end - st) / iters * 500 / 60
  runtime
}

