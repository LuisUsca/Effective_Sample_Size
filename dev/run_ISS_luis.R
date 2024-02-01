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
  dplyr::rename_with(tolower) # make all column names lower case


# resample marginal comps
fsh_iss(iters = iters, 
        lfreq_data = lfreq, 
        specimen_data = specimen, 
        catch_data = catch, 
        r_t = NULL, 
        yrs = 2015, 
        bin = 1, 
        join = 'both', 
        exp_meth = 'marginal', 
        boot_primes = TRUE, 
        boot_lengths = TRUE, 
        boot_ages = TRUE, 
        al_var = FALSE, 
        al_var_ann = FALSE, 
        age_err = FALSE, 
        region = area, 
        save = 'marg')

# test expanded comps
fsh_iss(iters = iters, 
        lfreq_data = lfreq, 
        specimen_data = specimen, 
        catch_data = catch, 
        r_t = NULL, 
        yrs = 2015, 
        bin = 1, 
        join = 'both', 
        exp_meth = 'expanded', 
        boot_primes = TRUE, 
        boot_lengths = TRUE, 
        boot_ages = TRUE, 
        al_var = FALSE, 
        al_var_ann = FALSE, 
        age_err = FALSE, 
        region = area, 
        save = 'exp')

# For testing run time of 500 iterations ----
if(iters < 100){
  end <- Sys.time()
  runtime <- (end - st) / iters * 500 / 60
  runtime
}

