#lmpt-ow-aaron-psychopath.R
# 2024 01 16 
# joseph bulbulia : joseph.bulbulia@gmail.com
# outcome-wide-analysis-psychopathy

# aaron's study


# preliminaries -----------------------------------------------------------


source("/Users/joseph/GIT/templates/functions/libs2.R")

# WARNING:  COMMENT THIS OUT. JB DOES THIS FOR WORKING WITHOUT WIFI
source("/Users/joseph/GIT/templates/functions/funs.R")

# experimental functions
source(
  "/Users/joseph/GIT/templates/functions/experimental_funs.R"
)
#
# 
# # ALERT: UNCOMMENT THIS AND DOWNLOAD THE LIBRARIES FROM JB's GITHUB
# source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs2.R")
# 
# # ALERT: UNCOMMENT THIS AND DOWNLOAD THE FUNCTIONS FROM JB's GITHUB
# source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")
# 
# 
# # ALERT: UNCOMMENT THIS AND DOWNLOAD THE FUNCTIONS FROM JB's GITHUB
# source(
#   "https://raw.githubusercontent.com/go-bayes/templates/main/functions/experimental_funs.R"
# )



## WARNING SET THIS PATH TO YOUR DATA ON YOUR SECURE MACHINE. DO NOT USE THIS PATH
# nzavs data
pull_path <-
  fs::path_expand(
    "/Users/joseph/Library/CloudStorage/Dropbox-v-project/Joseph\ Bulbulia/00Bulbulia\ Pubs/DATA/nzavs-current/r-data/nzavs_data"
)

# read data: note that you need use the arrow package in R
dat <- arrow::read_parquet(pull_path)

### WARNING: THIS PATH WILL NOT WORK FOR YOU. PLEASE SET A PATH TO YOUR OWN COMPUTER!! ###
### WARNING: FOR EACH NEW STUDY SET UP A DIFFERENT PATH OTHERWISE YOU WILL WRITE OVER YOUR MODELS
push_mods <-
  fs::path_expand(
    "/Users/joseph/Library/CloudStorage/Dropbox-v-project/data/lmtp/24/aaron_psychopathy"
  )

# check path:is this correct?  check so you know you are not overwriting other directors
push_mods

# set exposure here
#nzavs_exposure <- "XXXX"


# define exposures --------------------------------------------------------
# define exposure
A <- "t1_aaron_psychopathy_combined"

# set exposure variable, can be both the continuous and the coarsened, if needed

exposure_var = c("aaron_psychopathy_combined", "not_lost") #

# see second function below

# set number of folds for ML here. use a minimum of 5 and a max of 10
SL_folds = 10

#this will allow you to track progress
progressr::handlers(global = TRUE)

# set seed for reproducing results
seed <- 0112358
set.seed(seed)

# set cores for estimation
library(future)
plan(multisession)
n_cores <- parallel::detectCores()

# super learner libraries
sl_lib <- c(
 # "SL.glmnet",
  "SL.randomForest",
  "SL.ranger"
  #"SL.xgboost" # FORESTS WORK BEST FOR SMALL DATA
            )
SL_folds

# superlearner libraries
library(SuperLearner)
library(ranger)
library(randomForest)

# check options
listWrappers()

#notes on names
# support_help = rel_support01,
# self_control_have_lots = self_control01,
# self_control_wish_more_reversed = self_control02r,
# belong_routside_reversed = rel_belong02r,
# select variables that aaron wants:
dt_init <- dat |> 
  dplyr::arrange(id, wave) |>  dplyr::rename(
    rel_support01 = support_help,# needed to avoid stich ups
    self_control01 = self_control_have_lots,# needed to avoid stich ups
    rel_belong02r = belong_routside_reversed # needed to avoid stich ups
  )
  # select(
  #   pers_a_ipip02r,
  #   pers_a_ipip04r,
  #   pers_a_ipip01,
  #   sdo02,
  #   sdo03,
  #   support_help,# rel_support01
  #   pers_n_ipip02r,
  #   pers_n_ipip04r,
  #   vengeful_rumination01,
  #   vengeful_rumination03,
  #   self_control_have_lots, # self_control01
  #   belong_routside_reversed, #rel_belong02r,
  #   pers_c_ipip03r,
  #   pers_n_ipip01,
  #   pers_c_ipip04r,
  #   pers_c_ipip01,
  #   police_engagement02,
  #   pers_hon_hum04r,
  #   pers_e_ipip01,
  #   pers_e_ipip04,
  #   pers_modesty01r,
  #   pers_modesty04r,
  #   pers_narc01r,
  #   pers_modesty02
  # ) #|>   #reverse code





# list of variables to check
vars_to_check <- c("pers_a_ipip02r",
                   "pers_a_ipip04r",
                   "pers_a_ipip01",
                   "rel_support01",
                   "pers_n_ipip02r",
                   "pers_n_ipip04r",
                   "vengeful_rumination01",
                   "vengeful_rumination03",
                   "pers_c_ipip03r",
                   "pers_c_ipip04r",
                   "pers_c_ipip01",
                   "police_engagement02",
                   "pers_hon_hum04r",
                   "pers_modesty01r",
                   "pers_modesty04r",
                   "pers_narc01r",
                   "pers_modesty02")

# Check the maximum value for each variable
max_values <- dt_init %>%
  summarise(across(all_of(vars_to_check), ~ max(., na.rm = TRUE)))

# Check if all max values are 7
all_max_seven <- all(max_values == 7, na.rm = TRUE)

if(all_max_seven) {
  print("All maximum values are 7.")
} else {
  print("Not all maximum values are 7.")
}

# Corrected reverse scoring formula
dt_init_use <- dt_init |>
  dplyr::mutate(across(all_of(vars_to_check),
                       ~ 8 - .x,
                       .names = "aaron_reversed_{.col}"))



# checks

# create vars

# step 1: make a vector of the original variable names that need to be reversed

# get vars that are reversed


# step 2: prepend "aaron_reversed_" to the names that require reversing
clean_reversed_vars <- paste0("aaron_reversed_", vars_to_check)

clean_reversed_vars

# step 3: replace the original variable names with reversed names where needed

# get clean variable names
antagonism_vars_clean <- c(
  "pers_a_ipip02r",
  "pers_a_ipip04r",
  "pers_a_ipip01",
  "sdo03",
  "sdo02",
  "rel_support01"
)

emotional_stability_vars_clean <- c(
  "pers_n_ipip02r",
  "pers_n_ipip04r",
  "vengeful_rumination01",
  "vengeful_rumination03",
  "self_control01",
  "rel_belong02r"
)

disinhibition_vars_clean <- c(
  "pers_c_ipip03r",
  "pers_n_ipip01",
  "pers_c_ipip04r",
  "pers_c_ipip01",
  "police_engagement02",
  "pers_hon_hum04r"
)

narcissism_vars_clean <- c(
  "pers_e_ipip01",
  "pers_e_ipip04",
  "pers_modesty01r",
  "pers_modesty04r",
  "pers_narc01r",
  "pers_modesty02"
)

# function to get the reversed variables list with 'aaron_reversed_' prefix
get_reversed_vars <- function(vars_clean, reverse_vars_clean) {
  # Start with an empty vector to store the output
  reversed_vars <- vector("character", length(vars_clean))
  
  for (i in seq_along(vars_clean)) {
    var_name <- vars_clean[i]
    
    # Check if the variable needs to be reversed
    if (var_name %in% reverse_vars_clean) {
      # Add the reversed variable name with the prefix
      reversed_vars[i] <- paste0("aaron_reversed_", var_name)
    } else {
      # Keep the original variable name
      reversed_vars[i] <- var_name
    }
  }
  
  # Return the vector with no duplicates and only the necessary variables
  unique(reversed_vars)
}



# use function
antagonism_vars_reversed <- get_reversed_vars(antagonism_vars_clean, clean_reversed_vars)

antagonism_vars_reversed



#check
antagonism_vars_reversed

emotional_stability_vars_reversed <- get_reversed_vars(emotional_stability_vars_clean, clean_reversed_vars)

# check
emotional_stability_vars_reversed

disinhibition_vars_reversed <- get_reversed_vars(disinhibition_vars_clean, clean_reversed_vars)

#check
disinhibition_vars_reversed

narcissism_vars_reversed <- get_reversed_vars(narcissism_vars_clean, clean_reversed_vars)


# check all again
antagonism_vars_reversed
emotional_stability_vars_reversed
disinhibition_vars_reversed
narcissism_vars_reversed



# define the lists
antagonism_vars_reversed <- c(
  "aaron_reversed_pers_a_ipip02r", 
  "aaron_reversed_pers_a_ipip04r", 
  "aaron_reversed_pers_a_ipip01",  
  "sdo03",                        
  "sdo02",                         
  "aaron_reversed_rel_support01"
)

emotional_stability_vars_reversed <- c(
  "aaron_reversed_pers_n_ipip02r",        
  "aaron_reversed_pers_n_ipip04r",        
  "aaron_reversed_vengeful_rumination01",
  "aaron_reversed_vengeful_rumination03", 
  "self_control01",                       
  "rel_belong02r"                       
)

disinhibition_vars_reversed <- c(
  "aaron_reversed_pers_c_ipip03r",      
  "pers_n_ipip01",                      
  "aaron_reversed_pers_c_ipip04r",     
  "aaron_reversed_pers_c_ipip01",       
  "aaron_reversed_police_engagement02", 
  "aaron_reversed_pers_hon_hum04r"    
)

narcissism_vars_reversed <- c(
  "pers_e_ipip01",                  
  "pers_e_ipip04",                  
  "aaron_reversed_pers_modesty01r", 
  "aaron_reversed_pers_modesty04r",
  "aaron_reversed_pers_narc01r",    
  "aaron_reversed_pers_modesty02" 
)


# combine all the lists into a single vector
all_vars <- c(antagonism_vars_reversed, emotional_stability_vars_reversed, disinhibition_vars_reversed, narcissism_vars_reversed)

# find duplicates
duplicate_vars <- all_vars[duplicated(all_vars)]

# print out the duplicated variable names
duplicate_vars


# compute the average for each group
dat_init_use_2 <- dt_init_use %>%
  mutate(
    aaron_antagonism = rowMeans(select(., all_of(antagonism_vars_clean)), na.rm = TRUE),
    aaron_emotional_stability = rowMeans(select(., all_of(emotional_stability_vars_reversed)), na.rm = TRUE),
    aaron_disinhibition = rowMeans(select(., all_of(antagonism_vars_reversed)), na.rm = TRUE),
    aaron_narcissism = rowMeans(select(., all_of(narcissism_vars_reversed)), na.rm = TRUE),
    aaron_psychopathy = rowMeans(select(., all_of(all_vars)), na.rm = TRUE),
    aaron_psychopathy_test = rowMeans(select(., all_of(c(antagonism_vars_clean, emotional_stability_vars_reversed, disinhibition_vars_reversed,narcissism_vars_reversed))), na.rm = TRUE))


table(dat_init_use_2$aaron_psychopathy == dat_init_use_2$aaron_psychopathy_test)

hist(dat_init_use_2$aaron_psychopathy)

hist(dat_init_use_2$aaron_psychopathy_test)



hist(dat_init_use_2$aaron_antagonism)

hist(dat_init_use_2$aaron_emotional_stability)
hist(dat_init_use_2$aaron_disinhibition)
hist(dat_init_use_2$aaron_narcissism)
hist(dat_init_use_2$aaron_psychopathy_combined)


# psychometric validation -------------------------------------------------
library(psych)
library(lavaan)
library(performance)
library(knitr)
library(datawizard)


# version 1 Variables
vars_version_1 <-all_vars

# version 2 Variables
vars_subscale <- c('aaron_antagonism', 'aaron_emotional_stability', 'aaron_disinhibition', 'aaron_narcissism')


# data frame
dat_init_use_2$wave

psycho_dt <- dat_init_use_2 |> filter(wave == "2018" & year_measured == 1)

hist(psycho_dt$aaron_antagonism)
hist(psycho_dt$aaron_narcissism)
hist(psycho_dt$aaron_emotional_stability)
hist(psycho_dt$aaron_disinhibition)
hist(psycho_dt$aaron_psychopathy)
hist(psycho_dt$aaron_psychopathy_test)


# extract relevant variables
dt_antagonism <- psycho_dt |> select(all_of(antagonism_vars_reversed))
dt_emotional_stability <- psycho_dt |> select(all_of(emotional_stability_vars_reversed))
dt_disinhibition<- psycho_dt |> select(all_of(disinhibition_vars_reversed))
dt_narcissism<- psycho_dt |> select(all_of(narcissism_vars_reversed))
dt_all_vars<- psycho_dt |> select(all_of(all_vars))
dt_subscale_vars <- psycho_dt |> select(all_of(vars_subscale))

dt_subscale_vars
narcissism_vars_reversed

# check factor structure for each version
performance::check_factorstructure(dt_antagonism)
performance::check_factorstructure(dt_emotional_stability)
performance::check_factorstructure(dt_disinhibition)
performance::check_factorstructure(dt_narcissism)
performance::check_factorstructure(dt_all_vars)
performance::check_factorstructure(dt_subscale_vars)


# EFA for each version
efa_dt_antagonism <- psych::fa(dt_antagonism, nfactors = 1) %>%
  model_parameters(sort = TRUE, threshold = "max")   

efa_dt_emotional_stability <- psych::fa(dt_emotional_stability, nfactors = 1) %>%
  model_parameters(sort = TRUE, threshold = "max")

efa_dt_disinhibition <- psych::fa(dt_disinhibition, nfactors = 1) %>%
  model_parameters(sort = TRUE, threshold = "max")

efa_dt_narcissism <- psych::fa(dt_narcissism, nfactors = 1) %>%
  model_parameters(sort = TRUE, threshold = "max")

efa_dt_all_vars <- psych::fa(dt_all_vars, nfactors = 4) %>%
  model_parameters(sort = TRUE, threshold = "max")
efa_dt_all_vars

#efa_dt_subscale_vars <- psych::fa(dt_subscale_vars, nfactors = 3) %>%
  model_parameters(sort = TRUE, threshold = "max")

# Display EFA results
efa_dt_antagonism
efa_dt_emotional_stability
efa_dt_disinhibition
efa_dt_narcissism


# parallel analysis to determine the number of factors
n_dt_antagonism <- n_factors(dt_antagonism)
n_dt_emotional_stability <- n_factors(dt_emotional_stability)
n_dt_disinhibition <- n_factors(dt_disinhibition)
n_dt_narcissism <- n_factors(dt_narcissism)
n_dt_all_vars <- n_factors(dt_all_vars)
n_dt_subscale_vars <- n_factors(dt_subscale_vars)


# plot results
p_factors_antagonism <- plot(n_dt_antagonism) + theme_classic()
p_factors_emotional_stability <- plot(n_dt_emotional_stability) + theme_classic()
p_factors_disinhibition <- plot(n_dt_disinhibition) + theme_classic()
p_factors_narcissism <- plot(n_dt_narcissism) + theme_classic()
p_factors_antagonism + p_factors_emotional_stability + p_factors_disinhibition + p_factors_narcissism

# split data for training and testing (for CFA)
part_dt_antagonism <- data_partition(dt_antagonism, training_proportion = .7, seed = 1234)
part_dt_emotional_stability <- data_partition(dt_emotional_stability, training_proportion = .7, seed = 1234)
part_dt_disinhibition<- data_partition(dt_disinhibition, training_proportion = .7, seed = 1234)
part_dt_narcissism <- data_partition(dt_narcissism, training_proportion = .7, seed = 1234)
# Define CFA models based on EFA results

# code for making model to do cfa
create_lavaan_model <- function(var_list, latent_factor_name = "latent_factor") {
  # concatenate variable names with ' + '
  var_string <- paste(var_list, collapse = " + ")
  
  # construct the model string
  model_string <- paste(latent_factor_name, "=~", var_string)
  
  return(model_string)
}


# create the model string
model_string_antagonism <- create_lavaan_model(antagonism_vars_reversed)
model_string_emotional_stability <- create_lavaan_model(emotional_stability_vars_reversed)
model_string_disinhibition <- create_lavaan_model(disinhibition_vars_reversed)
model_string_narcissism <- create_lavaan_model(narcissism_vars_reversed)
model_string_all_vars <- create_lavaan_model(all_vars)


# model_string_subscale <- create_lavaan_model(vars_version_2)


# print the model strings
model_string_antagonism
model_string_emotional_stability
model_string_disinhibition
model_string_narcissism
model_string_all_vars

model_string_antagonism
model_string_emotional_stability
model_string_disinhibition
model_string_narcissism

# perform CFA on test data for each version
cfa_antagonism <- suppressWarnings(lavaan::cfa(model_string_antagonism, data = part_dt_antagonism$test))

cfa_emotional_stability <- suppressWarnings(lavaan::cfa(model_string_emotional_stability, data = part_dt_emotional_stability$test))

cfa_emotional_stability <- suppressWarnings(lavaan::cfa(model_string_emotional_stability, data = part_dt_emotional_stability$test))

cfa_disinhibition <- suppressWarnings(lavaan::cfa(model_string_disinhibition, data = part_dt_disinhibition$test))

cfa_emotional_stability <- suppressWarnings(lavaan::cfa(model_string_emotional_stability, data = part_dt_emotional_stability$test))

cfa_narcissism  <- suppressWarnings(lavaan::cfa(model_string_narcissism, data = part_dt_narcissism $test))


# summaries

summary(cfa_antagonism, fit.measures = TRUE)
summary(cfa_emotional_stability, fit.measures = TRUE)
summary(cfa_disinhibition, fit.measures = TRUE)
summary(cfa_narcissism, fit.measures = TRUE)




# Antagonism:
#   
#   Fit Indices: CFI = 0.896, TLI = 0.827, RMSEA = 0.092, SRMR = 0.051.
# Interpretation: The model shows a marginally acceptable fit with CFI slightly below the preferred threshold. The RMSEA value indicates a poor fit, and the SRMR value suggests an acceptable fit.
# Emotional Stability:
#   
#   Fit Indices: CFI = 0.860, TLI = 0.767, RMSEA = 0.120, SRMR = 0.062.
# Interpretation: The model demonstrates a poor fit with both CFI and TLI well below the desired levels. The RMSEA value is high, further indicating a poor fit.
# Disinhibition:
#   
#   Fit Indices: CFI = 0.934, TLI = 0.891, RMSEA = 0.060, SRMR = 0.032.
# Interpretation: The model has a relatively better fit compared to the other constructs, with CFI and TLI nearing acceptable levels. The RMSEA and SRMR values are within acceptable ranges.
# Narcissism:
#   
#   Fit Indices: CFI = 0.618, TLI = 0.363, RMSEA = 0.193, SRMR = 0.113.
# Interpretation: The model shows a poor fit across all indices. The CFI and TLI are substantially below the acceptable threshold, and the RMSEA is very high.
# In summary, the Disinhibition construct's model demonstrates the best fit among the four, though it's not entirely within the ideal fit thresholds. The Antagonism and Emotional Stability constructs show a marginally acceptable to poor fit. The Narcissism construct's model, however, indicates a particularly poor fit, suggesting a need for model reevaluation or revision. These interpretations should be contextualized within your theoretical framework and consider potential modifications to the model structure or measurement approach.

fit_antagonism <- fitMeasures(cfa_antagonism)
fit_emotional_stability <- fitMeasures(cfa_emotional_stability)
fit_disinhibition <- fitMeasures(cfa_disinhibition)
fit_narcissism <- fitMeasures(cfa_narcissism)





# data wrangling ----------------------------------------------------------


# kessler 6 ---------------------------------------------------------------
# uncomment to get analysis -- showing that K5 is *not* a single latent/ rather
# anxiety and depression differ
#
#
#
# dt_only_k6 <- dt_19 |> select(kessler_depressed, kessler_effort,kessler_hopeless,
#                                  kessler_worthless, kessler_nervous,
#                                  kessler_restless)
#
#
# # check factor structure
# performance::check_factorstructure(dt_only_k6)
#
# # explore a factor structure made of 3 latent variables
# efa <- psych::fa(dt_only_k6, nfactors = 2) %>%
#   model_parameters(sort = TRUE, threshold = "max")
#
# efa
#
#
# n <- n_factors(dt_only_k6)
#
# # plot
# plot(n) + theme_classic()
#
# # CFA
# part_data <- datawizard::data_partition(dt_only_k6, traing_proportion = .7, seed = seed)
#
#
# # set up training data
# training <- part_data$p_0.7
# test <- part_data$test
#
#
# # one factor model
# structure_k6_one <- psych::fa(training, nfactors = 1) |>
#   efa_to_cfa()
#
# # two factor model model
# structure_k6_two <- psych::fa(training, nfactors = 2) |>
#   efa_to_cfa()
#
# # three factor model
# structure_k6_three <- psych::fa(training, nfactors = 3) %>%
#   efa_to_cfa()
#
# # inspect models
# structure_k6_one
# structure_k6_two
# structure_k6_three
#
#
# # Next we perform the confirmatory factor analysis.
#
#
# one_latent <-
#   suppressWarnings(lavaan::cfa(structure_k6_one, data = test))
#
# # two latents model
# two_latents <-
#   suppressWarnings(lavaan::cfa(structure_k6_two, data = test))
#
# # three latents model
# three_latents <-
#   suppressWarnings(lavaan::cfa(structure_k6_three, data = test))
#
#
# # compare models
# compare <-
#   performance::compare_performance(one_latent, two_latents, three_latents, verbose = FALSE)
#
# # view as html table
# as.data.frame(compare) |>
#   kbl(format = "markdown")
#

# import data and wrangle-------------------------------------------------

dt_t <- dat_init_use_2 %>%
  arrange(id,wave) |> 
  rowwise() |> 
  mutate(
    aaron_psychopathy_combined = mean(c(aaron_antagonism, 
                                        aaron_emotional_stability, 
                                        aaron_disinhibition, 
                                        aaron_narcissism), na.rm = TRUE)) |> 
  ungroup()

# check 
table(dt_t$rel_num_l)

hist(dt_t$aaron_antagonism)
hist(dt_t$aaron_emotional_stability)
hist(dt_t$aaron_narcissism)
hist(dt_t$aaron_disinhibition)
hist(dt_t$aaron_psychopathy_combined)

# Checks
min(dt_t$aaron_antagonism, na.rm=TRUE)
max(dt_t$aaron_antagonism, na.rm=TRUE)

min(dt_t$aaron_emotional_stability, na.rm=TRUE)
max(dt_t$aaron_emotional_stability, na.rm=TRUE)

min(dt_t$aaron_disinhibition, na.rm=TRUE)
max(dt_t$aaron_disinhibition, na.rm=TRUE)

min(dt_t$aaron_narcissism, na.rm=TRUE)
max(dt_t$aaron_narcissism, na.rm=TRUE)

min(dt_t$aaron_psychopathy_combined, na.rm=TRUE)
max(dt_t$aaron_psychopathy_combined, na.rm=TRUE)


#### DATA 
here_save(dt_t, "dt_t")

### READ DATA HERE
dt_t <-here_read("dt_t")



# Assuming antagonism_vars_reversed, emotional_stability_vars_reversed, 
# disinhibition_vars_reversed, narcissism_vars_reversed, and vars_subscale 
# are vectors of column names for the respective constructs.

n_unique(dt_t$id)
n_unique(dt_t$rel_num_l)

str(dt_t$rel_num_l)

library(dplyr)

count_dyads <- function(dat, waves = c(2018), year_measured_val = 1) {
  rel_count_test <- dat %>%
    #mutate(rel_num = factor(rel_num)) |> 
    filter(wave %in% waves, year_measured == year_measured_val, !is.na(rel_num_l)) %>%
    group_by(rel_num_l) %>%
    summarise(n_in_couple = n(), .groups = 'drop') %>%
    ungroup()
  
  rel_count_1a <- rel_count_test %>% 
    filter(n_in_couple == 1)
  
  rel_count_2a <- rel_count_test %>% 
    filter(n_in_couple == 2)
  
  list(
    total_dyads = nrow(rel_count_2a) + nrow(rel_count_1a),
    singletons = nrow(rel_count_1a),
    complete_dyads = nrow(rel_count_2a),
    unique_complete_dyads = n_distinct(rel_count_2a$rel_num_l) # Corrected function for counting unique values
  )
}

count_dyads_old <- function(dat, waves = c(2018), year_measured_val = 1) {
  rel_count_test <- dat %>%
    #mutate(rel_num = factor(rel_num)) |> 
    filter(wave %in% waves, year_measured == year_measured_val, !is.na(rel_num)) %>%
    group_by(rel_num) %>%
    summarise(n_in_couple = n(), .groups = 'drop') %>%
    ungroup()
  
  rel_count_1a <- rel_count_test %>% 
    filter(n_in_couple == 1)
  
  rel_count_2a <- rel_count_test %>% 
    filter(n_in_couple == 2)
  
  list(
    total_dyads = nrow(rel_count_2a) + nrow(rel_count_1a),
    singletons = nrow(rel_count_1a),
    complete_dyads = nrow(rel_count_2a),
    unique_complete_dyads = n_distinct(rel_count_2a$rel_num) # Corrected function for counting unique values
  )
}


# Check for a specific wave, e.g., 2018
dyad_counts <- count_dyads(dat, waves = c(2018))
dyad_counts

## OLD DATA

## WARNING SET THIS PATH TO YOUR DATA ON YOUR SECURE MACHINE. DO NOT USE THIS PATH
pull_path_2 <-
  fs::path_expand(
    "/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/DATA/nzavs_refactor/nzavs_data_23"
  )

# read data: note that you need use the arrow package in R
dat_old <- arrow::read_parquet(pull_path_2)

dyad_counts_old <- count_dyads_old(dat_old, waves = c(2018))
dyad_counts_old

str(dat_old$rel_num)

n_unique(dat_old$rel_num)
n_unique(dat$rel_num)

dyad_counts_table <- count_dyads_multi_wave(dat)
dyad_counts_table_old <- count_dyads_multi_wave(dat_old)

dyad_counts_table
dyad_counts_table_old


# dyads in all waves ------------------------------------------------------
count_dyads_multi_wave <- function(dat, start_wave = 2009, end_wave = 2022, year_measured_val = 1) {
  results <- tibble()
  
  for(wave in seq(start_wave, end_wave)) {
    dyad_counts <- count_dyads(dat, waves = c(wave), year_measured_val = year_measured_val)
    temp_tibble <- tibble(
      wave = wave,
      total_dyads = dyad_counts$total_dyads,
      singletons = dyad_counts$singletons,
      complete_dyads = dyad_counts$complete_dyads,
      unique_dyads = dyad_counts$unique_dyads
    )
    
    results <- bind_rows(results, temp_tibble)
  }
  
  return(results)
}


count_dyads_multi_wave_old <- function(dat, start_wave = 2009, end_wave = 2022, year_measured_val = 1) {
  results <- tibble()
  
  for(wave in seq(start_wave, end_wave)) {
    dyad_counts <- count_dyads_old(dat, waves = c(wave), year_measured_val = year_measured_val)
    temp_tibble <- tibble(
      wave = wave,
      total_dyads = dyad_counts$total_dyads,
      singletons = dyad_counts$singletons,
      complete_dyads = dyad_counts$complete_dyads,
      unique_dyads = dyad_counts$unique_dyads
    )
    
    results <- bind_rows(results, temp_tibble)
  }
  
  return(results)
}


dyad_counts_table <- count_dyads_multi_wave(dat)
dyad_counts_table
dyad_counts_table_old <- count_dyads_multi_wave_old(dat_old)
dyad_counts_table_old

aaron_psychopathy_combined
# checks

initial_filter <- dt_t %>%
  filter((wave %in% c(2018, 2019) & year_measured == 1) | 
           (wave == 2020)) %>%
  filter(!is.na(aaron_psychopathy_combined))

glimpse(initial_filter)

initial_filter_dyads <- initial_filter %>% 
  filter(!is.na(rel_num_l))

time_order_debug <- initial_filter_dyads %>% 
  group_by(id, rel_num_l) %>% 
  summarise(waves = list(sort(as.numeric(as.character(unique(wave)))))) %>% 
  ungroup() %>%
  rowwise() %>% 
  filter(all(c(2018, 2019) %in% unlist(waves))) %>% 
  ungroup()


time_order_debug

time_order_check <- initial_filter %>% 
  group_by(id, rel_num_l) %>% 
  summarise(waves = list(sort(unique(wave)))) %>% 
  ungroup() %>%
  rowwise() %>% 
  filter(all(c(2018, 2019) %in% unlist(waves)) & 
           is.unsorted(unlist(waves), strictly = TRUE)) %>%
  ungroup()

time_order_check



initial_filter_year <- initial_filter %>% 
  filter(year_measured == 1)

time_order_debug_year <- initial_filter_year %>% 
  group_by(id, rel_num_l) %>% 
  summarise(waves = list(sort(unique(wave)))) %>% 
  ungroup() %>%
  rowwise() %>% 
  filter(all(c(2018, 2019) %in% unlist(waves))) %>% 
  ungroup()

final_dat <- dt_t %>% 
  semi_join(time_order_debug_year, by = c("id", "rel_num_l"))

n_unique( final_dat$id )
n_unique( final_dat$rel_num_l )

# we condintue debugging
singleton_dyads <- final_dat %>% 
  group_by(rel_num_l) %>% 
  summarise(n_id = n_distinct(id)) %>% 
  filter(n_id != 2)

final_dat_clean <- final_dat %>% 
  anti_join(singleton_dyads, by = "rel_num_l")

n_unique(final_dat_clean$id)
n_unique(final_dat_clean$rel_num_l)




# function to identify dyads ----------------------------------------------

# another approach for identifying dyads ----------------------------------
# Filter the data for years 2018 and 2019 where #year_measured == 1.
dat_18_19 <- dt_t |> 
  filter(wave %in% c(2018, 2019) & year_measured == 1 & !is.na(aaron_psychopathy_combined))

#Group by id and find the number of unique waves they are part of.
id_count <- dat_18_19 |> 
  group_by(id) |> 
  summarise(n_unique_waves = n_distinct(wave), .groups = 'drop') |> 
  filter(n_unique_waves == 2)

valid_ids_1 <- id_count$id

dat_filtered <- dt_t |> 
  filter(id %in% valid_ids_1 & wave %in% c(2018, 2019, 2020))

n_unique(dat_filtered$id)

# Group by rel_num_l and wave to find the number of individuals in each relationship for each wave.
rel_count <- dat_filtered |> 
  filter(wave %in% c(2018, 2019) & year_measured == 1 & !is.na(rel_num_l)) |>  # Only consider 2018 and 2019
  group_by(rel_num_l, wave) |> 
  summarise(n_in_wave = n(), .groups = 'drop') 

rel_count_1 <- rel_count |> 
  filter(n_in_wave == 2)

rel_count_2 <- rel_count |> 
  filter(n_in_wave == 2)

nrow( rel_count_2) + nrow(rel_count_1)
nrow( rel_count_2)

# 
# rel_count_compare <- dat_2 |> 
#   filter(wave %in% c(2018, 2019) & year_measured == 1 & !is.na(rel_num_l) & !is.na(psychopathy_scale) & employed == 1) |>  # Only consider 2018 and 2019
#   group_by(rel_num_l, wave) |> 
#   summarise(n_in_wave = n(), .groups = 'drop') 
# 
# table( rel_count_compare$n_in_wave)

# constitent relationship
dyadic_rel <- rel_count |> 
  filter(n_in_wave == 2) 


dyadic_rel

valid_dyadic_rel_num_l<- dyadic_rel$rel_num_l
length(valid_dyadic_rel_num_l)

# Filter the dat_filtered data frame for these consistent rel_num_l.
dat_final_dyadic <- dat_filtered |> 
  filter(rel_num_l %in% valid_dyadic_rel_num_l)

n_unique(dat_final_dyadic$id)

skimr::n_unique(dat_final_dyadic$id)
skimr::n_unique(dat_final_dyadic$rel_num_l)

hist( dat_final_dyadic$aaron_psychopathy_combined )

hist( dat_final_dyadic$aaron_narcissism)



# define exposure
nzavs_exposure <- "aaron_psychopathy_combined"
exposure_var = c("aaron_psychopathy_combined", "not_lost") #

# data
dat_final_dyadic$wave


#### SAVE DATA 
push_mods
here_save(dat_final_dyadic, "dat_final_dyadic")


## tests
# dat_final_dyadic <- readRDS("/Users/joseph/Library/CloudStorage/Dropbox-v-project/data/lmtp/24/aaron_psychopathy/dat_final_dyadic")
# table(test == dat_final_dyadic)
#                    
                   
dat_long <- dat_final_dyadic |> 
  # ungroup
  select(
    "wave",
    "year_measured",
    "id",
    "aaron_psychopathy_combined", #aaron
    "aaron_antagonism",#aaron
    "aaron_emotional_stability",#aaron
    "aaron_disinhibition", #aaron
    "aaron_narcissism",#aaron
    "aaron_psychopathy_combined",#aaron
    "sat_relationship", # Relationship satisfication
    "sample_origin_names_combined",
    # Sample origin names combined
    #"alert_level_combined_lead",  not needed because all receive all levels by the point the outcome is measured
    # covid alert levels -> 2019-2020
    "education_level_coarsen",
    # Ordinal-Rank 0-10 NZREG codes (with overseas school quals coded as Level 3, and all other ancillary categories coded as missing)  Combined highschool levels See:https://www.nzqa.govt.nz/assets/Studying-in-NZ/New-Zealand-Qualification-Framework/requirements-nzqf.pdf
    "male",
    "age",
    "born_nz",
    "hlth_disability",
    # value label 0    No 1   Yes
    "eth_cat",
    #factor(EthCat, labels = c("Euro", "Maori", "Pacific", "Asian")),
    "employed",
    # Are you currently employed? (this includes self-employment or casual work)
    # "gen_cohort",
    "household_inc",
    # Please estimate your total household income (before tax) for the last year.
    "nz_dep2018",
    # see nzavs materials
    "nzsei13",
    # see nzavs materials
    "partner",
    # 0 = no, 1 = yes
    "parent",
    # 0 = no, 1 = yes
    "political_conservative",
    #Please rate how politically liberal versus conservative you see yourself as being.
    "pol_wing",
    # Please rate how politically left-wing versus right-wing you see yourself as being.
    "urban",
    # see NZAVS,
    "have_siblings",
    #Do you have siblings?
    "total_siblings",
    # sum siblings
    "number_sisters_older",
    #How many older sisters do you have?
    "number_sisters_younger",
    #	How many younger sisters do you have?
    "number_brothers_older",
    #	How many older brothers do you have?
    "number_brothers_younger",
    #	How many older brothers do you have?
    "children_num",
    # How many children have you given birth to, fathered, or adopted?
    "hours_children",
    #Hours - Looking after children
    "hours_work",
    #Hours - Working in paid employment
    "hours_housework",
    # Hours - Housework/cooking
    "religion_church",
    "w_gend_age_euro",
    # sample_weights.
    "modesty",
    "hours_exercise",
    "kessler6_sum",
    # depression constructs,
    "kessler_latent_depression",
    "kessler_latent_anxiety",
    # During the last 30 days, how often did.... you have negative thoughts that repeated over and over?
    "kessler_depressed",
    #During the last 30 days, how often did.... you feel so depressed that nothing could cheer you up?
    "kessler_effort",
    #During the last 30 days, how often did.... you feel that everything was an effort?
    "kessler_hopeless",
    # During the last 30 days, how often did.... you feel hopeless?
    "kessler_nervous",
    #During the last 30 days, how often did.... you feel nervous?
    "kessler_restless",
    #During the last 30 days, how often did.... you feel restless or fidgety?
    "kessler_worthless",
    # During the last 30 days, how often did.... you feel worthless?
    "sexual_satisfaction",
    #  How satisfied are you with your sex life?
    "bodysat",
    ## Am satisfied with the appearance, size and shape of my body.
    "vengeful_rumin",
    "self_esteem",
    "selfesteem_satself",
    #  On the whole am satisfied with myself.
    "selfesteem_postiveself",
    # Take a positive attitude toward myself
    "selfesteem_failure_reversed",
    "pwi",
    "pwb_your_health",
    # #Your health.
    "pwb_your_relationships",
    # #Your personal relationships.
    "pwb_your_future_security",
    # #Your future security.
    "pwb_standard_living",
    #Your standard of living.
    "lifesat",
    "lifesat_satlife",
    # I am satisfied with my life.
    "lifesat_ideal",
    "sat_relationship",
    "conflict_in_relationship",
    "kessler_latent_anxiety",
    "kessler_latent_depression",
    "alert_level_combined_lead",
    "rel_num_l",
  ) |>
  mutate(
    eth_cat = as.integer(eth_cat),
    urban = as.numeric(urban),
    education_level_coarsen = as.integer(education_level_coarsen)
  ) |>
  dplyr::filter((wave == 2018 & year_measured  == 1) |
                  (wave == 2019  &
                     year_measured  == 1) |
                  (wave == 2020)) |>  # Eligibility criteria  Observed in 2018/2019 & Outcomes in 2020 or 2021
  group_by(id) |>
  ## MAKE SURE YOU HAVE ELIGIBILITY CRITERIA
  dplyr::mutate(
    meets_criteria_baseline = ifelse(year_measured == 1 &
                                       !is.na(!!sym(nzavs_exposure)), 1, 0)) |>  # using R lang
  dplyr::mutate(sample_origin = as.factor( sample_origin_names_combined)) |>  #shorter name
  arrange(id) |>
  filter((wave == 2018 & year_measured == 1) |
           (wave == 2019 & year_measured == 1) |
           (wave == 2020)) %>%
  group_by(id) |>
  mutate(k_18 = ifelse(wave == 2018 &
                         meets_criteria_baseline == 1, 1, 0)) %>% # selection criteria
  mutate(h_18 = mean(k_18, na.rm = TRUE)) %>%
  mutate(k_19 = ifelse(wave == 2019 &
                         meets_criteria_baseline == 1, 1, 0)) %>% # selection criteria
  mutate(h_19 = mean(k_19, na.rm = TRUE)) %>%
  dplyr::filter(h_18 > 0) |>  # hack to enable repeat of baseline
  dplyr::filter(h_19 > 0) |>  # hack to enable repeat of baseline
  ungroup() %>%
  mutate(
    not_lost = ifelse(lead(year_measured) == 1, 1, 0),
    # not_lost = ifelse(lead(year_measured)== -1, 0, not_lost,
    # not_lost = ifelse(lead(year_measured) == 0, 0, not_lost,
    not_lost = ifelse(is.na(not_lost) &
                        year_measured == 1, 1, not_lost),
    not_lost = ifelse(is.na(not_lost), 0, not_lost)
  ) |>
  ungroup() |>
  dplyr::mutate(
   # friends_money = ifelse(friends_money < 0, 0, friends_money),
    # someone gave neg number
   # household_inc_log = log(household_inc + 1),
  #  hours_children_log = log(hours_children + 1),
    hours_work_log = log(hours_work + 1),
    hours_housework_log = log(hours_housework + 1)#,
   # hours_exercise_log = log(hours_exercise + 1)
  ) |>
  dplyr::rename(sample_weights = w_gend_age_euro) |>
  dplyr::mutate(sample_origin = sample_origin_names_combined) |>  #shorter name
  arrange(id, wave) |>
  droplevels() |>
  select(-h_18, -k_18, -h_19, -k_19) |>
  droplevels() |>
  ungroup() %>%
  mutate(wave = as.numeric(wave)) |>
  arrange(id, wave) |>
mutate(
    religion_church_coarsen = cut(
      religion_church,
      breaks = c(-Inf, 0, 1, 3.99, Inf),
      labels = c("zero", "one", "less_four", "four_up"),
      include.lowest = TRUE,
      right = TRUE,
      ordered = TRUE
    )) %>%
  mutate(
    religion_church_coarsen_n = as.numeric(religion_church_coarsen) - 1,
  ) |>
mutate(
  # eth_cat = as.integer(eth_cat),
  urban = as.numeric(urban),
 # education_level_coarsen = as.integer(education_level_coarsen)
) |>
  droplevels() |>
  arrange(id, wave) |>
  data.frame()


# check sample 
N_participants <-n_unique(dat_long$id) #514 
N_participants

N_participants <-n_unique(dat_long$rel_num_l) #514 
N_participants



# save for paper
here_save(N_participants, "N_participants")

# eyeball distribution
# table(dat_long$wave)
dt_19 <- dat_long |>
  filter(year_measured == 1 & wave == 2) |> 
  mutate(aaron_psychopathy_combined_z = scale(aaron_psychopathy_combined))


here_save_arrow(dt_19, "dt_19")


min_score <- min(dt_19$aaron_psychopathy_combined, na.rm = TRUE)
min_score

max_score <- max(dt_19$aaron_psychopathy_combined, na.rm = TRUE)
max_score

sd_exposure <- sd(dt_19$aaron_psychopathy_combined,
                  na.rm = TRUE)
sd_exposure

one_point_in_sd_units <- 1/sd_exposure
one_point_in_sd_units

# half_sd <- sd_exposure / 2
# half_sd


# Decrease by one point (raw scores)
f <- function(data, trt) {
  ifelse(data[[trt]] >= min_score + 1, data[[trt]] - 1,  min_score)
}



#  Increase everyone by one point, contrasted with what they would be anyway.
# only use this function for raw scores

f_1 <- function(data, trt) {
  ifelse(data[[trt]] <= max_score - 1, data[[trt]] + 1,  max_score)
}

# check function logic
max_score - 1
min_score + 1

#check missing
#naniar::vis_miss(dat_long, warn_large_data = FALSE)
dev.off()


# check sample 
N <-n_unique(dat_long$id) #1070 
N

# double check path
push_mods

# check col names
colnames(dat)

dev.off()
# check
dt_check_exposure <- dat_long |> filter(wave == 1| wave == 2)

# makes sure all is false
table (is.na(dt_check_exposure$aaron_psychopathy_combined))

# makes sure all is false
table ((dt_check_exposure$aaron_psychopathy_combined))
# make
dt_18 <- dat_long |>
  filter(wave == 1 )



dt_positivity_full <- dt_check_exposure |>
  filter(wave == 1 | wave == 2) |>
  select(wave, id, aaron_psychopathy_combined, sample_weights) |> 
  mutate(aaron_psychopathy_combined_round = round(aaron_psychopathy_combined, 0))

dt_positivity_full

# check sample weights NA - will return to this after impute
table (is.na(dt_positivity_full$sample_weights)) # 

# test positivity
out <-
  msm::statetable.msm(aaron_psychopathy_combined_round, id, data = dt_positivity_full)

# transition table
t_tab <- transition_table(out, state_names = NULL)
t_tab

here_save(t_tab, "t_tab")



standard_deviation_exposure <-
  coloured_histogram_sd(dt_19, col_name = "aaron_psychopathy_combined", binwidth = .1)

standard_deviation_exposure



#here_save( standard_deviation_exposure, "standard_deviation_exposure")

ggsave(
  standard_deviation_exposure,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "standard_deviation_exposure.jpeg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)


# ALERT: UNCOMMENT THIS AND DOWNLOAD THE FUNCTIONS FROM JB's GITHUB
# histogram_shift <- coloured_histogram_shift(dt_19, col_name = "standard_deviation_exposure", binwidth = .1, range_highlight = "below")
# 
# histogram_shift
# 
# 
# ggsave(
#   histogram_shift,
#   path = here::here(here::here(push_mods, "figs")),
#   width = 12,
#   height = 8,
#   units = "in",
#   filename = "histogram_shift.jpeg",
#   device = 'jpeg',
#   limitsize = FALSE,
#   dpi = 600
# )

# 
# # generate bar plot
# graph_density_of_exposure <- coloured_histogram(dt_19, col_name = "standard_deviation_exposure", scale_min = 1, scale_max = 7)
# 
# graph_density_of_exposure
# 
# 
# ggsave(
#   graph_density_of_exposure,
#   path = here::here(here::here(push_mods, "figs")),
#   width = 12,
#   height = 8,
#   units = "in",
#   filename = "graph_density_of_exposure.jpg",
#   device = 'jpeg',
#   limitsize = FALSE,
#   dpi = 600
# )




# set variables for baseline exposure and outcome -------------------------
baseline_vars = c(
  "male",
  "age",
  "education_level_coarsen",
  # factors
  "eth_cat",
  "parent",
  #factor(EthCat, labels = c("Euro", "Maori", "Pacific", "Asian")),
  #"bigger_doms", #religious denomination
 # "sample_origin",
  "nz_dep2018",
  # "nzsei13",
  # "born_nz",
  "kessler6_sum",
  "kessler_latent_depression",
  "kessler_latent_anxiety",
  # social belonging
 # "household_inc_log",
  # added: measured with error but OK for imputations
 # "partner",
  # "parent",  # newly changed - have information in child number
  "political_conservative",
  "religion_church_coarsen_n",
  #Please rate how politically liberal versus conservative you see yourself as being.
  # Sample origin names combined
  "urban",
  "parent",
  "hours_housework_log",
  "hours_work_log",
  "sample_weights",
  "alert_level_combined_lead",
  "rel_num_l"
)

# check
baseline_vars

# check
exposure_var
dat_long$kessler6_sum
# outcomes
outcome_vars = c(
  "sat_relationship",
  "conflict_in_relationship",
  "kessler6_sum",
  "kessler_latent_depression",
  "kessler_latent_anxiety",
#  "pwb_your_health",
  # #Your health.
#  "pwb_your_relationships",
  # #Your personal relationships.
#  "pwb_your_future_security",
  # #Your future security.
#  "pwb_standard_living",
  "lifesat",
  "pwi",
  "self_esteem"
)
# impute baseline data (we use censoring for the outcomes)
#colnames(dat_long)
# function imputes only baseline not outcome



# make data wide and impute baseline missing values -----------------------


# custom function

dat_long <- as.data.frame(dat_long)
dat_long <- haven::zap_formats(dat_long)
dat_long <- haven::zap_label(dat_long)
dat_long <- haven::zap_widths(dat_long)


naniar::vis_miss(dat_long)
dat_long$wave

str(dat_long)

prep_coop_all <- margot_wide_impute_baseline(
  dat_long,
  baseline_vars = baseline_vars,
  exposure_var = exposure_var,
  outcome_vars = outcome_vars
)



# # check mi model
# outlist <-
#   row.names(prep_coop_all)[prep_coop_all$outflux < 0.5]
# length(outlist)
# 
# # checks. We do not impute with weights: area of current research
# head(prep_coop_all$loggedEvents, 10)

push_mods

# save function -- will save to your "push_mod" directory
here_save(prep_coop_all, "prep_coop_all")

# read function
prep_coop_all <- here_read("prep_coop_all")

head(prep_coop_all)
naniar::vis_miss(prep_coop_all, warn_large_data = FALSE)
dev.off()


#check must be a dataframe
str(prep_coop_all)
nrow(prep_coop_all)
colnames(prep_coop_all)

prep_coop_all <- as.data.frame(prep_coop_all)

sd( prep_coop_all$t2_sat_relationship, na.rm = TRUE)

## dyads response

library(magrittr)
prep_coop_all_1 <- prep_coop_all %>%
  group_by(id, t0_rel_num_l) %>%
  mutate_all(rev) %>%
  ungroup() %>%
  select(-id, -t0_rel_num_l) %>%
  set_colnames(paste0('partner_', colnames(.)))

prep_coop_all_1
prep_coop_all_use  <- cbind(prep_coop_all, prep_coop_all_1)

colnames(prep_coop_all_use)


#
n_unique(prep_coop_all_use$t0_rel_num_l)

# spit and shine:
# load required libraries
library(dplyr)
library(stringr)

# extract column names
col_names <- colnames(prep_coop_all_use)

# identify columns that start with 'partner_'
partner_cols <- str_detect(col_names, "^partner_")

# replace and rename columns
new_col_names <- col_names

# rename operation
new_col_names[partner_cols] <-
  gsub("partner_(t\\d+)_(.*)", "\\1_partner_\\2", col_names[partner_cols])

# apply new column names to dataframe
colnames(prep_coop_all_use) <- new_col_names

# check
colnames(prep_coop_all_use)

# extract column names
col_names <- colnames(prep_coop_all_use)

# extract time prefix and sort based on it
sorted_indices <- order(gsub(".*(t\\d+).*", "\\1", col_names))

# get sorted column names
sorted_col_names <- col_names[sorted_indices]

# use relocate to rearrange the columns
prep_coop_all_use_1 <-
  prep_coop_all_use %>% relocate(all_of(sorted_col_names))

# remove
colnames(prep_coop_all_use_1)


# save function -- will save to your "push_mod" directory
here_save(prep_coop_all_use_1, "prep_coop_all_use_1_backup")

# read function
prep_coop_all_use_1 <- here_read("prep_coop_all_use_1_backup")

colnames(prep_coop_all_use_1)
naniar::vis_miss(prep_coop_all_use_1, warn_large_data = FALSE)
dev.off()



#check must be a dataframe
str(prep_coop_all_use_1)
nrow(prep_coop_all_use_1)
colnames(prep_coop_all_use_1)

prep_coop_all_use_1 <- as.data.frame(prep_coop_all_use_1)

# arrange data for analysis -----------------------------------------------
# spit and shine
df_wide_censored <-
  prep_coop_all_use_1 |>
  mutate(
    t0_education_level_coarsen = as.factor(t0_education_level_coarsen),
    t0_eth_cat = as.factor(t0_eth_cat)
  ) |>
  relocate("t0_not_lost", .before = starts_with("t1_"))  %>%
  relocate("t1_not_lost", .before = starts_with("t2_")) |>
  relocate("t1_partner_not_lost", .before = starts_with("t2_"))

#check
head(df_wide_censored)
dim(df_wide_censored)
str(df_wide_censored)


A <- "t1_aaron_psychopathy_combined"

# save
here_save(df_wide_censored, "df_wide_censored")
df_wide_censored <- here_read("df_wide_censored")



# arrange data for analysis -----------------------------------------------
# spit and shine

# spit and shine
df_clean <- df_wide_censored %>%
  mutate(t2_na_flag = rowSums(is.na(select(
    ., starts_with("t2_")
  ))) > 0) %>%
  mutate(t1_not_lost = ifelse(t2_na_flag, 0, t1_not_lost)) %>%
  mutate(t1_partner_not_lost = ifelse(t2_na_flag, 0, t1_partner_not_lost)) %>%
  # select(-t2_na_flag) %>%
  filter(!rowSums(is.na(select(
    ., starts_with("t0_")
  )))) |>
  dplyr::mutate(
    across(
      where(is.numeric) &
        !t0_not_lost &
        !t1_not_lost &
        !t0_partner_not_lost &
        !t1_partner_not_lost &
        !t0_sample_weights &
       !t0_rel_num_l &
      !t1_aaron_psychopathy_combined,
      ~ scale(.x),
      .names = "{col}_z"
    )
  ) |>
  select(
    where(is.factor),
    t0_not_lost,
    t0_partner_not_lost,
    t0_sample_weights,
    t0_rel_num_l,
    t1_aaron_psychopathy_combined,
    t1_not_lost,
    t1_partner_not_lost,
    ends_with("_z")
  ) |>
  relocate(starts_with("t0_"), .before = starts_with("t1_"))  %>%
  relocate(starts_with("t2_"), .after = starts_with("t1_"))  %>%
  relocate("t0_not_lost", .before = starts_with("t1_"))  %>%
  relocate("t0_partner_not_lost", .before = starts_with("t1_"))  %>%
  relocate("t1_not_lost", .before = starts_with("t2_")) |>
  relocate("t1_partner_not_lost", .before = starts_with("t2_")) |>
  mutate(t0_sample_weights = as.numeric(t0_sample_weights)) |>
  data.frame()

dim(df_clean)
naniar::vis_miss(df_clean, warn_large_data = FALSE)
dev.off()


sd( df_clean$t2_sat_relationship_z, na.rm = TRUE)
sd( df_clean$t2_kessler_latent_depression_z, na.rm = TRUE)


dim(df_clean)
naniar::vis_miss(df_clean, warn_large_data = FALSE)
dev.off()

colnames(df_clean)
# again check path
push_mods
# save
here_save(df_clean, "df_clean")

# read if needed
df_clean <- here_read("df_clean")

str(df_clean)
df_clean <- as.data.frame(df_clean)

#check n
nrow(df_clean)

colnames(df_clean)
# get names
names_base <-
  df_clean |> select(starts_with("t0"),
                     -t0_sample_weights,
                     -t0_not_lost,
                    - t0_partner_sample_weights_z,
                    - t0_partner_alert_level_combined_lead,
                     -id) |> colnames()

names_base

names_outcomes <-
  df_clean |> select(starts_with("t2")) |> colnames()

names_outcomes

colnames(df_clean)

# set variables for models ------------------------------------------------

#### SET VARIABLE NAMES: Customise for each outcomewide model
#  model
A<- "t1_aaron_psychopathy_combined"


C <- c("t1_not_lost")

#L <- list(c("L1"), c("L2"))
W <- c(paste(names_base, collapse = ", "))

# check
print(W)


# check shift
f


# make test data (if needed)
# df_clean_test <- df_clean |>
#   slice_head(n = 2000)

# "SL.earth" refers to a wrapper for the 'earth' function from the 'earth' R package in the SuperLearner library. This function implements Multivariate Adaptive Regression Splines (MARS), a non-parametric regression method that extends linear models by allowing for interactions and non-linear relationships between variables.
# MARS models can handle high-dimensional data well and can be a useful tool for capturing complex patterns in the data. They work by fitting piecewise linear models to the data, which allows for flexible and potentially non-linear relationships between predictors and the outcome.


# models -----------------------------------------------------------


# smoker binary
#Do you currently smoke?

# select_and_rename_cols <- function(names_base, baseline_vars, outcome) {
#   # Select columns that match with baseline_vars
#   selected_cols <- names_base[grepl(paste(baseline_vars, collapse = "|"), names_base)]
#
#   # Rename the outcome variable prefix from t2 to t0
#   outcome_renamed <- gsub("t2_", "t0_", outcome)
#   # Append the renamed outcome to selected columns
#   final_cols <- c(selected_cols, outcome_renamed)
#
#   return(final_cols)
# }
names_outcomes

C
A
n_cores = 10
sl_lib
t2_partner_sat_relationship_z <- lmtp_tmle(
    data = df_clean,
    trt = A,
    baseline = names_base,
    outcome = "t2_partner_sat_relationship_z",
    cens = C,
    shift = f,
    mtp = TRUE,
   folds = SL_folds,
    # trim = 0.99, # if needed
    # time_vary = NULL,
    outcome_type = "continuous",
    id = "t0_rel_num_l",
   # weights = df_clean$t0_sample_weights,
    learners_trt = sl_lib,
    learners_outcome = sl_lib,
    parallel = n_cores
  )


t2_partner_sat_relationship_z
here_save(t2_partner_sat_relationship_z, "t2_partner_sat_relationship_z")

t2_partner_sat_relationship_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_sat_relationship_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
 folds = SL_folds,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  id = "t0_rel_num_l",
  #weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)
here_save(t2_partner_sat_relationship_z_1, "t2_partner_sat_relationship_z_1")
t2_partner_sat_relationship_z_1

t2_partner_sat_relationship_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_sat_relationship_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
 folds = SL_folds,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  id = "t0_rel_num_l",
 # weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)
here_save(t2_partner_sat_relationship_z_null, "t2_partner_sat_relationship_z_null")

t2_partner_sat_relationship_z
t2_partner_sat_relationship_z_null
t2_partner_sat_relationship_z_1



### CONFLICT
names_outcomes


t2_partner_conflict_in_relationship_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_conflict_in_relationship_z",
  cens = C,
  shift = f,
  mtp = TRUE,
 folds = SL_folds,
 #trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  id = "t0_rel_num_l",
#  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_partner_conflict_in_relationship_z
here_save(t2_partner_conflict_in_relationship_z, "t2_partner_conflict_in_relationship_z")

t2_partner_conflict_in_relationship_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_conflict_in_relationship_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
 folds = SL_folds,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  id = "t0_rel_num_l",
#  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

here_save(t2_partner_conflict_in_relationship_z_1, "t2_partner_conflict_in_relationship_z_1")
t2_partner_conflict_in_relationship_z_1

t2_partner_conflict_in_relationship_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_conflict_in_relationship_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
 folds = SL_folds,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  id = "t0_rel_num_l",
#  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)
t2_partner_conflict_in_relationship_z_null
here_save(t2_partner_conflict_in_relationship_z_null, "t2_partner_conflict_in_relationship_z_null")

t2_partner_conflict_in_relationship_z
t2_partner_conflict_in_relationship_z_null
t2_partner_conflict_in_relationship_z_1


##
# names_base_t2_kessler_latent_anxiety_z <-
#   select_and_rename_cols(names_base = names_base,
#                          baseline_vars = baseline_vars,
#                          outcome = "t2_kessler_latent_anxiety_z")

# During the last 30 days, how often did.... you feel that everything was an effort?
# During the last 30 days, how often did.... you feel nervous?
# During the last 30 days, how often did.... you feel restless or fidgety?

### KESSLER


t2_partner_kessler6_sum_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_kessler6_sum_z",
  cens = C,
  shift = f,
  mtp = TRUE,
 folds = SL_folds,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  id = "t0_rel_num_l",
#  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_partner_kessler6_sum_z
here_save(t2_partner_kessler6_sum_z, "t2_partner_kessler6_sum_z")

t2_partner_kessler6_sum_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_kessler6_sum_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
 folds = SL_folds,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  id = "t0_rel_num_l",
 # weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)
here_save(t2_partner_kessler6_sum_z_1, "t2_partner_kessler6_sum_z_1")
t2_partner_kessler6_sum_z_1

t2_partner_kessler6_sum_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_kessler6_sum_z",
  cens = C,
  shift = NULL,
  mtp = FALSE,
 folds = SL_folds,
  # trim = 0.99, # if needed
  # time_vary = NULL,
  outcome_type = "continuous",
  id = "t0_rel_num_l",
#  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)
here_save(t2_partner_kessler6_sum_z_null, "t2_partner_kessler6_sum_z_null")

t2_partner_kessler6_sum_z
t2_partner_kessler6_sum_z_null
t2_partner_kessler6_sum_z_1



## anxiety
t2_partner_kessler_latent_anxiety_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_kessler_latent_anxiety_z",
  cens = C,
  shift = f,
  mtp = TRUE,
 folds = SL_folds,
  outcome_type = "continuous",
#  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_partner_kessler_latent_anxiety_z
here_save(t2_partner_kessler_latent_anxiety_z, "t2_partner_kessler_latent_anxiety_z")




t2_partner_kessler_latent_anxiety_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_kessler_latent_anxiety_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
 folds = SL_folds,
  outcome_type = "continuous",
 # weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


here_save(t2_partner_kessler_latent_anxiety_z_1, "t2_partner_kessler_latent_anxiety_z_1")



t2_partner_kessler_latent_anxiety_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_kessler_latent_anxiety_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
 folds = SL_folds,
  outcome_type = "continuous",
 # weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

# test
here_save(t2_partner_kessler_latent_anxiety_z_null,
          "t2_partner_kessler_latent_anxiety_z_null")

t2_partner_kessler_latent_anxiety_z
t2_partner_kessler_latent_anxiety_z_null
t2_partner_kessler_latent_anxiety_z_1

# depression

# During the last 30 days, how often did.... you feel so depressed that nothing could cheer you up?
# During the last 30 days, how often did.... you feel hopeless?
# During the last 30 days, how often did.... you feel worthless?

# During the last 30 days, how often did.... you feel so depressed that nothing could cheer you up?
# During the last 30 days, how often did.... you feel hopeless?
# During the last 30 days, how often did.... you feel worthless?

t2_partner_kessler_latent_depression_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_kessler_latent_depression_z",
  cens = C,
  shift = f,
  mtp = TRUE,
 folds = SL_folds,
  outcome_type = "continuous",
  #weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_partner_kessler_latent_depression_z
here_save(t2_partner_kessler_latent_depression_z,
          "t2_partner_kessler_latent_depression_z")


t2_partner_kessler_latent_depression_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_kessler_latent_depression_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
 folds = SL_folds,
  outcome_type = "continuous",
 # weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_partner_kessler_latent_depression_z_1
here_save(t2_partner_kessler_latent_depression_z_1,
          "t2_partner_kessler_latent_depression_z_1")



t2_partner_kessler_latent_depression_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_kessler_latent_depression_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
 folds = SL_folds,
  outcome_type = "continuous",
  #weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_partner_kessler_latent_depression_z_null
here_save(t2_partner_kessler_latent_depression_z_null,
          "t2_partner_kessler_latent_depression_z_null")

t2_partner_kessler_latent_depression_z
t2_partner_kessler_latent_depression_z_null
t2_partner_kessler_latent_depression_z_1





# SELF ESTEEM 
t2_partner_self_esteem_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_self_esteem_z",
  cens = C,
  shift = f,
  mtp = TRUE,
 folds = SL_folds,
  outcome_type = "continuous",
 # weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_partner_self_esteem_z
here_save(t2_partner_self_esteem_z, "t2_partner_self_esteem_z")



t2_partner_self_esteem_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_self_esteem_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
 folds = SL_folds,
  outcome_type = "continuous",
  #weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_partner_self_esteem_z_1
here_save(t2_partner_self_esteem_z_1, "t2_partner_self_esteem_z_1")





t2_partner_self_esteem_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_self_esteem_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
 folds = SL_folds,
  outcome_type = "continuous",
  #weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_partner_self_esteem_z_null
here_save(t2_partner_self_esteem_z_null, "t2_partner_self_esteem_z_null")

t2_partner_self_esteem_z
t2_partner_self_esteem_z_1
t2_partner_self_esteem_z_null

# PWI
t2_partner_pwi_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_pwi_z",
  cens = C,
  shift = f,
  mtp = TRUE,
 folds = SL_folds,
  outcome_type = "continuous",
 # weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_partner_pwi_z
here_save(t2_partner_pwi_z, "t2_partner_pwi_z")



# Your health.
t2_partner_pwi_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_pwi_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
 folds = SL_folds,
  outcome_type = "continuous",
 # weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_partner_pwi_z_1
here_save(t2_partner_pwi_z_1, "t2_partner_pwi_z_1")

t2_partner_pwi_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_pwi_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
 folds = SL_folds,
  outcome_type = "continuous",
  #weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_partner_pwi_z_null
here_save(t2_partner_pwi_z_null, "t2_partner_pwi_z_null")

t2_partner_pwi_z
t2_partner_pwi_z_null
t2_partner_pwi_z_1


## life sat
# I am satisfied with my life.
# In most ways my life is close to ideal.
t2_partner_lifesat_z <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_lifesat_z",
  cens = C,
  shift = f,
  mtp = TRUE,
 folds = SL_folds,
  outcome_type = "continuous",
  #weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_partner_lifesat_z
here_save(t2_partner_lifesat_z, "t2_partner_lifesat_z")


t2_partner_lifesat_z_1 <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_lifesat_z",
  cens = C,
  shift = f_1,
  mtp = TRUE,
 folds = SL_folds,
  outcome_type = "continuous",
  #weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_partner_lifesat_z_1
here_save(t2_partner_lifesat_z_1, "t2_partner_lifesat_z_1")

# I am satisfied with my life.
# In most ways my life is close to ideal.
t2_partner_lifesat_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_partner_lifesat_z",
  cens = C,
  shift = NULL,
  # mtp = TRUE,
 folds = SL_folds,
  outcome_type = "continuous",
  #weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)

t2_partner_lifesat_z_null
here_save(t2_partner_lifesat_z_null, "t2_partner_lifesat_z_null")

t2_partner_lifesat_z_1
t2_partner_lifesat_z
t2_partner_lifesat_z_null
# contrasts ---------------------------------------------------------------

# sat relationships
t2_partner_sat_relationship_z <- here_read("t2_partner_sat_relationship_z")
t2_partner_sat_relationship_z_1 <- here_read("t2_partner_sat_relationship_z_1")
t2_partner_sat_relationship_z_null <-
  here_read("t2_partner_sat_relationship_z_null")

# first contrast 
contrast_t2_partner_sat_relationship_z <-
  lmtp_contrast(t2_partner_sat_relationship_z,
                ref = t2_partner_sat_relationship_z_null,
                type = "additive")

tab_contrast_t2_partner_sat_relationship_z <-
  margot_tab_lmtp(contrast_t2_partner_sat_relationship_z,
                  scale = "RD",
                  new_name = "Gain psychopathy: partner relationship satisfaction")


out_tab_contrast_t2_partner_sat_relationship_z <-
  lmtp_evalue_tab(tab_contrast_t2_partner_sat_relationship_z,
                  scale = c("RD"))

out_tab_contrast_t2_partner_sat_relationship_z

# second contrast
contrast_t2_partner_sat_relationship_z_1 <-
  lmtp_contrast(t2_partner_sat_relationship_z_1,
                ref = t2_partner_sat_relationship_z_null,
                type = "additive")


tab_contrast_t2_partner_sat_relationship_z_1 <-
  margot_tab_lmtp(contrast_t2_partner_sat_relationship_z_1,
                  scale = "RD",
                  new_name = "Loss psychopathy: partner relationship satisfacton")


out_tab_contrast_t2_partner_sat_relationship_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_partner_sat_relationship_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_partner_sat_relationship_z_1


# conflict
t2_partner_conflict_in_relationship_z<- here_read("t2_partner_conflict_in_relationship_z")
t2_partner_conflict_in_relationship_z_null<- here_read("t2_partner_conflict_in_relationship_z_null")
t2_partner_conflict_in_relationship_z_1<- here_read("t2_partner_conflict_in_relationship_z_1")

t2_partner_conflict_in_relationship_z
t2_partner_conflict_in_relationship_z_null
t2_partner_conflict_in_relationship_z_1



# first contrast 
contrast_t2_partner_conflict_in_relationship_z <-
  lmtp_contrast(t2_partner_conflict_in_relationship_z,
                ref = t2_partner_conflict_in_relationship_z_null,
                type = "additive")

tab_contrast_t2_partner_conflict_in_relationship_z <-
  margot_tab_lmtp(contrast_t2_partner_conflict_in_relationship_z,
                  scale = "RD",
                  new_name = "Gain psychopathy: partner conflict in relationship")


out_tab_contrast_t2_partner_conflict_in_relationship_z <-
  lmtp_evalue_tab(tab_contrast_t2_partner_conflict_in_relationship_z,
                  scale = c("RD"))

out_tab_contrast_t2_partner_conflict_in_relationship_z

# second contrast
contrast_t2_partner_conflict_in_relationship_z_1 <-
  lmtp_contrast(t2_partner_conflict_in_relationship_z_1,
                ref = t2_partner_conflict_in_relationship_z_null,
                type = "additive")

tab_contrast_t2_partner_conflict_in_relationship_z_1 <-
  margot_tab_lmtp(contrast_t2_partner_conflict_in_relationship_z_1,
                  scale = "RD",
                  new_name = "Loss psychopathy: partner conflict in relationship")


out_tab_contrast_t2_partner_conflict_in_relationship_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_partner_conflict_in_relationship_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_partner_conflict_in_relationship_z_1


# kessler 6
t2_partner_kessler6_sum_z <- here_read("t2_partner_kessler6_sum_z")
t2_partner_kessler6_sum_z_null <- here_read("t2_partner_kessler6_sum_z_null")
t2_partner_kessler6_sum_z_1 <- here_read("t2_partner_kessler6_sum_z_1")


contrast_t2_partner_kessler6_sum_z <-
  lmtp_contrast(t2_partner_kessler6_sum_z,
                ref = t2_partner_kessler6_sum_z_null,
                type = "additive")

tab_contrast_t2_partner_kessler6_sum_z<-
  margot_tab_lmtp(contrast_t2_partner_kessler6_sum_z,
                  scale = "RD",
                  new_name = "Gain psychopathy: partner Kessler 6 distress")


out_tab_contrast_t2_partner_kessler6_sum_z <-
  lmtp_evalue_tab(tab_contrast_t2_partner_kessler6_sum_z,
                  scale = c("RD"))

out_tab_contrast_t2_partner_kessler6_sum_z


# loss

contrast_t2_partner_kessler6_sum_z_1 <-
  lmtp_contrast(t2_partner_kessler6_sum_z_1,
                ref = t2_partner_kessler6_sum_z_null,
                type = "additive")

tab_contrast_t2_partner_kessler6_sum_z_1<-
  margot_tab_lmtp(contrast_t2_partner_kessler6_sum_z_1,
                  scale = "RD",
                  new_name = "Loss psychopathy: partner Kessler 6 distress")


out_tab_contrast_t2_partner_kessler6_sum_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_partner_kessler6_sum_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_partner_kessler6_sum_z_1


# depression
t2_partner_kessler_latent_depression_z <- here_read("t2_partner_kessler_latent_depression_z")
t2_partner_kessler_latent_depression_z_null <- here_read("t2_partner_kessler_latent_depression_z_null")
t2_partner_kessler_latent_depression_z_1 <- here_read("t2_partner_kessler_latent_depression_z_1")
             
             

# first contrast
contrast_t2_partner_kessler_latent_depression_z <-
  lmtp_contrast(t2_partner_kessler_latent_depression_z,
                ref = t2_partner_kessler_latent_depression_z_null,
                type = "additive")

tab_contrast_t2_partner_kessler_latent_depression_z <-
  margot_tab_lmtp(contrast_t2_partner_kessler_latent_depression_z,
                  scale = "RD",
                  new_name = "Gain psychopathy: partner Kessler 6 depression")


out_tab_contrast_t2_partner_kessler_latent_depression_z <-
  lmtp_evalue_tab(tab_contrast_t2_partner_kessler_latent_depression_z,
                  scale = c("RD"))

out_tab_contrast_t2_partner_kessler_latent_depression_z


# second contrast
contrast_t2_partner_kessler_latent_depression_z_1 <-
  lmtp_contrast(t2_partner_kessler_latent_depression_z_1,
                ref = t2_partner_kessler_latent_depression_z_null,
                type = "additive")

tab_contrast_t2_partner_kessler_latent_depression_z_1 <-
  margot_tab_lmtp(contrast_t2_partner_kessler_latent_depression_z_1,
                  scale = "RD",
                  new_name = "Loss psychopathy: partner Kessler 6 depression")


out_tab_contrast_t2_partner_kessler_latent_depression_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_partner_kessler_latent_depression_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_partner_kessler_latent_depression_z_1


#anxiety
t2_partner_kessler_latent_anxiety_z<- here_read("t2_partner_kessler_latent_anxiety_z")
t2_partner_kessler_latent_anxiety_z_1 <- here_read("t2_partner_kessler_latent_anxiety_z_1")
t2_partner_kessler_latent_anxiety_z_null <- here_read("t2_partner_kessler_latent_anxiety_z_null")
             
            
# first contrast
contrast_t2_partner_kessler_latent_anxiety_z <-
  lmtp_contrast(t2_partner_kessler_latent_anxiety_z,
                ref = t2_partner_kessler_latent_anxiety_z_null,
                type = "additive")

tab_contrast_t2_partner_kessler_latent_anxiety_z <-
  margot_tab_lmtp(contrast_t2_partner_kessler_latent_anxiety_z,
                  scale = "RD",
                  new_name = "Gain psychopathy: partner Kessler 6 anxiety")


out_tab_contrast_t2_partner_kessler_latent_anxiety_z <-
  lmtp_evalue_tab(tab_contrast_t2_partner_kessler_latent_anxiety_z,
                  scale = c("RD"))

out_tab_contrast_t2_partner_kessler_latent_anxiety_z

# second contrast
contrast_t2_partner_kessler_latent_anxiety_z_1 <-
  lmtp_contrast(t2_partner_kessler_latent_anxiety_z_1,
                ref = t2_partner_kessler_latent_anxiety_z_null,
                type = "additive")

tab_contrast_t2_partner_kessler_latent_anxiety_z_1 <-
  margot_tab_lmtp(contrast_t2_partner_kessler_latent_anxiety_z_1,
                  scale = "RD",
                  new_name = "Loss psychopathy: partner Kessler 6 anxiety")


out_tab_contrast_t2_partner_kessler_latent_anxiety_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_partner_kessler_latent_anxiety_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_partner_kessler_latent_anxiety_z_1


# self esteem
t2_partner_self_esteem_z<- here_read("t2_partner_self_esteem_z")
t2_partner_self_esteem_z_1<- here_read("t2_partner_self_esteem_z_1")
t2_partner_self_esteem_z_null<- here_read("t2_partner_self_esteem_z_null")


# first contrast
contrast_t2_partner_self_esteem_z <-
  lmtp_contrast(t2_partner_self_esteem_z,
                ref = t2_partner_self_esteem_z_null,
                type = "additive")


tab_contrast_t2_partner_self_esteem_z <-
  margot_tab_lmtp(contrast_t2_partner_self_esteem_z,
                  scale = "RD",
                  new_name = "Gain psychopathy: partner self-esteem")


out_tab_contrast_t2_partner_self_esteem_z <-
  lmtp_evalue_tab(tab_contrast_t2_partner_self_esteem_z,
                  scale = c("RD"))

out_tab_contrast_t2_partner_self_esteem_z



# second contrast
contrast_t2_partner_self_esteem_z_1 <-
  lmtp_contrast(t2_partner_self_esteem_z_1,
                ref = t2_partner_self_esteem_z_null,
                type = "additive")

tab_contrast_t2_partner_self_esteem_z_1<-
  margot_tab_lmtp(contrast_t2_partner_self_esteem_z_1,
                  scale = "RD",
                  new_name = "Loss psychopathy: partner self-esteem")

out_tab_contrast_t2_partner_self_esteem_z_1<-
  lmtp_evalue_tab(tab_contrast_t2_partner_self_esteem_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_partner_self_esteem_z_1

# pwi
t2_partner_pwi_z<- here_read("t2_partner_pwi_z")
t2_partner_pwi_z_1<- here_read("t2_partner_pwi_z_1")
t2_partner_pwi_z_null<- here_read("t2_partner_pwi_z_null")


# first contrast
contrast_t2_partner_pwi_z <-
  lmtp_contrast(t2_partner_pwi_z,
                ref = t2_partner_pwi_z_null,
                type = "additive")

tab_contrast_t2_partner_pwi_z <-
  margot_tab_lmtp(contrast_t2_partner_pwi_z,
                  scale = "RD",
                  new_name = "Gain psychopathy: partner pers. wellbeing")


out_tab_contrast_t2_partner_pwi_z <-
  lmtp_evalue_tab(tab_contrast_t2_partner_pwi_z,
                  scale = c("RD"))

out_tab_contrast_t2_partner_pwi_z

# second contrast
contrast_t2_partner_pwi_z_1 <-
  lmtp_contrast(t2_partner_pwi_z_1,
                ref = t2_partner_pwi_z_null,
                type = "additive")

tab_contrast_t2_partner_pwi_z_1 <-
  margot_tab_lmtp(contrast_t2_partner_pwi_z_1,
                  scale = "RD",
                  new_name = "Loss psychopathy: partner pers. wellbeing")


out_tab_contrast_t2_partner_pwi_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_partner_pwi_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_partner_pwi_z_1

# lifesat
t2_partner_lifesat_z <- here_read("t2_partner_lifesat_z")
t2_partner_lifesat_z_1<- here_read("t2_partner_lifesat_z_1")
t2_partner_lifesat_z_null<- here_read("t2_partner_lifesat_z_null")
             
            


# first contrast
contrast_t2_partner_lifesat_z <- lmtp_contrast(t2_partner_lifesat_z,
                                       ref = t2_partner_lifesat_z_null,
                                       type = "additive")

tab_contrast_t2_partner_lifesat_z <-
  margot_tab_lmtp(contrast_t2_partner_lifesat_z, scale = "RD", 
                  new_name = "Gain psychopathy: partner life satisfaction")


out_tab_contrast_t2_partner_lifesat_z <-
  lmtp_evalue_tab(tab_contrast_t2_partner_lifesat_z,
                  scale = c("RD"))

out_tab_contrast_t2_partner_lifesat_z

# second contrast
contrast_t2_partner_lifesat_z_1 <- lmtp_contrast(t2_partner_lifesat_z_1,
                                         ref = t2_partner_lifesat_z_null,
                                         type = "additive")

tab_contrast_t2_partner_lifesat_z_1 <-
  margot_tab_lmtp(contrast_t2_partner_lifesat_z_1, scale = "RD", new_name = "Loss psychopathy: partner life satisfaction")


out_tab_contrast_t2_partner_lifesat_z_1 <-
  lmtp_evalue_tab(tab_contrast_t2_partner_lifesat_z_1,
                  scale = c("RD"))

out_tab_contrast_t2_partner_lifesat_z_1



# make tables -------------------------------------------------------------

# bind individual tables
tab_outcomes_gain <- rbind(
  out_tab_contrast_t2_partner_conflict_in_relationship_z,  
  out_tab_contrast_t2_partner_sat_relationship_z,
  out_tab_contrast_t2_partner_kessler6_sum_z,
  out_tab_contrast_t2_partner_kessler_latent_depression_z,
  out_tab_contrast_t2_partner_kessler_latent_anxiety_z,
  out_tab_contrast_t2_partner_self_esteem_z,
  out_tab_contrast_t2_partner_pwi_z,
  out_tab_contrast_t2_partner_lifesat_z
)

here_save(tab_outcomes_gain,"tab_outcomes_gain")
tab_outcomes_gain

# make group table
group_tab_outcomes_gain <- group_tab(tab_outcomes_gain, type = "RD")

group_tab_outcomes_gain
here_save(group_tab_outcomes_gain,"group_tab_outcomes_gain")
group_tab_outcomes_gain <- here_read("group_tab_outcomes_gain")




# bind individual tables
tab_outcomes_loss <- rbind(
    out_tab_contrast_t2_partner_conflict_in_relationship_z_1,  
    out_tab_contrast_t2_partner_sat_relationship_z_1,
    out_tab_contrast_t2_partner_kessler6_sum_z_1,
    out_tab_contrast_t2_partner_kessler_latent_depression_z_1,
    out_tab_contrast_t2_partner_kessler_latent_anxiety_z_1,
    out_tab_contrast_t2_partner_self_esteem_z_1,
    out_tab_contrast_t2_partner_pwi_z_1,
    out_tab_contrast_t2_partner_lifesat_z_1
  )
tab_outcomes_loss
here_save(tab_outcomes_loss,"tab_outcomes_loss")

# make group table
group_tab_outcomes_loss <- group_tab(tab_outcomes_loss , type = "RD")

# save
here_save(group_tab_outcomes_loss, "group_tab_outcomes_loss")
group_tab_outcomes_loss <- here_read("group_tab_outcomes_loss")



# create plots -------------------------------------------------------------

# check N
N <- here_read("N_participants")
N = N * 2
N
sub_title = ""

push_mods
# graph health
conflicts_prefer(ggplot2::margin)
plot_group_tab_gain <- margot_plot(
  group_tab_outcomes_gain,
  type = "RD",
  title = "Gain +1 Psychopathy (Combined): N=1070",
  subtitle = sub_title,
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 12,
  text_size = 3.0,
  point_size = .5,
  title_size = 15,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)
plot_group_tab_gain
dev.off()
# save graph
ggsave(
  plot_group_tab_gain,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "plot_group_tab_gain.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)



# graph body
plot_group_tab_loss <- margot_plot(
  group_tab_outcomes_loss,
  type = "RD",
  title = "Loss +1 Psychopathy (Combined): N=1070",
  subtitle = sub_title,
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 12,
  text_size = 3.0,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)
plot_group_tab_loss
# save graph
ggsave(
  plot_group_tab_loss,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "plot_group_tab_loss.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)


plot_group_tab_loss

