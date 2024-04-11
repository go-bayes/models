# aaron-usa perfectionism, sexual satisfaction, relationship satisfaction.

#lmpt-ow-aaron-psychopath.R
# 2024 04 11 
# joseph bulbulia : joseph.bulbulia@gmail.com
# outcome-wide-analysis-psychopathy

### WARNING: THIS PATH WILL NOT WORK FOR YOU. PLEASE SET A PATH TO YOUR OWN COMPUTER!! ###
### WARNING: FOR EACH NEW STUDY SET UP A DIFFERENT PATH OTHERWISE YOU WILL WRITE OVER YOUR MODELS
push_mods <-
  fs::path_expand(
    "/Users/joseph/Library/CloudStorage/Dropbox-v-project/data/nzvs_mods/24/aaron-usa-perfect-on-relation-sat"
  )


# preliminaries -----------------------------------------------------------

source("/Users/joseph/GIT/templates/functions/libs2.R")

library("margot")
library("skimr")
# WARNING:  COMMENT THIS OUT. JB DOES THIS FOR WORKING WITHOUT WIFI
# source("/Users/joseph/GIT/templates/functions/funs.R")
# 
# # experimental functions
# source(
#   "/Users/joseph/GIT/templates/functions/experimental_funs.R"
# )
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
    "/Users/joseph/Library/CloudStorage/Dropbox-v-project/Joseph\ Bulbulia/00Bulbulia\ Pubs/DATA/nzavs-current/r-data/nzavs_data_qs"
  )

#

library(haven)
# dat <- margot::select_and_rename_cols(dat)

# read data: note that you need use the arrow package in R
dat <- qs::qread(pull_path)

# not working
# dat <- dat %>%
#   mutate(across(where(~inherits(.x, "labelled") || is.factor(.x)), 
#                 ~ if(is.factor(.x)) as.character(.x) else zap_labels(.x)))
# check path:is this correct?  check so you know you are not overwriting other directors
push_mods


# set exposure here
nzavs_exposure <- "perfectionism"


# define limits
max_score = 7
min_score = 1
# intervention 


# SHIFT INTERVENTION
# increase everyone by one point, contrasted with what they would be anyway.
shift_up <- function(data, trt) {
  ifelse(data[[trt]] <= max_score - 2, data[[trt]] + 2,  max_score)
}

# decrease everyone
shift_down <- function(data, trt) {
  ifelse(data[[trt]] >= min_score + 2, data[[trt]] - 2,  min_score)
}

dat$alert_level_combined
# define exposures --------------------------------------------------------
# define exposure
A <- c("t0_perfectionism", "t1_perfectionism")
L <- list(c(NULL), c("t1_alert_level_combined")) # COULD PUT NULL HERE
C <- c("t0_censored", "t1_censored")

# set exposure variable, can be both the continuous and the coarsened, if needed
exposure_var = c("perfectionism", "censored") #

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

# check options
listWrappers()
# super learner libraries
sl_lib <- c(
  "SL.ranger")


# superlearner libraries
library("SuperLearner")
library("ranger")
library("randomForest")
# library("rJava")
# library("bartMachineJARs")

#library(extraTrees)


n_total<- skimr::n_unique(dat$id)


here_save(n_total, "n_total")

# id number at baseline
ids_2018 <- dat |>
  filter(year_measured == 1,
         wave == 2018) |>
  filter(!is.na(perfectionism)) |> 
  pull(id)

# length
length(ids_2018)
  
# get ids at baseline
ids_at_baseline <- dat |>
  dplyr::filter(id %in% ids_2018 &
                  wave %in% c(2018, 2019, 2020)) |>
  droplevels() |> 
  arrange(id, wave)



# get data for study
dat_init_sample <- dat |>
  dplyr::filter(id %in% ids_2018 &
                  wave %in% c(2018, 2019, 2020)) |>
  droplevels() |> 
  dplyr::arrange(id, wave)





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


# check for a specific wave, e.g., 2018
dyad_counts <- count_dyads(dat_init_sample, waves = c(2018))

# 930 complete dyads in 2018
n_unique_complete_dyads <- dyad_counts$unique_complete_dyads

n_total_dyads
n_dyads <- dyad_counts$unique_complete_dyads

n_dyads

here_save(n_unique_complete_dyads, "n_unique_complete_dyads")

here_save(n_dyads, "n_dyads")
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



dyad_counts_table <- count_dyads_multi_wave(dat)
dyad_counts_table

# checks
initial_filter <- dat_init_sample %>%
  filter((wave %in% c(2018) & year_measured == 1) | 
           (wave %in% c(2019, 2020) )) %>%
  filter(!is.na(perfectionism))

glimpse(initial_filter)

initial_filter_dyads <- initial_filter %>% 
  filter(!is.na(rel_num_l))

time_order_debug <- initial_filter_dyads %>% 
  group_by(id, rel_num_l) %>% 
  summarise(waves = list(sort(as.numeric(as.character(unique(wave)))))) %>% 
  ungroup() %>%
  rowwise() %>% 
  filter(all(c(2018) %in% unlist(waves))) %>% 
  ungroup()


time_order_debug

time_order_check <- initial_filter %>% 
  group_by(id, rel_num_l) %>% 
  summarise(waves = list(sort(unique(wave)))) %>% 
  ungroup() %>%
  rowwise() %>% 
  filter(all(c(2018) %in% unlist(waves)) & 
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
  filter(all(c(2018) %in% unlist(waves))) %>% 
  ungroup()

final_dat <- dat_init_sample %>% 
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
dat_18_19 <- dat_init_sample |> 
  filter(wave %in% c(2018) & year_measured == 1 & !is.na(perfectionism))

#Group by id and find the number of unique waves they are part of.
id_count <- dat_18_19 |> 
  group_by(id) |> 
  summarise(n_unique_waves = n_distinct(wave), .groups = 'drop') |> 
  filter(n_unique_waves == 1)

valid_ids_1 <- id_count$id

dat_filtered <- dat_init_sample |> 
  filter(id %in% valid_ids_1 & wave %in% c(2018, 2019, 2020))

n_unique(dat_filtered$id)

# Group by rel_num_l and wave to find the number of individuals in each relationship for each wave.
rel_count <- dat_filtered |> 
  filter(wave %in% c(2018) & year_measured == 1 & !is.na(rel_num_l)) |>  # Only consider 2018 and 2019
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

hist( dat_final_dyadic$perfectionism )

na_at_base<- dat_final_dyadic |> 
  filter(wave == 2018)

# test
table(is.na(na_at_base$perfectionism))

# define exposure
nzavs_exposure <- "perfectionism"
exposure_var = c("alert_level_combined", "perfectionism", "censored") #

# check
table( dat_final_dyadic$wave)


#### SAVE DATA 
push_mods
margot::here_save(dat_final_dyadic, "dat_final_dyadic")

# dat_final_dyadic <- readRDS("/Users/joseph/Library/CloudStorage/Dropbox-v-project/data/lmtp/24/aaron_psychopathy/dat_final_dyadic")


## tests
# dat_final_dyadic <- readRDS("/Users/joseph/Library/CloudStorage/Dropbox-v-project/data/lmtp/24/aaron_psychopathy/dat_final_dyadic")
# table(test == dat_final_dyadic)
#                    

str(dat$alert_level_combined)
dat_long <- dat_final_dyadic |> 
  # ungroup
  select(
    "wave",
    "year_measured",
    "age",
    "id",
    "male",
    "education_level_coarsen",
    "sample_frame_opt_in",
    "nzsei_13_l",
    "rural_gch_2018_l",
    # Ordinal-Rank 0-10 NZREG codes (with overseas school quals coded as Level 3, and all other ancillary categories coded as missing)  Combined highschool levels See:https://www.nzqa.govt.nz/assets/Studying-in-NZ/New-Zealand-Qualification-Framework/requirements-nzqf.pdf
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
    # see nzavs materials
  #  "partner",
    # 0 = no, 1 = yes
    "parent",
    # 0 = no, 1 = yes
  # Please estimate your total household income (before tax) for the last year.
    "nz_dep2018",
  # see nzavs materials
    "nzsei_13_l",
    "political_conservative",
    #Please rate how politically liberal versus conservative you see yourself as being.
    #"pol_wing",
    # Please rate how politically left-wing versus right-wing you see yourself as being.
    # see NZAVS,
    "has_siblings",
    #Do you have siblings?
    "children_num",
    # How many children have you given birth to, fathered, or adopted?
    "hours_children",
    #Hours - Looking after children
    "hours_work",
    #Hours - Working in paid employment
    "hours_housework",
    # Hours - Housework/cooking
    "religion_church_binary",
    # sample_weights.
    "modesty",
    "hours_exercise",
    # depression constructs,
    "kessler_latent_depression",
    "kessler_latent_anxiety",
   "perfectionism",
    # During the last 30 days, how often did.... you have negative thoughts that repeated over and over?
    # "kessler_depressed",
    # #During the last 30 days, how often did.... you feel so depressed that nothing could cheer you up?
    # "kessler_effort",
    # #During the last 30 days, how often did.... you feel that everything was an effort?
    # "kessler_hopeless",
    # # During the last 30 days, how often did.... you feel hopeless?
    # "kessler_nervous",
    # #During the last 30 days, how often did.... you feel nervous?
    # "kessler_restless",
    # #During the last 30 days, how often did.... you feel restless or fidgety?
    # "kessler_worthless",
    # # During the last 30 days, how often did.... you feel worthless?
  "agreeableness",
  # Mini-IPIP6 Agreeableness (also modelled as empathy facet)
  # Sympathize with others' feelings.
  # Am not interested in other people's problems.
  # Feel others' emotions.
  # Am not really interested in others.
  "conscientiousness",
  # see mini ipip6
  # Get chores done right away.
  # Like order.
  # Make a mess of things.
  # Often forget to put things back in their proper place.
  "extraversion",
  # Mini-IPIP6 Extraversion
  # Am the life of the party.
  # Don't talk a lot.
  # Keep in the background.
  # Talk to a lot of different people at parties.
  "honesty_humility",
  # see mini ipip6
  # Would like to be seen driving around in a very expensive car.
  # Would get a lot of pleasure from owning expensive luxury goods.
  # Feel entitled to more of everything.
  # Deserve more things in life.
  "openness",
  # see mini ipip6
  # Have a vivid imagination.
  # Have difficulty understanding abstract ideas.
  # Do not have a good imagination.
  # Am not interested in abstract ideas.
  "neuroticism",
  # see mini ipip6
  # Have frequent mood swings.
  # Am relaxed most of the time.
  # Get upset easily.
  # Seldom feel blue.
  # "modesty",
  # # see mini ipip6
  # # I want people to know that I am an important person of high status,
  # # I am an ordinary person who is no better than others.
  # # I wouldn’t want people to treat me as though I were superior to them.
  # # I think that I am entitled to more respect than the average person is
  # "sdo",
    "sexual_satisfaction",
    #  How satisfied are you with your sex life?
    "bodysat",
    ## Am satisfied with the appearance, size and shape of my body.
    # "vengeful_rumin",
   # "self_esteem",
    # "selfesteem_satself",
    # #  On the whole am satisfied with myself.
    # "selfesteem_postiveself",
    # # Take a positive attitude toward myself
    # "selfesteem_failure_reversed",
   # "pwi",
    # "pwb_your_health",
    # # #Your health.
    # "pwb_your_relationships",
    # # #Your personal relationships.
    # "pwb_your_future_security",
    # # #Your future security.
    # "pwb_standard_living",
    # #Your standard of living.
    # "lifesat",
   # "lifesat_satlife",
    # I am satisfied with my life.
   # "lifesat_ideal",
    "sat_relationship",
    "conflict_in_relationship",
   # "kessler_latent_anxiety",
  #  "kessler_latent_depression",
    # "alert_level_combined_lead",
    # "rel_num_l",
    # "aaron_antagonism",
    # "aaron_disinhibition",
    # "aaron_emotional_stability",
    # "aaron_narcissism", 
   "sample_origin_names_combined",
    "alert_level_combined",
  "w_gend_age_ethnic",
  "rel_num_l"
  ) |>
  mutate(
    #initialize 'censored'
    censored = ifelse(lead(year_measured) == 1, 1, 0),
    
    # modify 'censored' based on the condition; no need to check for NA here as 'censored' is already defined in the previous step
    censored =  ifelse(is.na(censored) &
                         year_measured == 1, 1, censored)
    
    # # Apply the case_when condition for setting 'censored' based on 'wave' and the dynamic column specified by 'nzavs_exposure'
    # censored = case_when(
    #   # Add this condition to keep previous modifications unless the specific condition is met!is.na(censored) ~ censored,
    #
    #   # Then check if 'wave' is 2019 and the specified exposure is NA, adjusting the condition to reflect the accurate logic
    #   wave == 2019 & !is.na(!!sym(nzavs_exposure)) ~ 1,
    #
    #   # Default case if none of the above apply; might not be necessary if all possibilities are covered
    #   TRUE ~ 0
    # )
  ) |>
  select(-year_measured) |>
  # mutate(
  #   eth_cat = as.integer(eth_cat),
  #   urban = as.numeric(urban),
  #   education_level_coarsen = as.integer(education_level_coarsen)
  # ) |>
  dplyr::mutate(
    # friends_money = ifelse(friends_money < 0, 0, friends_money),
    # someone gave neg number
    household_inc_log = log(household_inc + 1),
    hours_children_log = log(hours_children + 1),
    hours_work_log = log(hours_work + 1),
    hours_housework_log = log(hours_housework + 1),
    hours_exercise_log = log(hours_exercise + 1)
  ) |>
  dplyr::select( -c(household_inc, hours_children, hours_work, hours_housework, hours_exercise)) |> 
  dplyr::rename(sample_weights = w_gend_age_ethnic,
                sample_origin =  sample_origin_names_combined) |>
  arrange(id, wave) |>
  droplevels() |>
  arrange(id, wave) |>
  # mutate(
  #   religion_church_coarsen = cut(
  #     religion_church,
  #     breaks = c(-Inf, 0, 1, 3.99, Inf),
  #     labels = c("zero", "one", "less_four", "four_up"),
  #     include.lowest = TRUE,
  #     right = TRUE,
  #     ordered = TRUE
  #   )) %>%
  # mutate(
  #   religion_church_coarsen_n = as.numeric(religion_church_coarsen) - 1,
  # ) |>
  droplevels() |>
  arrange(id, wave) |>
  data.frame()

str(dat_long)
# Directly specify the conversion for variables that did not convert

dat_long$perfectionism
# check sample 
n_participants <-n_unique(dat_long$id) #514 
n_participants

here_save(n_participants, "n_participants")

# eyeball distribution
# table(dat_long$wave)
dt_19 <- dat_long |>
  filter(wave == 2019) 

dt_20 <- dat_long |>
  filter(wave == 2020) 


# set min and max score for shift functions
min_score <- min(dt_19$perfectionism, na.rm = TRUE)
min_score

max_score <- max(dt_19$perfectionism, na.rm = TRUE)
max_score



# save means and sds of outcomes in the end-of-study wave
# sds
sd_sexual_satisfaction <- sd(dt_20$sexual_satisfaction, na.rm=TRUE)
sd_conflict_in_relationship <- sd(dt_20$conflict_in_relationship, na.rm=TRUE)
sd_sat_relationship <- sd(dt_20$sat_relationship)


# means
mean_sexual_satisfaction <- mean(dt_20$sexual_satisfaction, na.rm=TRUE)
mean_conflict_in_relationship <- mean(dt_20$conflict_in_relationship, na.rm=TRUE)
mean_sat_relationship <- mean(dt_20$sat_relationship, na.rm=TRUE)

yola

margot::here_save(sd_sexual_satisfaction, "sd_sexual_satisfaction")
margot::here_save(sd_conflict_in_relationship, "sd_conflict_in_relationship")
margot::here_save(sd_sat_relationship, "sd_sat_relationship")

margot::here_save(mean_sexual_satisfaction, "mean_sexual_satisfaction")
margot::here_save(mean_conflict_in_relationship, "mean_conflict_in_relationship")
margot::here_save(mean_sat_relationship, "mean_sat_relationship")



# baseline vars -----------------------------------------------------------

# check
table(dat_long$censored)

# select vars for baseline
dat_long_colnames <- colnames(dat_long)

dat_long_colnames <- sort(dat_long_colnames)

dat_long_colnames


# set baseline exposure and outcomes --------------------------------------

exposure_var = c("perfectionism",
                 "alert_level_combined",
                 "censored"#,
                 # "hours_community_round"
) #


# set outcomes for prosocial domain
# save prejudice for separate paper
outcome_vars = c(
  "sexual_satisfaction",
  "conflict_in_relationship",
  "sat_relationship"
)

dat_long_colnames <- colnames(dat_long)
#
baseline_vars <-
  setdiff(dat_long_colnames,
          c("id","wave"))

# c(outcome_vars, 'id', 'wave'))
baseline_vars
baseline_vars <- sort(baseline_vars)

baseline_vars

# just core baseline variables
base_var <-
  setdiff(baseline_vars, c("censored", "sample_weights", "alert_level_combined", 
                           "rel_num_l", outcome_vars))
base_var


#community at baseline
n_participants <-
  n_unique(dat_long$id) #47202 # reports hours with

# check
n_participants

margot::here_save(n_participants, "n_participants")

# double check path
push_mods

# check col names
colnames(dat)

# assess positivity
dat_long$wave

dt_positivity_full <- dat_long |>
  filter(wave == 2018 | wave == 2019) |>
  mutate(perfectionism_round = round(perfectionism, digits = 0)) |> 
  select(wave, id, perfectionism_round) 



# create transition matrix
out <-
  msm::statetable.msm(perfectionism_round, id, data = dt_positivity_full)


library(margot)



out_exposure <-
  msm::statetable.msm(perfectionism_round, id, data = dt_positivity_full)

out <-margot::create_transition_matrix(data = dt_positivity_full, state_var = "perfectionism_round", id_var = "id")

out

# t_tab_2_labels <- c("< weekly", ">= weekly")
# transition table

transition_table  <- margot::transition_table(out)
transition_table
# for import later
margot::here_save(transition_table, "transition_table")



# sd values ---------------------------------------------------------------

dt_outcome <-
  dat_long |>
  filter(wave == 2020)



# check
baseline_vars


# remove haven labels -----------------------------------------------------

dat_long <- haven::zap_labels(dat_long)

#
# check associations only -------------------------------------------------

dt_18 <- dat_long|>
  filter(wave == 2018) 


table(dt_18$censored)



# check association only
#dt_18_miss$sample_weights


naniar::vis_miss(dt_18, warn_large_data = F)
outcome_vars

fit_sexual_satisfaction
# base_vars set above
fit_sexual_satisfaction <-
  margot::regress_with_covariates(
    dt_18,
    outcome = "sexual_satisfaction",
    exposure = "perfectionism",
    baseline_vars = base_var,
    sample_weights = "sample_weights"
  )
parameters::model_parameters(fit_sexual_satisfaction, ci_method="wald")[2, ] 
margot::here_save(fit_sexual_satisfaction, "fit_sexual_satisfaction")

#fit_church_on_hours_charity
#(0.198274  + 0.168511) * 60

fit_conflict_in_relationship <-
  margot::regress_with_covariates(
    dt_18,
    outcome = "conflict_in_relationship",
    exposure = "perfectionism",
    baseline_vars = base_var,
    sample_weights = "sample_weights"
  )
parameters::model_parameters(fit_conflict_in_relationship, ci_method="wald")[2, ]
margot::here_save(fit_conflict_in_relationship, "fit_conflict_in_relationship")

fit_sat_relationship <-
  margot::regress_with_covariates(
    dt_18,
    outcome = "sat_relationship",
    exposure = "perfectionism",
    baseline_vars = base_var,
    sample_weights = "sample_weights"
  )
parameters::model_parameters(fit_sat_relationship, ci_method="wald")[2, ]
here_save(fit_sat_relationship, "fit_sat_relationship")



fit_sat_relationship<- margot::here_read('fit_sat_relationship')
fit_conflict_in_relationship<- margot::here_read('fit_conflict_in_relationship')
fit_sexual_satisfaction<- margot::here_read('fit_sexual_satisfaction')

# run once then comment out

# lm_coef_fit_church_on_charity_donate <- tbl_regression(fit_church_on_charity_donate)
# b_church_on_charity_donate <-inline_text(lm_coef_fit_church_on_charity_donate, variable = religion_church_round, pattern = "b = {estimate}; (95% CI {conf.low}, {conf.high})")
# # #
#  b_church_on_charity_donate
# here_save(b_church_on_charity_donate, "b_church_on_charity_donate")
# 
# lm_coef_fit_church_on_hours_charity <- tbl_regression(fit_church_on_hours_charity)
# lm_coef_fit_church_on_hours_charity
# b_church_on_hours_charity <-inline_text(lm_coef_fit_church_on_hours_charity, variable = religion_church_round, pattern = "b = {estimate}; (95% CI {conf.low}, {conf.high})")
# b_church_on_hours_charity
# # b_church_on_charity_donate
# here_save(b_church_on_hours_charity, "b_church_on_hours_charity")

# tables ------------------------------------------------------------------
library(gtsummary)



# table baseline ----------------------------------------------------------



# get names
base_var

# prepare df
selected_base_cols <-
  dt_18 |> select(all_of(base_var))


#check
colnames(selected_base_cols)

#chck
#selected_base_cols <- setdiff(selected_base_cols)
# baseline table

library(gtsummary)




table_baseline <- selected_base_cols |> 
  janitor::clean_names(case = "title") |> 
  tbl_summary(
    missing = "ifany",
    percent = "column",
    statistic = list(
      all_continuous() ~ c(
        "{mean} ({sd})", # Mean and SD
        "{min}, {max}", # Range (Min, Max)
        "{p25}, {p75}" # IQR (25th percentile, 75th percentile)
      )
    ),
    type = all_continuous() ~ "continuous2"
  ) |>
  modify_header(label = "**Exposure + Demographic Variables**") |> # update the column header
  bold_labels() 



table_baseline
# save baseline
here_save(table_baseline, "table_baseline")
# 

# table exposure ----------------------------------------------------------

# get first and second wave
dt_18_19 <- dat_long|> 
  dplyr::filter(wave == 2018 | wave == 2019) |> 
  droplevels()

# get vars.
selected_exposure_cols <-
  dt_18_19 %>% select(
    c(
      "perfectionism",
      "alert_level_combined",
      "wave"
    )
  )

# check
str(selected_exposure_cols)


library(gtsummary)

table_exposures <- selected_exposure_cols %>%
  janitor::clean_names(case = "title") %>% 
  labelled::to_factor() %>%  # ensure consistent use of pipe operator
  tbl_summary(
    by = "Wave",  #specify the grouping variable. Adjust "Wave" to match the cleaned column name
    missing = "always", 
    percent = "column",
    # statistic = list(all_continuous() ~ "{mean} ({sd})")  # Uncomment and adjust if needed for continuous variables
  ) %>%
  #  add_n() %>%  # Add column with total number of non-missing observations
  modify_header(label = "**Exposure Variables by Wave**") %>%  # Update the column header
  bold_labels()

table_exposures


# save baseline
here_save(table_exposures, "table_exposures")

table_exposures


# outcome table -----------------------------------------------------------
dt_18_20 <- dat_long |> 
  dplyr::filter(wave == 2018 | wave == 2020) |> 
  droplevels()

names_outcomes_tab <- setdiff(outcome_vars, dt_18_20)
names_outcomes_sorted <- sort(names_outcomes_tab)
names_outcomes_final <- names_outcomes_sorted # consistent workflow

names_outcomes_final

names_outcomes_final



# names_outcomes_final
# better names
selected_outcome_cols <-
  dt_18_20 %>% select(all_of(names_outcomes_final),
                      wave)
# order names correctly
selected_outcome_cols <- selected_outcome_cols %>%
  select(sort(names(selected_outcome_cols)))

# checks
str(selected_outcome_cols)
colnames(selected_outcome_cols)

table_outcomes <- selected_outcome_cols %>%
  janitor::clean_names(case = "title") %>% 
  labelled::to_factor() %>%  # ensure consistent use of pipe operator
  tbl_summary(
    by = "Wave",  #specify the grouping variable. Adjust "Wave" to match the cleaned column name
    missing = "always", 
    percent = "column",
    # statistic = list(all_continuous() ~ "{mean} ({sd})")  # Uncomment and adjust if needed for continuous variables
  ) %>%
  #  add_n() %>%  # Add column with total number of non-missing observations
  modify_header(label = "**Outcome Variables by Wave**") %>%  # Update the column header
  bold_labels()

table_outcomes


here_save(table_outcomes, "table_outcomes")
table_outcomes <- here_read("table_outcomes")
table_outcomes
# histogram exposure ------------------------------------------------------

dt_19 <- dat_long |> 
  filter(wave == 2019)

library(ggplot2)
library(dplyr)
#

sd(dt_19$perfectionism, na.rm=TRUE)
coloured_histogram()
# # generate bar plot
graph_density_of_exposure_both <- margot::coloured_histogram(
  dt_19,
  unit_of_change = 2,
  col_name = "perfectionism",
  highlight_range = "both",
  binwidth = .1
)
graph_density_of_exposure_both


here_save(graph_density_of_exposure_both, "graph_density_of_exposure_both")
exposure_var

str(dat_long)
dat_long_df <- data.frame(dat_long)

my_data_filtered <- as.data.frame(dat_long_df)
my_data_filtered <- haven::zap_formats(dat_long_df)
my_data_filtered <- haven::zap_label(dat_long_df)
my_data_filtered <- haven::zap_widths(dat_long_df)

dat_long_df <- data.frame(my_data_filtered)
dat_long_df <- margot::remove_numeric_attributes(dat_long_df)
str(dat_long_df)
dat_long_df<-data.frame(dat_long_df)

# dat_long <- dat_long_df %>%
#   mutate(
#     parent = as.numeric(zap_labels(has_siblings)),
#     employed = as.character(zap_labels(employed)),
#     wscore = as.numeric(zap_labels(wscore)),
#     gender = as.character(zap_labels(gender))
#     # Add similar lines for other variables that need direct conversion

naniar::vis_miss(dat_long_df, warn_large_data = FALSE)
exposure_var

outcome_vars
library(mice)
str(dat_long_df)

prep_coop_all <- margot::margot_wide_impute_baseline(dat_long_df, baseline_vars = baseline_vars,
    exposure_var = exposure_var,outcome_vars = outcome_vars)

prep_coop_all$t0_sample_weights
prep_coop_all$t0_alert_level_combined
prep_coop_all$t1_alert_level_combined


# save function -- will save to your "push_mod" directory
margot::here_save(prep_coop_all, "prep_coop_all")

# check mi model
outlist <-
  row.names(prep_coop_all)[prep_coop_all$outflux < 0.5]
.length(outlist)

# checks. We do not impute with weights: area of current research
head(prep_coop_all$loggedEvents, 10)

# 
# Warning messages:
#   1: Number of logged events: 190 
# 2: Using an external vector in selections was deprecated in tidyselect 1.1.0.
# ℹ Please use `all_of()` or `any_of()` instead.
# # Was:
# data %>% select(t0_column_order)
# 
# # Now:
# data %>% select(all_of(t0_column_order))
# 
# See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.


str(prep_coop_all$t0_rel_num_l)

prep_coop_all$id <- as.character(prep_coop_all$id)
prep_coop_all$t0_rel_num_l <- as.numeric(prep_coop_all$t0_rel_num_l)  # Ensure this is numeric if not already

partner_mapping <- prep_coop_all %>%
  select(id, t0_rel_num_l) %>%
  arrange(t0_rel_num_l, id) %>%
  group_by(t0_rel_num_l) %>%
  mutate(partner_id = lead(id, default = first(id))) %>%
  ungroup()

prep_coop_all_with_partners <- prep_coop_all %>%
  left_join(partner_mapping, by = "id") %>%
  left_join(prep_coop_all, by = c("partner_id" = "id"), suffix = c("", "_partner")) |> 
  arrange(t0_rel_num_l) |> 
  select(-t0_rel_num_l.y)

head(prep_coop_all_with_partners)
str(prep_coop_all_with_partners)

# validate
# Example to check for NA values in partner columns
na_counts <- sapply(prep_coop_all_with_partners, function(x) sum(is.na(x)))
na_counts_partner_columns <- na_counts[grep("_partner$", names(na_counts))]
na_counts_partner_columns


# TEST COde
library(testthat)


test_that("Partner columns for j are correctly assigned to i", {
  # Define the columns to test within the test to ensure visibility
  columns_to_test <- c("t0_age", "t0_male", "t0_education_level_coarsen")
  partner_columns_to_test <- paste0(columns_to_test, "_partner")
  
  for (col in seq_along(columns_to_test)) {
    for (row in 1:nrow(prep_coop_all_with_partners)) {
      # Extract the partner's ID for the current row
      partner_id <- prep_coop_all_with_partners$partner_id[row]
      
      if (!is.na(partner_id) && partner_id != "") {
        # Find the row corresponding to the partner's ID
        partner_row_index <- which(prep_coop_all_with_partners$id == partner_id)
        
        if (length(partner_row_index) == 1) {  # Ensure exactly one match is found
          # Compare the original data for the partner with the corresponding partner data for the individual
          original_value <- prep_coop_all_with_partners[[columns_to_test[col]]][partner_row_index]
          partner_value <- prep_coop_all_with_partners[[partner_columns_to_test[col]]][row]
          
          # Assert that the original value for the partner matches the partner value for the individual
          expect_equal(original_value, partner_value,
                       info = paste("Mismatch in", columns_to_test[col], "for row", row, "and partner row", partner_row_index))
        }
      }
    }
  }
})



# read function
margot::here_save(prep_coop_all_with_partners, "prep_coop_all_with_partners")

prep_coop_all_with_partners <-margot::here_read( "prep_coop_all_with_partners")

head(prep_coop_all_with_partners)
naniar::vis_miss(prep_coop_all_with_partners, warn_large_data = FALSE)
dev.off()

table(prep_coop_all_with_partners$t0_censored)
table(prep_coop_all_with_partners$t0_censored_partner)

#check must be a dataframe
str(prep_coop_all)
nrow(prep_coop_all)





# spit shine --------------------------------------------------------------
df_wide_censored <- prep_coop_all_with_partners |>
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate("t0_censored", .before = starts_with("t1_")) |>
  relocate("t0_censored_partner", .before = starts_with("t1_")) |>
  relocate("t1_censored", .before = starts_with("t2_")) |>
  relocate("t1_censored_partner", .before = starts_with("t2_"))

# check
naniar::vis_miss(df_wide_censored, warn_large_data = FALSE)

table(df_wide_censored$t0_censored)
outcome_vars

outcome_vars
str(df_wide_censored)

library(dplyr)


library(dplyr)

# Assuming df_wide_censored is your dataframe

# Calculate the conditions before the mutate steps
t0_na_condition <-
  rowSums(is.na(select(df_wide_censored, starts_with("t1_")))) > 0
t1_na_condition <-
  rowSums(is.na(select(df_wide_censored, starts_with("t2_")))) > 0


df_clean <- df_wide_censored %>%
  mutate(t0_censored = ifelse(t0_na_condition, 0, t0_censored)) %>%
  mutate(t1_censored = ifelse(t1_na_condition, 0, t1_censored)) %>%
  mutate(across(starts_with("t1_"), ~ ifelse(t0_censored == 0, NA_real_, .)),
         across(starts_with("t2_"), ~ ifelse(t0_censored == 0, NA_real_, .))) %>%
  mutate(across(starts_with("t2_"), ~ ifelse(t1_censored == 0, NA_real_, .))) |>
  select(-c(ends_with("_volunteers_binary"))) |> 
  # select variables
  dplyr::mutate(
    across(
      .cols = where(is.numeric) &
        !t0_perfectionism & 
        !t0_perfectionism_partner & 
        # !t0_nzsei_13_l & 
        !t0_rel_num_l & 
        !t0_sample_weights & 
        !t0_sample_weights_partner & 
        !t0_rural_gch_2018_l & 
        !t0_rural_gch_2018_l_partner & 
        !t0_nzsei_13_l& 
        !t0_nzsei_13_l_partner& 
        !t0_sample_frame_opt_in & 
        !t0_sample_frame_opt_in_partner & 
        !t0_religion_church_binary &
        !t0_religion_church_binary_partner &
        #  !t0_charity_donate & !t0_sample_weights &
        !t0_censored &
        !t0_censored_partner &
        !t1_perfectionism & 
        !t1_perfectionism_partner & 
        !t1_alert_level_combined &
        !t1_alert_level_combined_partner &
        !t1_censored &
        !t1_censored_partner,
      .fns = ~ scale(.),
      .names = "{.col}_z"
    )
  ) |>
  # select(-t0_charity_donate,
  #        -t0_hours_charity) |>
  select(
    where(is.factor),
    t0_perfectionism,
    t0_perfectionism_partner,
      # !t0_nzsei_13_l & 
    t0_rel_num_l,
    t0_sample_weights,
    t0_sample_weights_partner,
    t0_rural_gch_2018_l,
    t0_rural_gch_2018_l_partner,
    t0_nzsei_13_l,
    t0_nzsei_13_l_partner,
    t0_sample_frame_opt_in,
    t0_sample_frame_opt_in_partner,
    t0_religion_church_binary,
    t0_religion_church_binary_partner,
      #  !t0_charity_donate & !t0_sample_weights &
    t0_censored,
    t0_censored_partner,
    t1_perfectionism,
    t1_perfectionism_partner,
    t1_alert_level_combined,
    t1_alert_level_combined_partner,
    t1_censored,
    t1_censored_partner,
    ends_with("_z")
  ) |>
  select(-"t0_rel_num_l.x_z") |> 
  relocate(starts_with("t0_"), .before = starts_with("t1_"))  %>%
  relocate(starts_with("t2_"), .after = starts_with("t1_"))  %>%
  relocate("t0_censored", .before = starts_with("t1_"))  %>%
  relocate("t0_censored_partner", .before = starts_with("t1_"))  %>%
  relocate("t1_censored", .before = starts_with("t2_")) |>
  relocate("t1_censored_partner", .before = starts_with("t2_")) |>
  mutate(t0_sample_weights = as.numeric(t0_sample_weights)) |>
  data.frame()





here_save(df_clean, "df_clean")
# read data --  start here if previous work already done


# imputed data already from previous study
df_clean <- here_read("df_clean")
colnames(df_clean)
str(df_clean)
# names of vars for modelling


names_base <-
  df_clean |> select(starts_with("t0"),
                     -t0_sample_weights,
                     -t0_perfectionism, # not need because we are shifting this
                     -t0_rel_num_l,
                     -t0_censored) |> colnames()

names_base

here_save(names_base, "names_base")
names_base <- here_read("names_base")

push_mods
names_outcomes <-
  df_clean |> select(starts_with("t2")) |> colnames()

names_outcomes

here_save(names_outcomes, "names_outcomes")
names_outcomes <- here_read("names_outcomes")

names_base
df_clean$t0_sample_weights


## MODELS 



# check vars
A
L
C

df_clean
# check balance at baseline -----------------------------------------------


df_clean_no_na_treatment <- df_clean |> filter(!is.na(t1_perfectionism))

df_clean_no_na_treatment_base <- df_clean |> filter(!is.na(t0_perfectionism)) 

names_base_base
names_base_base <- setdiff(names_base, "t0_perfectionism")

match_ebal_base<- margot::match_mi_general(data = df_clean_no_na_treatment_base,
                                           X = "t0_perfectionism",
                                           baseline_vars = names_base_base,
                                           estimand = "ATE",
                                           #  focal = 0, #for ATT
                                           method = "ebal",
                                           sample_weights = "t0_sample_weights")

love_plot_base <- love.plot(match_ebal_base, binary = "std", thresholds = c(m = .1),
                            wrap = 50, position = "bottom", size = 3,  limits = list(m = c(-.5, 1))) 

# love_plot_base
here_save(love_plot_base, "love_plot_base")

summary_match_ebal_base <- summary(match_ebal_base)
summary_match_ebal_base
bal.tab(match_ebal_base,  un = TRUE, thresholds = c(m = .05, v = 2))

here_save(summary_match_ebal_base, "summary_match_ebal_base")


match_ebal_one <- match_mi_general(data = df_clean_no_na_treatment,
                              X = "t1_perfectionism",
                              baseline_vars = names_base,
                              estimand = "ATE",
                              #  focal = 0, #for ATT
                              method = "ebal",
                              sample_weights = "t0_sample_weights")

match_ebal_one
# save
here_save(match_ebal_one, "match_ebal_one")



bal.tab(match_ebal_one)
love_plot_one <- love.plot(match_ebal_one, binary = "std", thresholds = c(m = .1),
                       wrap = 50, position = "bottom", size = 3,  limits = list(m = c(-.5, 1))) 
love_plot_one
here_save(love_plot_one, "love_plot_one")

# consider results 
summary_match_ebal_one <- summary(match_ebal_one)
summary_match_ebal_one
here_save(summary_match_ebal_one, "summary_match_ebal_one")

plot(summary_match_ebal_one)
# models ------------------------------------------------------------------
library(lmtp)


#  check shift
shift_up # by two
sl_lib
t2_sat_relationship_partner_z_gain <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_sat_relationship_partner_z",
  cens = C,
  shift = shift_up,
  mtp = TRUE,
  folds = SL_folds,
  # trim = 0.99, # if needed
  time_vary = L,
  outcome_type = "continuous",
  id = "t0_rel_num_l",
 # weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_sat_relationship_partner_z_gain
here_save(t2_sat_relationship_partner_z_gain, "t2_sat_relationship_partner_z_gain")


t2_sat_relationship_partner_z_loss <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_sat_relationship_partner_z",
  cens = C,
  shift = shift_down,
  mtp = TRUE,
  folds = SL_folds,
  # trim = 0.99, # if needed
  time_vary = L,
  outcome_type = "continuous",
  id = "t0_rel_num_l",
#  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_sat_relationship_partner_z_loss
here_save(t2_sat_relationship_partner_z_loss, "t2_sat_relationship_partner_z_loss")

t2_sat_relationship_partner_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_sat_relationship_partner_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = SL_folds,
  # trim = 0.99, # if needed
  time_vary = L,
  outcome_type = "continuous",
  id = "t0_rel_num_l",
 # weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_sat_relationship_partner_z_null
here_save(t2_sat_relationship_partner_z_null, "t2_sat_relationship_partner_z_null")



t2_conflict_in_relationship_z_gain <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_conflict_in_relationship_z",
  cens = C,
  shift = shift_up,
  mtp = TRUE,
  folds = SL_folds,
  # trim = 0.99, # if needed
  time_vary = L,
  outcome_type = "continuous",
  id = "t0_rel_num_l",
#  weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_conflict_in_relationship_z_gain
here_save(t2_conflict_in_relationship_z_gain, "t2_conflict_in_relationship_z_gain")



t2_conflict_in_relationship_z_loss <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_conflict_in_relationship_z",
  cens = C,
  shift = shift_down,
  mtp = TRUE,
  folds = SL_folds,
  # trim = 0.99, # if needed
  time_vary = L,
  outcome_type = "continuous",
  id = "t0_rel_num_l",
  # weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_conflict_in_relationship_z_loss
here_save(t2_conflict_in_relationship_z_loss, "t2_conflict_in_relationship_z_loss")


t2_conflict_in_relationship_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_conflict_in_relationship_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = SL_folds,
  # trim = 0.99, # if needed
  time_vary = L,
  outcome_type = "continuous",
  id = "t0_rel_num_l",
 # weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_conflict_in_relationship_z_null
here_save(t2_conflict_in_relationship_z_null, "t2_conflict_in_relationship_z_null")



t2_sexual_satisfaction_z_gain <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_sexual_satisfaction_z",
  cens = C,
  shift = shift_up,
  mtp = TRUE,
  folds = SL_folds,
  # trim = 0.99, # if needed
  time_vary = L,
  outcome_type = "continuous",
  id = "t0_rel_num_l",
 # weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_sexual_satisfaction_z_gain
here_save(t2_sexual_satisfaction_z_gain, "t2_sexual_satisfaction_z_gain")


t2_sexual_satisfaction_z_loss <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_sexual_satisfaction_z",
  cens = C,
  shift = shift_down,
  mtp = TRUE,
  folds = SL_folds,
  # trim = 0.99, # if needed
  time_vary = L,
  outcome_type = "continuous",
  id = "t0_rel_num_l",
 # weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_sexual_satisfaction_z_loss
here_save(t2_sexual_satisfaction_z_loss, "t2_sexual_satisfaction_z_loss")


t2_sexual_satisfaction_z_null <- lmtp_tmle(
  data = df_clean,
  trt = A,
  baseline = names_base,
  outcome = "t2_sexual_satisfaction_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = SL_folds,
  # trim = 0.99, # if needed
  time_vary = L,
  outcome_type = "continuous",
  id = "t0_rel_num_l",
 # weights = df_clean$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib,
  parallel = n_cores
)


t2_sexual_satisfaction_z_null
here_save(t2_sexual_satisfaction_z_null, "t2_sexual_satisfaction_z_null")



# contrasts ------------------------------------------------------------------------
t2_sat_relationship_partner_z_gain<- here_read("t2_sat_relationship_partner_z_gain")
t2_sat_relationship_partner_z_null<- here_read("t2_sat_relationship_partner_z_null")
t2_conflict_in_relationship_z_gain<- here_read("t2_conflict_in_relationship_z_gain")
t2_conflict_in_relationship_z_null<- here_read("t2_conflict_in_relationship_z_null")

t2_sexual_satisfaction_z_gain<- here_read("t2_sexual_satisfaction_z_gain")
t2_sexual_satisfaction_z_null<- here_read("t2_sexual_satisfaction_z_null")

# first contrast gain
contrast_t2_sat_relationship_partner_z_gain <-
  lmtp_contrast(t2_sat_relationship_partner_z_gain,
                ref = t2_sat_relationship_partner_z_null,
                type = "additive")

tab_contrast_t2_sat_relationship_partner_z_gain <-
  margot_lmtp_evalue(contrast_t2_sat_relationship_partner_z_gain,
                  scale = "RD",
                  new_name = "partner relationship satisfaction")

# first contrast loss
contrast_t2_sat_relationship_partner_z_loss <-
  lmtp_contrast(t2_sat_relationship_partner_z_loss,
                ref = t2_sat_relationship_partner_z_null,
                type = "additive")


# loss
tab_contrast_t2_sat_relationship_partner_z_loss <-
  margot_lmtp_evalue(contrast_t2_sat_relationship_partner_z_loss,
                     scale = "RD",
                     new_name = "partner relationship satisfaction")


contrast_t2_conflict_in_relationship_z_gain <-
  lmtp_contrast(t2_conflict_in_relationship_z_gain,
                ref = t2_conflict_in_relationship_z_null,
                type = "additive")

tab_contrast_t2_conflict_in_relationship_z_gain<-
  margot_lmtp_evalue(contrast_t2_conflict_in_relationship_z_gain,
                     scale = "RD",
                     new_name = "partner perceived conflict")


# loss
contrast_t2_conflict_in_relationship_z_loss <-
  lmtp_contrast(t2_conflict_in_relationship_z_loss,
                ref = t2_conflict_in_relationship_z_null,
                type = "additive")

tab_contrast_t2_conflict_in_relationship_z_loss<-
  margot_lmtp_evalue(contrast_t2_conflict_in_relationship_z_loss,
                     scale = "RD",
                     new_name = "partner perceived conflict")


# gaom
contrast_t2_sexual_satisfaction_z_gain <-
  lmtp_contrast(t2_sexual_satisfaction_z_gain,
                ref = t2_sexual_satisfaction_z_null,
                type = "additive")

tab_contrast_t2_sexual_satisfaction_z_gain <-
  margot_lmtp_evalue(contrast_t2_sexual_satisfaction_z_gain,
                     scale = "RD",
                     new_name = "partner sexual satisfaction")


# loss
contrast_t2_sexual_satisfaction_z_loss <-
  lmtp_contrast(t2_sexual_satisfaction_z_loss,
                ref = t2_sexual_satisfaction_z_null,
                type = "additive")

tab_contrast_t2_sexual_satisfaction_z_loss<-
  margot_lmtp_evalue(contrast_t2_sexual_satisfaction_z_loss,
                     scale = "RD",
                     new_name = "partner sexual satisfaction")


# make tables -------------------------------------------------------------

# bind individual tables
tab_outcomes_gain <- rbind(
  tab_contrast_t2_sat_relationship_partner_z_gain,
  tab_contrast_t2_conflict_in_relationship_z_gain,
  tab_contrast_t2_sexual_satisfaction_z_gain
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
  tab_contrast_t2_sat_relationship_partner_z_loss,
  tab_contrast_t2_conflict_in_relationship_z_loss,
  tab_contrast_t2_sexual_satisfaction_z_loss
)
tab_outcomes_loss
here_save(tab_outcomes_loss,"tab_outcomes_loss")

# make group table
group_tab_outcomes_loss <- group_tab(tab_outcomes_loss , type = "RD")

# save
here_save(group_tab_outcomes_loss, "group_tab_outcomes_loss")


group_tab_outcomes_loss <- here_read("group_tab_outcomes_loss")
group_tab_outcomes_gain <- here_read("group_tab_outcomes_gain")

tab_outcomes_loss <- here_read("tab_outcomes_loss")
tab_outcomes_gain <- here_read("tab_outcomes_gain")

tab_outcomes_gain
tab_outcomes_loss

# create plots -------------------------------------------------------------

# check N
n <- here_read("n_participants")
n
title = "Causal effect of a two-unit shift in perfectionism on PARTNER relationship perceptions"

# graph health
conflicts_prefer(ggplot2::margin)
plot_group_tab_gain <- margot_plot(
  group_tab_outcomes_gain,
  type = "RD",
  title = title,
  subtitle = "Gain of Perfectionism, N = 1860",
  estimate_scale = 1,
  base_size = 18,
  text_size = 5.0,
  point_size = 2,
  title_size = 18,
  subtitle_size = 16,
  legend_text_size = 12,
  legend_title_size = 12,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)
plot_group_tab_gain


# graph body
plot_group_tab_loss <- margot_plot(
  group_tab_outcomes_loss,
  type = "RD",
  title = title,
  subtitle = "LOSS of Perfectionism, N = 1860",
  estimate_scale = 1,
  base_size = 18,
  text_size = 5.0,
  point_size = 2,
  title_size = 18,
  subtitle_size = 16,
  legend_text_size = 12,
  legend_title_size = 12,
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




margot::here_save(sd_sexual_satisfaction, "sd_sexual_satisfaction")
margot::here_save(sd_conflict_in_relationship, "sd_conflict_in_relationship")
margot::here_save(sd_sat_relationship, "sd_sat_relationship")

margot::here_save(mean_sexual_satisfaction, "mean_sexual_satisfaction")
margot::here_save(mean_conflict_in_relationship, "mean_conflict_in_relationship")
margot::here_save(mean_sat_relationship, "mean_sat_relationship")
